# exported functions -------

#'
#' DRAFT: binning raw data into blocks for dual inlet analyses
#'
#' @param dataset A data frame or tibble produced from IsoX data by [orbi_simplify_isox()]
#' @return A data frame (tibble) with block annotations in the form of the additional columns described below:
#' * `block` is an integer counting the data blocks in each file (0 is the startup block)
#' * `sample_name` is the name of the material being measured as defined by the `ref_block_name` and `sample_block_name` parameters
#' * `segment` is an integer defines segments within individual blocks - this will be `NA` until the optional [orbi_segment_blocks()`] is called
#' * `data_group` is an integer that numbers each data group (whether that's startup, a sample block, a segment, etc.) in each file sequentially to uniquely identify groups of data that belong together - this columns is NOT static (i.e. functions like [orbi_adjust_block()] and [orbi_segment_blocks()] will lead to renumbering) and should be used purely for grouping purposes in calculations and visualization
#' * `data_type` is a text value describing the type of data in each `data_group` - for a list of the main categories, call `orbi_get_settings("data_type")`
#' @export
orbi_define_blocks_for_dual_inlet <- function(
    dataset,
    ref_block_time.min,
    change_over_time.min,
    sample_block_time.min = ref_block_time.min,
    startup_time.min = 0,
    ref_block_name = setting("di_ref_name"),
    sample_block_name = setting("di_sample_name")
    ) {

  # parameter checks
  # FIXME: needs implementation
  # check that dataset is provided and the right data kind
  # check that dataset has "filename", "scan.no", "time.min" columns
  # check that ref_block_time.min is provided and a single positive number
  # check that change over time.min is provided and a single positive number
  # check that sample_block_time (if provided) is a single positive number
  # check that startup_time.min is a single >= number
  # what else?

  # get blocks
  blocks <- dataset |>
    find_blocks(
      ref_block_time.min = ref_block_time.min,
      sample_block_time.min = sample_block_time.min,
      startup_time.min = startup_time.min
    ) |>
    # add in the ref and sample names
    dplyr::left_join(
      tibble(
        idx = c(0L, 1L, 2L),
        sample_name = c(ref_block_name, ref_block_name, sample_block_name),
        startup = c(TRUE, FALSE, FALSE)
      ),
      by = "idx"
    ) |>
    # don't really need the max time and idx
    dplyr::select(-"max_time.min", -"idx")

  # get scans
  scans <- dataset |>
    dplyr::select("filename", "scan.no", "time.min") |>
    dplyr::distinct() |>
    # make sure it's in the correct order (for data group identification later)
    dplyr::arrange(.data$filename, .data$scan.no)

  # add block information to scans
  scans_with_blocks <-
    scans |>
    # assign blocks (all time values should be covered)
    dplyr::left_join(blocks, by = "filename", multiple = "all") |>
    dplyr::filter(.data$time.min >= .data$start & .data$time.min < .data$end) |>
    # add additional columns
    dplyr::mutate(
      # identify changeover scans
      changeover = .data$block > 1 & (.data$time.min - .data$start) <= change_over_time.min,
      # segments (none so far)
      segment = NA_real_,
      # data type
      data_type =
        dplyr::case_when(
          # note that there is no unused data ("data_type_unused") yet at this point
          .data$startup ~ setting("data_type_startup"),
          .data$changeover ~ setting("data_type_changeover"),
          TRUE ~ setting("data_type_data")
        ) |>
        # turn into a factor to make it faster to filter on
        as.factor()
    ) |>
    # assign data groups
    group_by(.data$filename) |>
    mutate(
      .grouping = paste(.data$block, .data$segment, .data$data_type) |> factor_in_order() |> as.integer(),
      data_group = cumsum(c(0L, diff(.data$.grouping)) != 0) + 1L
    ) |>
    ungroup()

  # info message
  sprintf(
    "orbi_define_blocks_for_dual_inlet() identified %d data blocks (%s '%s', %s '%s') in data from %d file(s)",
    blocks |> dplyr::filter(.data$block > 0) |> nrow(),
    blocks |> dplyr::filter(.data$block > 0, .data$sample_name == ref_block_name) |> nrow(), ref_block_name,
    blocks |> dplyr::filter(.data$block > 0, .data$sample_name == sample_block_name) |> nrow(), sample_block_name,
    blocks |> select(filename) |> distinct() |> nrow()
  ) |> message()

  # combine with the whole dataset
  dataset_with_blocks <-
    dataset |>
    dplyr::left_join(
      scans_with_blocks |>
        dplyr::select("filename", "scan.no", "block", "sample_name",
                      "segment", "data_group", "data_type"),
      by = c("filename", "scan.no")
    )

  # return new dataset
  return(dataset_with_blocks)
}

#' Manually adjust block delimiters
#'
#' Note that adjusting blocks removes all block segmentation. Make sure to call [orbi_segment_blocks()] **after** adjusting block delimiters.
#' FIXME: complete description and parameters
#'
#' @param dataset tibble produced by [orbi_define_blocks_for_dual_inlet()]
#' @param block the block for which to adjust the start and/or end
#' @param filename needs to be specified only if the `dataset` has more than one `filename`
#' @return A data frame (tibble) with block limits altered according to the provided start/end change parameters. Any data that is no longer part of the original block will be marked with the value of `orbi_get_settings("data_type_unused")`. Any previously applied segmentation will be discarded (`segement` column set to `NA`) to avoid unintended side effects.
#' @export
orbi_adjust_block <- function(
    dataset, block, filename = NULL,
    shift_start_time.min = NULL, shift_end_time.min = NULL,
    shift_start_scan.no = NULL, shift_end_scan.no = NULL,
    set_start_time.min = NULL, set_end_time.min = NULL,
    set_start_scan.no = NULL, set_end_scan.no = NULL
) {

  # provide parameters safety checks
  # dataset as data frame
  # dataset has needed columns
  # FIXME

  # get scans with blocks and data types from the data set
  scans <- dataset |>
    dplyr::select("filename", "scan.no", "time.min", "block":"data_type") |>
    dplyr::distinct()

  # filename check
  if (length(unique(scans$filename)) > 1 && is.null(filename)) {
    stop("dataset has data from more than 1 file - specify the 'filename' parameter for block adjustment", call. = FALSE)
  } else if (!is.null(filename) && !filename %in% scans$filename) {
    stop("provided filename is not in this dataset", call. = FALSE)
  } else if (is.null(filename)) {
    # there's only one filename -- assign it
    filename <- scans$filename[1]
  }

  # get file scan
  file_scans <- scans |>
    dplyr::filter(.data$filename == !!filename) |>
    # make sure it's in the correct order (for data group identification later)
    dplyr::arrange(.data$scan.no)

  # block number check
  if (!block %in% file_scans$block)
    stop("provided block number is not in this dataset", call. = FALSE)

  # start/end definitions safety checks
  start_changes <- sum(!is.null(shift_start_time.min), !is.null(shift_start_scan.no), !is.null(set_start_time.min), !is.null(set_start_scan.no))
  end_changes <- sum(!is.null(shift_end_time.min), !is.null(shift_end_scan.no), !is.null(set_end_time.min), !is.null(set_end_scan.no))
  if (start_changes > 1)
    stop("only provide ONE of the following to change the block start: shift_start_time, shift_start_scan, set_start_time, set_start_scan", call. = FALSE)
  if (end_changes > 1)
    stop("only provide ONE of the following to change the block end: shift_end_time, shift_end_scan, set_end_time, set_end_scan", call. = FALSE)

  # find old start/end
  block_scans <- file_scans |>
    dplyr::filter(
      .data$block == !!block,
      .data$data_type == setting("data_type_data")
    )

  old_start_scan <- block_scans$scan.no |> head(1)
  old_end_scan <- block_scans$scan.no |> tail(1)
  new_start_scan <- NA_integer_
  new_end_scan <- NA_integer_

  old_start_time <- block_scans$time.min |> head(1)
  old_end_time <- block_scans$time.min |> tail(1)
  new_start_time <- NA_real_
  new_end_time <- NA_real_

  # small helpers for scan <--> time interconversion
  find_scan_from_time <- function(time, which = c("first", "last")) {
    time_scans <- file_scans |>
      dplyr::filter(
        (!!which == "first" & .data$time.min >= !!time) |
          (!!which == "last" & .data$time.min < !!time)
      ) |> dplyr::pull(scan.no)
    if (which == "first") return(head(time_scans, 1))
    else return(tail(time_scans, 1))
  }

  # find new start
  if (!is.null(shift_start_time.min)) {
    new_start_time <- old_start_time + shift_start_time.min
    new_start_scan <- find_scan_from_time(new_start_time, "first")
  } else if (!is.null(set_start_time.min)) {
    new_start_time <- set_start_time.min
    new_start_scan <- find_scan_from_time(new_start_time, "first")
  } else if (!is.null(shift_start_scan.no)) {
    new_start_scan <- old_start_scan + shift_start_scan.no
  } else if (!is.null(set_start_scan.no)) {
    new_start_scan <- set_start_scan.no
  } else {
    # no change
    new_start_scan <- old_start_scan
  }

  # find new end
  if (!is.null(shift_end_time.min)) {
    new_end_time <- old_end_time + shift_end_time.min
    new_end_scan <- find_scan_from_time(new_end_time, "last")
  } else if (!is.null(set_end_time.min)) {
    new_end_time <- set_end_time.min
    new_end_scan <- find_scan_from_time(new_end_time, "last")
  } else if (!is.null(shift_end_scan.no)) {
    new_end_scan <- old_end_scan + shift_end_scan.no
  } else if (!is.null(set_end_scan.no)) {
    new_end_scan <- set_end_scan.no
  } else {
    # no change
    new_end_scan <- old_end_scan
  }

  # pull out scan rows (and safety check along the way)
  get_scan_row <- function(scan) {
    scan_row <- file_scans |>
      dplyr::filter(.data$scan.no == !!scan)

    if (nrow(scan_row) == 1L)
      return(scan_row)

    # scan not found --> error
    sprintf("file %s does not contain scan# %s - the scans range from %s to %s",
            filename, scan, min(file_scans$scan.no), max(file_scans$scan.no)) |>
      stop(call. = FALSE)
  }
  old_start_row <- get_scan_row(old_start_scan)
  new_start_row <- get_scan_row(new_start_scan)
  new_end_row <- get_scan_row(new_end_scan)
  new_start_time <- new_start_row$time.min
  new_end_time <- new_end_row$time.min

  # check that the block start/end are valid
  if (new_end_scan <= new_start_scan) {
    sprintf("invalid scan range adjustment requested for block %d in file %s (start: %s end: %s) - block cannot end before it starts",
            block, filename, new_start_scan, new_end_scan) |>
      stop(call. = FALSE)
  }

  # summarize what needs to happen
  change_start <- new_start_scan != old_start_scan
  change_end <- new_end_scan != old_end_scan
  all_blocks <- file_scans$block |> unique()
  removed_blocks <- all_blocks[
    (all_blocks > new_start_row$block & all_blocks < block) |
      (all_blocks < new_end_row$block & all_blocks > block)
    ]

  # any changes?
  if (!change_start && !change_end) {
    sprintf("orbi_adjust_block() is not making any changes to block %d in file %s as no actual changes were requested",
            block, filename) |>
      message()
    return(dataset)
  }

  # info message for changes
  msg <- sprintf("orbi_adjust_block() is making the following block adjustments in file %s:", filename)
  if (change_start) {
    msg <- sprintf("%s\n - moving block %d start from scan %d (%.2f min) to %d (%.2f min)",
                   msg, block, old_start_scan, old_start_time, new_start_scan, new_start_time)
  }
  if (change_end) {
    msg <- sprintf("%s\n - moving block %d end from scan %d (%.2f min) to %d (%.2f min)",
                   msg, block, old_end_scan, old_end_time, new_end_scan, new_end_time)
  }
  if (new_start_row$block < block) {
    msg <- sprintf("%s\n - moving block %d end to the new start of block %d",
                   msg, new_start_row$block, block)
  }
  if (new_end_row$block > block) {
    msg <- sprintf("%s\n - moving block %d start to the new end of block %d",
                   msg, new_end_row$block, block)
  }
  if (length(removed_blocks) > 0) {
    msg <- sprintf("%s\n - removing block %s entirely, as a result of the block adjustments",
                   msg, paste(removed_blocks, collapse = " / "))
  }
  message(msg)

  # actualize changes
  # FIXME: should there be a flag to identify altered block boundaries?
  updated_file_scans <- file_scans |>
    dplyr::mutate(
      segment = NA_real_,
      # update data type
      data_type = dplyr::case_when(
        # new data range
        .data$scan.no >= new_start_scan & .data$scan.no <= new_end_scan ~ setting("data_type_data"),
        # previous data that's now unused
        .data$data_type == setting("data_type_data") &
          .data$scan.no >= old_start_scan & .data$scan.no <= old_end_scan ~ setting("data_type_unused"),
        # unchanged
        TRUE ~ .data$data_type
      ),
      # update block
      block = ifelse(
        .data$scan.no >= new_start_scan & .data$scan.no <= new_end_scan,
        !!block, .data$block
      ),
      # update sample name
      sample_name = ifelse(
        .data$scan.no >= new_start_scan & .data$scan.no <= new_end_scan,
        old_start_row$sample_name, .data$sample_name
      ),
      # new data groups
      .grouping = paste(.data$block, .data$segment, .data$data_type) |> factor_in_order() |> as.integer(),
      data_group = cumsum(c(0L, diff(.data$.grouping)) != 0) + 1L
    )

  # combine with scans from other files
  updated_scans <-
    dplyr::bind_rows(
      updated_file_scans,
      scans |> dplyr::filter(.data$filename != !!filename)
    )

  # combine with the whole dataset
  updated_dataset <-
    dataset |>
    dplyr::select(-c("block":"data_type")) |>
    dplyr::left_join(
      updated_scans |>
        dplyr::select("filename", "scan.no", "block":"data_type"),
      by = c("filename", "scan.no")
    )

  # return new dataset
  return(updated_dataset)
}

orbi_segment_blocks <- function(dataset) {

}

#' Summarize blocks info
#'
#' FIXME: fully document
#'
#' @inheritParams orbi_get_blocks_info
#' @export
orbi_get_blocks_info <- function(dataset) {

  # FIXME: implement safety check on the dataset and needed columns

  # summarize block info
  dataset |>
    dplyr::group_by(.data$filename, .data$data_group, .data$block, .data$sample_name, .data$segment, .data$data_type) |>
    dplyr::summarise(
      start_scan.no = min(.data$scan.no),
      end_scan.no = max(.data$scan.no),
      start_time.min = min(.data$time.min),
      end_time.min = max(.data$time.min),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$filename, .data$start_scan.no) |>
    dplyr::mutate(
      label = ifelse(
        .data$data_type == setting("data_type_data"),
        .data$sample_name,
        as.character(.data$data_type))
    )
}


# internal functions ------------

# helper function to number data gruops (internal)
number_data_groups <- function(dataset) {

  # add safety checks for dataset
  # FIXME: needs to be implemented

}

# helper function to find blocks (internal)
find_blocks <- function(dataset, ref_block_time.min, sample_block_time.min, startup_time.min) {

  # add safety checks for dataset
  # FIXME

  # find blocks
  dataset |>
    dplyr::group_by(.data$filename) |>
    dplyr::summarize(max_time.min = max(.data$time.min)) |>
    dplyr::mutate(
      intervals = purrr::map(.data$max_time.min, ~{
        # do we have a startup block?
        if (startup_time.min > 0) {
          startup_block <- dplyr::tibble(
            interval = 0,
            idx = 0,
            start = 0,
            length = startup_time.min,
            end = startup_time.min
          )
        }
        # all other blocks
        blocks <- find_intervals(
          # only consier for blocks the total time minus the start
          total_time = .x - startup_time.min,
          # just 2 blocks: ref and sample
          intervals = c(ref_block_time.min, sample_block_time.min)
        ) |>
          # bring startup time back into the start/end
          dplyr::mutate(start = .data$start + startup_time.min, end = .data$start + .data$length)

        # return
        dplyr::bind_rows(startup_block, blocks) |>
          # interval for this purpose is called a block
          dplyr::rename(block = "interval")
      })
    ) |>
    tidyr::unnest(.data$intervals)
}

# general helper function to divide up time into intervals (internal)
find_intervals <- function(total_time, intervals) {

  # find how many times the whole sequence of intervals fits inside the total time
  sequence_time <- sum(intervals)
  n_sequences <- total_time %/% sequence_time
  lengths <- rep(intervals, times = n_sequences)
  idx <- rep(seq_along(intervals), times = n_sequences)

  # check the tail for intervals that fit in it
  remaining_intervals <- cumsum(intervals) < total_time - sum(lengths)
  lengths <- c(lengths, intervals[remaining_intervals])
  idx <- c(idx, seq_along(intervals)[remaining_intervals])

  # check for incomplete final interval
  if ( (remainder <- total_time - sum(lengths)) > 0) {
    # add a little extra add the end to make sure that interval includes all data
    lengths <- c(lengths, remainder + 1e-2)
    idx <- c(idx, (tail(idx, 1) %% length(intervals)) + 1L)
  }

  # assemble the totals
  dplyr::tibble(
    interval = seq_along(lengths),
    idx = idx,
    start = c(0, cumsum(head(lengths, -1))),
    length = lengths,
    end = .data$start + .data$length
  )
}

