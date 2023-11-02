# exported functions -------
#' @title Define data block for flow injection
#' @description Define a data block by either start and end time or start and end scan number.
#' If you want to make segments in the blocks (optional), note that this function - manually defining blocks - removes all block segmentation. Make sure to call [orbi_segment_blocks()] **only after** finishing block definitions.
#'
#' @param dataset tibble with Orbitrap data
#' @param start_time.min set the start time of the block
#' @param end_time.min set the end time of the block
#' @param start_scan.no set the start scan of the block
#' @param end_scan.no set the end scan of the block
#' @param sample_name if provided, will be used as the `sample_name` for the block
#' @return A data frame (tibble) with block definition added. Any data that is not part of a block will be marked with the value of `orbi_get_settings("data_type_unused")`. Any previously applied segmentation will be discarded (`segment` column set to `NA`) to avoid unintended side effects.
#' @export
orbi_define_block_for_flow_injection <- function(
    dataset,
    start_time.min = NULL, end_time.min = NULL,
    start_scan.no = NULL, end_scan.no = NULL,
    sample_name = NULL) {

  # type checks
  stopifnot(
    "`dataset` must be a data frame or tibble" =
      !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filename`, `scan.no` and `time.min`" =
      all(c("filename", "scan.no", "time.min") %in% names(dataset)),
    "if set, `start_time.min` must be a single number" =
      is.null(start_time.min) || rlang::is_scalar_double(start_time.min),
    "if set, `end_time.min` must be a single number" =
      is.null(end_time.min) || rlang::is_scalar_double(end_time.min),
    "if set, `start_scan.no` must be a single integer" =
      is.null(start_scan.no) || rlang::is_scalar_integerish(start_scan.no),
    "if set, `end_scan.no` must be a single integer" =
      is.null(end_scan.no) || rlang::is_scalar_integerish(end_scan.no)
  )

  # start/end definitions safety checks
  set_by_time <- !rlang::is_empty(start_time.min) && !rlang::is_empty(end_time.min)
  set_by_scan <- !rlang::is_empty(start_scan.no) && !rlang::is_empty(end_scan.no)
  if (set_by_time && set_by_scan)
    abort("block definition can either be by time or by scan but not both")
  else if (!set_by_time && !set_by_scan)
    abort("block definition requires either `start_time.min` and `end_time.min` or `start_scan.no` and `end_scan.no`")

  # info message
  dataset <-
    dataset |>
    factorize_dataset("filename") |>
    dplyr::mutate(..row_id = dplyr::row_number())
  start_time <-
    sprintf("orbi_define_block_for_flow_injection() is adding new block (%s) to %d files... ",
            if(set_by_time) sprintf("%s to %s min", start_time.min, end_time.min)
            else sprintf("scan %s to %s", start_scan.no, end_scan.no),
            length(levels(dataset$filename))) |>
    message_start()

  # get scans with blocks and data types from the data set
  scans <- dataset |>
    dplyr::select("filename", "scan.no", "time.min", dplyr::any_of(c("data_group", "block", "sample_name", "data_type", "segment"))) |>
    dplyr::distinct()

  # make sure columns exist
  if (!"block" %in% names(scans))
    scans$block <- 0L
  if (!"sample_name" %in% names(scans))
    scans$sample_name <- NA_character_
  if (!"data_type" %in% names(scans))
    scans$data_type <- setting("data_type_unused")

  # nest
  scans <- scans |> tidyr::nest(data = -"filename")

  # find start scans
  if (set_by_time) {
    scans <- scans |>
      dplyr::mutate(
        start_scan.no = map_int(data, find_scan_from_time, start_time.min, "start"),
        end_scan.no = map_int(data, find_scan_from_time, end_time.min, "end")
      )
  } else {
    scans <- scans |>
      dplyr::mutate(
        start_scan.no = !!start_scan.no,
        end_scan.no = !!end_scan.no
      )
  }

  # determine new block numbers
  scans <- scans |>
    dplyr::mutate(
      next_block = map_int(data, ~max(.x$block) + 1L)
    ) |>
    tidyr::unnest("data")

  # actualize changes
  scans <-
    try_catch_all(
      scans |>
        # introduce updated segment, block, data type and sample_name
        dplyr::mutate(
          new_block = ifelse(.data$scan.no >= .data$start_scan.no & .data$scan.no <= .data$end_scan.no, .data$next_block, .data$block),
          sample_name = ifelse(!is.null(!!sample_name) & .data$new_block == .data$next_block, !!sample_name, .data$sample_name),
          data_type = ifelse(.data$new_block == .data$next_block, setting("data_type_data"), .data$data_type),
          segment = NA_integer_
        ) |>
        # determine data groups
        determine_data_groups(),
      "error trying to update scan blocks:"
    )

  # NOTE: should this provide more information and/or allow an argument to make
  # the new plot definition flexible? (snap_to_blocks = TRUE?)
  if (any(scans$block > 0L & scans$new_block > scans$block )) {
    cat("\n")
    abort("new block definition overlaps with existing block")
  }

  # combine with the whole dataset
  updated_dataset <-
    try_catch_all(
      dataset |>
        dplyr::select(-dplyr::any_of(c("data_group", "block", "sample_name", "data_type", "segment"))) |>
        dplyr::left_join(
          scans |>
            dplyr::select(
              "filename", "scan.no", "data_group", "block" = "new_block", "sample_name", "data_type", "segment"
            ),
          by = c("filename", "scan.no")
        ),
      "error trying to update dataset with new block: "
    )

  # info
  message_finish("complete", start_time = start_time)

  # return updated dataset (original row order restored)
  return(updated_dataset |> dplyr::arrange(.data$..row_id) |> dplyr::select(-"..row_id"))
}


#' @title Binning raw data into blocks for dual inlet analyses
#' @description This function sorts out (bins) data into indivual blocks of reference, sample, changeover time, and startup time.
#' @param dataset A data frame or tibble produced from IsoX data by [orbi_simplify_isox()]
#' @param ref_block_time.min time where the signal is stable when reference is analyzed
#' @param change_over_time.min time where the signal is unstable after switching from reference to sample or back
#' @param sample_block_time.min time where the signal is stable when sample is analyzed
#' @param startup_time.min initial time to stabilize spray
#' @param ref_block_name the name of the reference being measured
#' @param sample_block_name the name of the sample being measured
#'
#' @return A data frame (tibble) with block annotations in the form of the additional columns described below:
#' * `data_group` is an integer that numbers each data group (whether that's startup, a sample block, a segment, etc.) in each file sequentially to uniquely identify groups of data that belong together - this columns is NOT static (i.e. functions like [orbi_adjust_block()] and [orbi_segment_blocks()] will lead to renumbering) and should be used purely for grouping purposes in calculations and visualization
#' * `block` is an integer counting the data blocks in each file (0 is the startup block)
#' * `sample_name` is the name of the material being measured as defined by the `ref_block_name` and `sample_block_name` parameters
#' * `segment` is an integer defines segments within individual blocks - this will be `NA` until the optional [orbi_segment_blocks()] is called
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

  # type checks
  stopifnot(
    "`dataset` must be a data frame or tibble" =
      !missing(dataset) && is.data.frame(dataset),
    "`ref_block_time.min` must be a single positive number" =
      !missing(ref_block_time.min) && rlang::is_scalar_double(ref_block_time.min) && ref_block_time.min > 0,
    "`change_over_time.min` must be a single positive number" =
      !missing(change_over_time.min) && rlang::is_scalar_double(change_over_time.min) && change_over_time.min > 0,
    "`sample_block_time.min` must be a single positive number" =
      rlang::is_scalar_double(sample_block_time.min) && sample_block_time.min > 0,
    "`startup_time.min` must be a single number (>= 0)" =
      rlang::is_scalar_double(startup_time.min) && startup_time.min >= 0,
    "`ref_block_name` must be a single string" =
      rlang::is_scalar_character(ref_block_name),
    "`sample_block_name` must be a single string" =
      rlang::is_scalar_character(sample_block_name)
  )

  # dataset columns check
  req_cols <- c("filename", "scan.no", "time.min")
  if (length(missing <- setdiff(req_cols, names(dataset)))) {
    sprintf("`dataset` is missing the column(s) '%s'", paste(missing, collapse = "', '")) |>
      rlang::abort()
  }

  # info message
  start_time <- message_start("orbi_define_blocks_for_dual_inlet() is assessing block structure... ")

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
    dplyr::left_join(blocks, by = "filename", relationship = "many-to-many") |>
    # find right blocks for data
    dplyr::filter(
      .data$time.min >= .data$start &
        ((!.data$last & .data$time.min < .data$end) |
           (.data$last & .data$time.min <= .data$end))
    ) |>
    # add additional columns
    dplyr::mutate(
      # identify changeover scans
      changeover = .data$block > 1 & (.data$time.min - .data$start) <= change_over_time.min,
      # segments (none so far)
      segment = NA_integer_,
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
    determine_data_groups()

  # info message
  sprintf(
    "identified %d blocks (%s '%s', %s '%s') in data from %d file(s)",
    blocks |> dplyr::filter(.data$block > 0) |> nrow(),
    blocks |> dplyr::filter(.data$block > 0, .data$sample_name == ref_block_name) |> nrow(), ref_block_name,
    blocks |> dplyr::filter(.data$block > 0, .data$sample_name == sample_block_name) |> nrow(), sample_block_name,
    blocks |> dplyr::select("filename") |> dplyr::distinct() |> nrow()
  ) |> message_finish(start_time = start_time)

  # combine with the whole dataset
  dataset_with_blocks <-
    dataset |>
    dplyr::left_join(
      scans_with_blocks |>
        dplyr::select(
          "filename", "scan.no",
          "data_group", "block", "sample_name", "data_type", "segment"),
      by = c("filename", "scan.no")
    )

  # return new dataset
  return(dataset_with_blocks)
}

#' @title Manually adjust block delimiters
#' @description This function can be used to manually adjust where certain `block` starts or ends using either time or scan number.
#' Note that adjusting blocks removes all block segmentation. Make sure to call [orbi_segment_blocks()] **after** adjusting block delimiters.
#'
#' @param dataset tibble produced by [orbi_define_blocks_for_dual_inlet()]
#' @param block the block for which to adjust the start and/or end
#' @param filename needs to be specified only if the `dataset` has more than one `filename`
#' @param shift_start_time.min if provided, the start time of the block will be shifted by this many minutes (use negative numbers to shift back)
#' @param shift_end_time.min if provided, the end time of the block will be shifted by this many minutes (use negative numbers to shift back)
#' @param shift_start_scan.no if provided, the start of the block will be shifted by this many scans (use negative numbers to shift back)
#' @param shift_end_scan.no if provided, the end of the block will be shifted by this many scans (use negative numbers to shift back)
#' @param set_start_time.min if provided, sets the start time of the block as close as possible to this time
#' @param set_end_time.min if provided, sets the end time of the block as close as possible to this time
#' @param set_start_scan.no if provided, sets the start of the block to this scan number (scan must exist in the `dataset`)
#' @param set_end_scan.no if provided, sets the end of the block to this scan number (scan must exist in the `dataset`)
#' @return A data frame (tibble) with block limits altered according to the provided start/end change parameters. Any data that is no longer part of the original block will be marked with the value of `orbi_get_settings("data_type_unused")`. Any previously applied segmentation will be discarded (`segment` column set to `NA`) to avoid unintended side effects.
#' @export
orbi_adjust_block <- function(
    dataset, block, filename = NULL,
    shift_start_time.min = NULL, shift_end_time.min = NULL,
    shift_start_scan.no = NULL, shift_end_scan.no = NULL,
    set_start_time.min = NULL, set_end_time.min = NULL,
    set_start_scan.no = NULL, set_end_scan.no = NULL
) {

  # type checks
  stopifnot(
    "`dataset` must be a data frame or tibble" =
      !missing(dataset) && is.data.frame(dataset),
    "`block` must be a single integer" =
      !missing(block) && rlang::is_scalar_integerish(block),
    "if set, `filename` must be a single string" =
      is.null(filename) || rlang::is_scalar_character(filename),
    "if set, `shift_start_time.min` must be a single number" =
      is.null(shift_start_time.min) || rlang::is_scalar_double(shift_start_time.min),
    "if set, `shift_end_time.min` must be a single number" =
      is.null(shift_end_time.min) || rlang::is_scalar_double(shift_end_time.min),
    "if set, `shift_start_scan.no` must be a single integer" =
      is.null(shift_start_scan.no) || rlang::is_scalar_integerish(shift_start_scan.no),
    "if set, `shift_end_scan.no` must be a single integer" =
      is.null(shift_end_scan.no) || rlang::is_scalar_integerish(shift_end_scan.no),
    "if set, `set_start_time.min` must be a single number" =
      is.null(set_start_time.min) || rlang::is_scalar_double(set_start_time.min),
    "if set, `set_end_time.min` must be a single number" =
      is.null(set_end_time.min) || rlang::is_scalar_double(set_end_time.min),
    "if set, `set_start_scan.no` must be a single integer" =
      is.null(set_start_scan.no) || rlang::is_scalar_integerish(set_start_scan.no),
    "if set, `set_end_scan.no` must be a single integer" =
      is.null(set_end_scan.no) || rlang::is_scalar_integerish(set_end_scan.no)
  )
  block <- as.integer(block)
  shift_start_scan.no <- as.integer(shift_start_scan.no)
  shift_end_scan.no <- as.integer(shift_end_scan.no)
  set_start_scan.no <- as.integer(set_start_scan.no)
  set_end_scan.no <- as.integer(set_end_scan.no)

  # dataset columns check
  req_cols <- c("filename", "scan.no", "time.min", "block", "sample_name", "data_type")
  if (length(missing <- setdiff(req_cols, names(dataset)))) {
    sprintf("`dataset` is missing the column(s) '%s'", paste(missing, collapse = "', '")) |>
      rlang::abort()
  }

  # get scans with blocks and data types from the data set
  scans <- dataset |>
    dplyr::select(!!!req_cols, dplyr::any_of("data_group"), dplyr::any_of("segment")) |>
    dplyr::distinct()

  # filename value check
  if (length(unique(scans$filename)) > 1 && is.null(filename)) {
    rlang::abort("`dataset` has data from more than 1 file - specify the `filename` argument for block adjustment")
  } else if (!is.null(filename) && !filename %in% scans$filename) {
    rlang::abort("provided `filename` is not in this `dataset`")
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
    rlang::abort("provided `block` is not in this `dataset`")

  # start/end definitions safety checks
  start_changes <- sum(!rlang::is_empty(shift_start_time.min), !rlang::is_empty(shift_start_scan.no), !rlang::is_empty(set_start_time.min), !rlang::is_empty(set_start_scan.no))
  end_changes <- sum(!rlang::is_empty(shift_end_time.min), !rlang::is_empty(shift_end_scan.no), !rlang::is_empty(set_end_time.min), !rlang::is_empty(set_end_scan.no))
  if (start_changes > 1)
    rlang::abort("only provide ONE of the following to change the block start: `shift_start_time.min`, `shift_start_scan.no`, `set_start_time.min`, `set_start_scan.no`")
  if (end_changes > 1)
    rlang::abort("only provide ONE of the following to change the block end: `shift_end_time.min`, `shift_end_scan.no`, `set_end_time.min`, `set_end_scan.no`")


  # find old start/end
  block_scans <- file_scans |>
    dplyr::filter(
      .data$block == !!block,
      .data$data_type == setting("data_type_data")
    )

  old_start_scan <- block_scans$scan.no |> utils::head(1)
  old_end_scan <- block_scans$scan.no |> utils::tail(1)
  new_start_scan <- NA_integer_
  new_end_scan <- NA_integer_

  old_start_time <- block_scans$time.min |> utils::head(1)
  old_end_time <- block_scans$time.min |> utils::tail(1)
  new_start_time <- NA_real_
  new_end_time <- NA_real_

  # find new start
  if (!rlang::is_empty(shift_start_time.min)) {
    new_start_time <- old_start_time + shift_start_time.min
    new_start_scan <- find_scan_from_time(file_scans, new_start_time, "start")
  } else if (!rlang::is_empty(set_start_time.min)) {
    new_start_time <- set_start_time.min
    new_start_scan <- find_scan_from_time(file_scans, new_start_time, "start")
  } else if (!rlang::is_empty(shift_start_scan.no)) {
    new_start_scan <- old_start_scan + shift_start_scan.no
  } else if (!rlang::is_empty(set_start_scan.no)) {
    new_start_scan <- set_start_scan.no
  } else {
    # no change
    new_start_scan <- old_start_scan
  }

  # find new end
  if (!rlang::is_empty(shift_end_time.min)) {
    new_end_time <- old_end_time + shift_end_time.min
    new_end_scan <- find_scan_from_time(file_scans, new_end_time, "end")
  } else if (!rlang::is_empty(set_end_time.min)) {
    new_end_time <- set_end_time.min
    new_end_scan <- find_scan_from_time(file_scans, new_end_time, "end")
  } else if (!rlang::is_empty(shift_end_scan.no)) {
    new_end_scan <- old_end_scan + shift_end_scan.no
  } else if (!rlang::is_empty(set_end_scan.no)) {
    new_end_scan <- set_end_scan.no
  } else {
    # no change
    new_end_scan <- old_end_scan
  }

  old_start_row <- get_scan_row(file_scans, old_start_scan)
  new_start_row <- get_scan_row(file_scans, new_start_scan)
  new_end_row <- get_scan_row(file_scans, new_end_scan)
  new_start_time <- new_start_row$time.min
  new_end_time <- new_end_row$time.min

  # check that the block start/end are valid
  if (new_end_scan <= new_start_scan) {
    sprintf("invalid scan range adjustment requested for block %d in file %s (start: %s end: %s) - block cannot end before it starts",
            block, filename, new_start_scan, new_end_scan) |>
      rlang::abort()
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
      message_standalone()
    return(dataset)
  }

  # info message for changes
  sprintf("orbi_adjust_block() is making the following block adjustments in file %s:", filename) |>
    message_standalone()

  if (change_start) {
    sprintf(" - moving block %d start from scan %d (%.2f min) to %d (%.2f min)",
            block, old_start_scan, old_start_time, new_start_scan, new_start_time) |>
      message_standalone()
  }
  if (change_end) {
    sprintf(" - moving block %d end from scan %d (%.2f min) to %d (%.2f min)",
            block, old_end_scan, old_end_time, new_end_scan, new_end_time) |>
      message_standalone()
  }
  if (new_start_row$block < block) {
    sprintf(" - moving block %d end to the new start of block %d",
            new_start_row$block, block) |>
      message_standalone()
  }
  if (new_end_row$block > block) {
    sprintf(" - moving block %d start to the new end of block %d",
            new_end_row$block, block) |>
      message_standalone()
  }
  if (length(removed_blocks) > 0) {
    sprintf(" - removing block %s entirely, as a result of the block adjustments",
            paste(removed_blocks, collapse = " / ")) |>
      message_standalone()
  }

  # actualize changes
  # FIXME: should there be a flag to identify altered block boundaries?
  updated_file_scans <- file_scans |>
    dplyr::mutate(
      # update segment
      segment = NA_integer_,
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
      )
    ) |>
    # determine data groups
    determine_data_groups()

  # combine with scans from other files
  updated_scans <-
    dplyr::bind_rows(
      updated_file_scans,
      scans |> dplyr::filter(.data$filename != !!filename)
    )

  # combine with the whole dataset
  updated_dataset <-
    dataset |>
    dplyr::select(-"block", -"sample_name", -"data_type", -dplyr::any_of("data_group"), -dplyr::any_of("segment")) |>
    dplyr::left_join(
      updated_scans |>
        dplyr::select(
          "filename", "scan.no",
          "data_group", "block", "sample_name", "data_type", "segment"
        ),
      by = c("filename", "scan.no")
    )

  # return new dataset
  return(updated_dataset)
}

#' @title Segment data blocks
#' @description This step is optional and is intended to make it easy to explore the data within a sample or ref data block. Note that any raw data not identified with `data_type` set to "data" (`orbi_get_settings("data_type")`) will stay unsegmented. This includes raw data flagged as "startup", "changeover", and "unused".
#' @inheritParams orbi_adjust_block
#' @param into_segments segment each data block into this many segments. The result will have exactly this number of segments for each data block except for if there are more segments requested than observations in a group (in which case each observation will be one segment)
#' @param by_scans segment each data block into segments spanning this number of scans. The result will be approximately the requested number of scans per segment, depending on what is the most sensible distribution of the data. For example, in a hypothetical data block with 31 scans, if by_scans = 10, this function will create 3 segments with 11, 10 and 10 scans each (most evenly distributed), instead of 4 segments with 10, 10, 10, 1 (less evenly distributed).
#' @param by_time_interval segment each data block into segments spanning this time interval. The result will have the requested time interval for all segments except usually the last one which is almost always shorter than the requested interval.
#' @export
orbi_segment_blocks <- function(dataset, into_segments = NULL, by_scans = NULL, by_time_interval = NULL) {

  # type checks
  stopifnot(
    "`dataset` must be a data frame or tibble" =
      !missing(dataset) && is.data.frame(dataset),
    "if set, `into_segments` must be a single positive integer" =
      is.null(into_segments) || (rlang::is_scalar_integerish(into_segments) && into_segments > 0L),
    "if set, `by_scans` must be a single positive integer" =
      is.null(by_scans) || (rlang::is_scalar_integerish(by_scans) && by_scans > 0L),
    "if set, `by_time_interval` must be a single positive number" =
      is.null(by_time_interval) || (rlang::is_scalar_double(by_time_interval) && by_time_interval > 0)
  )
  into_segments <- as.integer(into_segments)
  by_scans <- as.integer(by_scans)

  # dataset columns check
  req_cols <- c("filename", "scan.no", "time.min", "block", "sample_name", "data_type")
  if (length(missing <- setdiff(req_cols, names(dataset)))) {
    sprintf("`dataset` is missing the column(s) '%s'", paste(missing, collapse = "', '")) |>
      rlang::abort()
  }

  # provide exactly one argument on how to segment
  set_args <- sum(!rlang::is_empty(into_segments), !rlang::is_empty(by_time_interval), !rlang::is_empty(by_scans))
  if (set_args == 0)
    rlang::abort("set one of the 3 ways to segment: `into_segments`, `by_time_interval`, `by_scans`")
  else if (set_args > 1)
    rlang::abort("only set ONE of the 3 ways to segment: `into_segments`, `by_time_interval`, `by_scans`")

  # info
  dataset <- dataset |> factorize_dataset("filename")
  start_time <-
    sprintf(
      "orbi_segment_blocks() is segmenting %d data blocks in %d file(s)... ",
      dataset |> dplyr::filter(.data$block > 0) |>
        dplyr::select("filename", "block") |> dplyr::distinct() |> nrow(),
      length(levels(dataset$filename))
    ) |>
    message_start()

  # get scans
  scans <- dataset |>
    dplyr::select(!!!req_cols) |>
    dplyr::distinct() |>
    # make sure it's in the correct order (for data group identification later)
    dplyr::arrange(.data$filename, .data$scan.no)

  # calculate segments
  segmented_scans <-
    scans |>
    # segment
    dplyr::group_by(.data$filename, .data$block, .data$data_type) |>
    dplyr::mutate(
      segment =
        if(.data$data_type[1] == setting("data_type_data")) {
          # data type is 'data'
          if (!rlang::is_empty(!!into_segments)) {
            segment_by_segs(.data$scan.no, !!into_segments)
          } else if (!rlang::is_empty(!!by_scans)) {
            segment_by_scans(.data$scan.no, !!by_scans)
          } else {
            segment_by_time_interval(.data$time.min, !!by_time_interval)
          }
        } else {
          NA_integer_
        }
    ) |>
    dplyr::ungroup() |>
    # determine data groups
    determine_data_groups()

  # info message
  info_sum <- segmented_scans |>
    dplyr::filter(.data$block > 0, .data$data_type == setting("data_type_data")) |>
    dplyr::select("filename", "block", "segment", "scan.no") |>
    dplyr::distinct() |>
    dplyr::count(.data$filename, .data$block, .data$segment) |>
    dplyr::group_by(.data$filename, .data$block) |>
    dplyr::summarise(
      n_segments = n(),
      n_scans_avg = mean(n),
      .groups = "drop"
    )
  sprintf(
    "created %s segments / block (on average) with %s scans / segment (on average)",
    info_sum$n_segments |> mean() |> signif(2),
    info_sum$n_scans_avg |> mean() |> signif(2)
  ) |> message_finish(start_time = start_time)

  # combine with the whole dataset
  updated_dataset <-
    dataset |>
    dplyr::select(-"block", -"sample_name", -"data_type", -dplyr::any_of("data_group"), -dplyr::any_of("segment")) |>
    dplyr::left_join(
      segmented_scans |>
        dplyr::select(
          "filename", "scan.no",
          "data_group", "block", "sample_name", "data_type", "segment"
        ),
      by = c("filename", "scan.no")
    )

  # return new dataset
  return(updated_dataset)
}

#' @title Summarize blocks info
#' @description This function provides an overview table `blocks_info` which shows information on blocks in the dataset (block number, sample name, data type, scan number and start time where a block starts, and scan number and end time where a block ends).
#' @inheritParams orbi_adjust_block
#' @param .by grouping columns for block info (akin to dplyr's `.by` parameter e.g. in [dplyr::summarize()]). If not set by the user, all columns in the parameter's default values are used, if present in the dataset.
#' @return a block summary or if no blocks defined yet, an empty tibble (with warning)
#' @export
orbi_get_blocks_info <- function(dataset, .by = c("filename", "injection", "data_group", "block", "sample_name", "data_type", "segment")) {

  # type checks
  stopifnot(
    "`dataset` must be a data frame or tibble" =
      !missing(dataset) && is.data.frame(dataset),
    "`dataset` requires columns `filename`, `scan.no` or `start/end_scan.no`, `time.min` or `start/end_time.min`" =
      "filename" %in% names(dataset) &&
      (all(c("scan.no", "time.min") %in% names(dataset)) ||
         all(c("start_scan.no", "end_scan.no", "start_time.min", "end_time.min") %in% names(dataset)))
  )

  # empty blocks
  empty <- dataset |>
    dplyr::group_by(.data$filename) |>
    dplyr::summarise(
      data_group = NA_integer_,
      block = NA_integer_,
      sample_name = NA_character_,
      data_type = factor(NA_character_),
      segment = NA_integer_,
      start_scan.no = NA_integer_,
      end_scan.no = NA_integer_,
      start_time.min = NA_real_,
      end_time.min = NA_real_
    )

  # no information
  if (nrow(dataset) == 0) return(empty)
  if (!"block" %in% names(dataset)) {
    rlang::warn("`dataset` does not seem to have any block definitions yet (`block` column missing)")
    return(empty)
  }

  # summarize block info
  if ("start_scan.no" %in% names(dataset)) {
    # already summarized
    dataset |>
      dplyr::select(dplyr::any_of(.by), "start_scan.no", "end_scan.no", "start_time.min", "end_time.min")
  } else {
    dataset |>
      group_if_exists(.by) |>
      dplyr::summarise(
        start_scan.no = min(.data$scan.no),
        end_scan.no = max(.data$scan.no),
        start_time.min = min(.data$time.min),
        end_time.min = max(.data$time.min),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$filename, .data$start_scan.no)
  }
}

#' @title Plot blocks background
#' @description This function can be used to add colored background to a plot of dual-inlet data where different colors signify different data types (data, startup time, changeover time, unused). Note that this function only works with continuous and pseudo-log y axis, not with log y axes.
#'
#' FIXME: this should also work with scan number
#'
#' @param plot object with a dataset that has defined blocks
#' @param x which x-axis to use (time vs. scan number). If set to "guess" (the default), the function will try to figure it out from the plot.
#' @param data_only if set to TRUE, only the blocks flagged as "data" (`setting("data_type_data")`) are highlighted
#' @param fill what to use for the fill aesthetic, default is the block `data_type`
#' @param fill_colors which colors to use, by default a color-blind friendly color palettes (RColorBrewer, dark2)
#' @param fill_scale use this parameter to replace the entire fill scale rather than just the `fill_colors`
#' @param alpha opacity settings for the background
#' @param show.legend whether to include the background information in the legend
#' @export
orbi_add_blocks_to_plot <- function(
    plot,
    x = c("guess", "scan.no", "time.min"), 
    data_only = FALSE,
    fill = .data$data_type,
    fill_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666"),
    fill_scale = scale_fill_manual(values = fill_colors),
    alpha = 0.5, show.legend = !data_only) {

  # safety checks
  stopifnot(
    "`plot` has to be a ggplot" = !missing(plot) && is(plot, "ggplot")
  )
  x_column <- arg_match(x)
  
  # find out if it's a scan.no or time.min based plot if x is "guess"
  if (x_column == "guess") {
    x_column <- p$mapping$x |> as_label()
    if (!x_column %in% c("scan.no", "time.min")) {
      sprintf("cannot guess x-axis for blocks from plot as its aes(x = ) variable is neither 'scan.no' nor 'time.min'") |>
        warn()
      return(plot)
    }
  }
  
  # add the rectangle plot as underlying layer
  plot$layers <- c(
    ggplot2::geom_rect(
      data = function(df) {
        blocks <- df |> orbi_get_blocks_info()
        if (!all(c("block", "sample_name", "data_type") %in% names(blocks))) {
          abort("columns `block`, `sample_name`, and `data_type` required for showing block")
        }
        if (data_only) {
          blocks <- blocks |> dplyr::filter(.data$data_type == setting("data_type_data"))
        }
        blocks |> 
          dplyr::filter(!is.na(.data$block)) |>
          dplyr::mutate(
            xmin = if(!!x_column == "time.min") .data$start_time.min else .data$start_scan.no,
            xmax = if(!!x_column == "time.min") .data$end_time.min else .data$end_scan.no
          )
      },
      map = ggplot2::aes(
        x = NULL, xmin = .data$xmin, xmax = .data$xmax,
        y = NULL, color = NULL,  ymin = -Inf, ymax = Inf,
        fill = {{ fill }}
      ),
      alpha = alpha, linetype = 0, color = NA_character_,
      show.legend = show.legend
    ),
    plot$layers
  )

  # return plot
  plot <- plot + {{ fill_scale }} +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(fill = NA_character_))) +
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(fill = NA_character_)))
  return(plot)
}

# internal functions ------------

# helper function to find blocks (internal)
#' @param dataset tibble produced by [orbi_define_blocks_for_dual_inlet()]
#' @param ref_block_time.min time where the signal is stable when reference is analyzed
#' @param sample_block_time.min time where the signal is stable when sample is analyzed
#' @param startup_time.min initial time to stabilize spray
find_blocks <- function(dataset, ref_block_time.min, sample_block_time.min = ref_block_time.min, startup_time.min = 0) {

  # type checks
  stopifnot(
    "`dataset` must be a data frame or tibble" =
      !missing(dataset) && is.data.frame(dataset),
    "`ref_block_time.min` must be a single positive number" =
      !missing(ref_block_time.min) && rlang::is_scalar_double(ref_block_time.min) && ref_block_time.min > 0,
    "`sample_block_time.min` must be a single positive number" =
      rlang::is_scalar_double(sample_block_time.min) && sample_block_time.min > 0,
    "`startup_time.min` must be a single number (>= 0)" =
      rlang::is_scalar_double(startup_time.min) && startup_time.min >= 0
  )

  # dataset columns check
  req_cols <- c("filename", "time.min")
  if (length(missing <- setdiff(req_cols, names(dataset)))) {
    sprintf("`dataset` is missing the column(s) '%s'", paste(missing, collapse = "', '")) |>
      rlang::abort()
  }

  # find blocks
  find_file_blocks <- function(tmin, tmax) {
    # non-startup blocks
    blocks <- find_intervals(
      # only consider the total time without the startup
      total_time = tmax - startup_time.min,
      # just 2 blocks: ref and sample
      intervals = c(ref_block_time.min, sample_block_time.min)
    ) |>
      # bring tmin and startup time back into the start/end
      dplyr::mutate(
        start = .data$start + startup_time.min,
        last = dplyr::n() == dplyr::row_number()
      )

    # do we have a startup block?
    if (startup_time.min > 0) {
      startup_block <- dplyr::tibble(
        interval = 0L, idx = 0, start = 0,
        length = startup_time.min, last = FALSE
      )
      blocks <- dplyr::bind_rows(startup_block, blocks) |>
        dplyr::relocate("last", .after = dplyr::last_col())
    }

    # return
    blocks |>
      # recalculate end for all blocks
      dplyr::mutate(end = .data$start + .data$length) |>
      # interval for this purpose is called a block
      dplyr::rename(block = "interval") |>
      # exclude blocks that cannot possibily be in the file
      dplyr::filter(.data$end > tmin)
  }

  # find blocks by filename
  dataset |>
    dplyr::group_by(.data$filename) |>
    dplyr::summarize(
      min_time.min = min(.data$time.min),
      max_time.min = max(.data$time.min),
      intervals = list(find_file_blocks(.data$min_time.min, .data$max_time.min))
    ) |>
    tidyr::unnest("intervals")
}

# general helper function to divide up time into intervals (internal)
#' @param total_time which time should be divided up into intervals
#' @param intervals how many intervals
find_intervals <- function(total_time, intervals) {

  # safety checks
  stopifnot(
    "`total_time` must a single number" = !missing(total_time) && rlang::is_scalar_double(total_time),
    "`intervals` must be one or more numbers" = !missing(intervals) && is.numeric(intervals) && length(intervals) >= 1L
  )

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
  if (rlang::is_empty(lengths)) {
    # no interval fits in
    lengths <- total_time
    idx <- 1L
  } else if ( (remainder <- total_time - sum(lengths)) > 0) {
    # some residual in the last interval
    lengths <- c(lengths, remainder)
    idx <- c(idx, (utils::tail(idx, 1) %% length(intervals)) + 1L)
  }

  # assemble the totals
  dplyr::tibble(
    interval = seq_along(lengths),
    idx = idx,
    start = c(0, cumsum(utils::head(lengths, -1))),
    length = lengths,
    end = .data$start + .data$length
  )
}

# helper to find scan number from time
#' @param scans must be scans from a single file
#' @param time time stamp where the scan number is required
#' @param which searching towards the start or end of the file
find_scan_from_time <- function(scans, time, which = c("start", "end")) {
  time_scans <- scans |>
    dplyr::filter(
      (!!which == "start" & .data$time.min >= !!time) |
        (!!which == "end" & .data$time.min < !!time)
    ) |> dplyr::pull(.data$scan.no)
  time_scan <-
    if (which == "start") utils::head(time_scans, 1)
  else utils::tail(time_scans, 1)

  # safety check
  if (rlang::is_empty(time_scan)) {
    sprintf("invalid %s time (%s minutes) for file '%s' - the time ranges from %s to %s minutes",
            which, signif(time), scans$filename[1],
            signif(min(scans$time.min)),
            signif(max(scans$time.min))) |>
      rlang::abort()
  }

  # return
  return(time_scan)
}

# pull out scan rows (and safety check along the way)
#' @param scans must be scans from a single file
#' @param scan scan number
get_scan_row <- function(scans, scan) {
  if (!rlang::is_empty(scan)) {
    scan_row <- scans |>
      dplyr::filter(.data$scan.no == !!scan)

    if (nrow(scan_row) == 1L) {
      return(scan_row)
    } else if (nrow(scan_row) > 1L) {
      sprintf("scan# %s not unique! should never get to this with only one file! %s",
              scan, paste(unique(scans$filename), collapse = ", ")) |>
        rlang::abort()
    }
  }
  # scan not found --> error
  sprintf("file '%s' does not contain scan# %s - the scans range from %s to %s",
          scans$filename[1], scan, min(scans$scan.no), max(scans$scan.no)) |>
    rlang::abort()
}

# internal function to determine data groups
#' @param dataset assumes is in the correct order
determine_data_groups <- function(dataset) {
  dataset |>
    # assign data groups
    dplyr::group_by(.data$filename) |>
    dplyr::mutate(
      .grouping = paste(.data$block, .data$segment, .data$data_type) |> factor_in_order() |> as.integer(),
      data_group = cumsum(c(0L, diff(.data$.grouping)) != 0) + 1L
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".grouping")
}

# internal function to segment into a specific number of segments
#' @param scan.no where the segmentation starts
#' @param into_segments how many segments
segment_by_segs <- function(scan.no, into_segments) {
  idx <- seq_along(scan.no)
  if(into_segments >= length(scan.no)) {
    # more segments requested than total data points
    # --> one segment per data point
    return(idx)
  } else {
    divider <- length(scan.no) / into_segments
    out <- as.integer((idx - 1L) %/% divider + 1L)
    return(out)
  }
}

# internal function to segment by scans
#' @param scan.no where the segmentation starts
#' @param by_scans number of scans in each segment
segment_by_scans <- function(scan.no, by_scans) {
  # approximates number of scans
  return(segment_by_segs(scan.no, into_segments = round(length(scan.no) / by_scans)))
}

# internal function to segment by time interval
#' @param time.min where the segmentation starts
#' @param time_interval length of each segment in minutes
segment_by_time_interval <- function(time.min, time_interval) {
  return( as.integer((time.min - min(time.min)) %/% time_interval + 1L))
}
