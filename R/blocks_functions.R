# exported functions -------

#' @title Define data blocks
#' @description Define one or more data blocks by either start and end time or start and end scan number.
#' The blocks can be provided as vectors in the individual parameters or as a `blocks_table` - whichever is more convenient.
#' If you want to make segments in the blocks (optional), note that this function - manually defining blocks - removes all block segmentation. Make sure to call [orbi_segment_blocks()] **only after** finishing block definitions.
#'
#' @inheritParams orbi_flag_satellite_peaks
#' @param start_time.min start time of the block(s), a single value or a vector for multiple blocks
#' @param end_time.min end time of the block(s), a single value or a vector for multiple blocks. Use `Inf` for a block that lasts until the end of each file.
#' @param start_scan.no start scan of the block(s), a single value or a vector for multiple blocks
#' @param end_scan.no end scan of the block(s), a single value or a vector for multiple blocks. Use `Inf` for a block that lasts until the last scan of each file.
#' @param block_name name(s) for the block(s), a single value or a vector for multiple blocks, `NA` by default (i.e. unnamed)
#' @param in_filename the file(s) to add the block(s) to, a single value or a vector for multiple blocks, `NA` (the default if not provided) adds a block to all files. This must be the `filename` of the file(s) as it appears in the `dataset`.
#' @param blocks_table alternative to the individual parameters: a data frame with any of the columns `start_time.min`, `end_time.min`, `start_scan.no`, `end_scan.no`, `block_name` and `in_filename`, one row per block. Any other columns are ignored. If provided, the individual parameters are not used.
#' @details Each block has to be defined either by time (`start_time.min` **and** `end_time.min`) or by scan number (`start_scan.no` **and** `end_scan.no`) but not by both. Different blocks in the same call can use different definitions.
#'
#' By default, each block is added to all files in the `dataset`. To add a block only to a specific file, provide its `in_filename`. Since all parameters are recycled, the same block definition can be added to several (but not all) files by providing a vector of filenames, e.g. `in_filename = c("file1", "file2")` defines the block once for each of these two files.
#'
#' Blocks are matched against the scans of each file separately. A block whose range reaches beyond what a file recorded is trimmed to the scans that exist in that file, and a block that does not overlap a file at all is not added there - this is reported with a warning naming the affected files and the range each of them covers. The summary message lists the scans and times each block actually ended up covering, which is what to check if a block was trimmed.
#' @return A data frame (tibble) with the block definitions added. Any data that is not part of a block will be marked with the value of `orbi_get_option("data_type_unused")`. Any previously applied segmentation will be discarded (`segment` column set to `NA`) to avoid unintended side effects.
#' @examples
#' fpath <- system.file("extdata", "testfile_flow.isox", package = "isoorbi")
#' df <- orbi_read_isox(file = fpath) |> orbi_simplify_isox()
#'
#' # a single block
#' df |> orbi_define_blocks(start_time.min = 0.2, end_time.min = 0.8)
#'
#' # several blocks at once
#' df |> orbi_define_blocks(
#'   start_time.min = c(0.1, 0.5),
#'   end_time.min = c(0.4, 0.8),
#'   block_name = c("first", "second")
#' )
#'
#' # the same via a blocks table, here mixing time and scan definitions
#' df |> orbi_define_blocks(
#'   blocks_table = tibble::tibble(
#'     start_time.min = c(0.1, NA),
#'     end_time.min = c(0.4, NA),
#'     start_scan.no = c(NA, 200),
#'     end_scan.no = c(NA, 350),
#'     block_name = c("first", "second")
#'   )
#' )
#'
#' # a block that is only added to a specific file
#' df |> orbi_define_blocks(
#'   start_time.min = 0.2,
#'   end_time.min = 0.8,
#'   in_filename = "ac5"
#' )
#' @export
orbi_define_blocks <- function(
  dataset,
  start_time.min = NULL,
  end_time.min = NULL,
  start_scan.no = NULL,
  end_scan.no = NULL,
  block_name = NA_character_,
  in_filename = NULL,
  blocks_table = NULL
) {
  # safety checks
  check_dataset_arg(dataset)

  # assemble + validate the block definitions
  blocks_table <- get_blocks_table(
    blocks_table = blocks_table,
    start_time.min = start_time.min,
    end_time.min = end_time.min,
    start_scan.no = start_scan.no,
    end_scan.no = end_scan.no,
    block_name = block_name,
    in_filename = in_filename
  )

  # blocks for specific files need those files to be in the dataset
  # note: a dataset without filenames at all is caught when adding the blocks
  is_agg <- is(dataset, "orbi_aggregated_data")
  filenames <- if (is_agg) dataset$file_info$filename else dataset[["filename"]]
  filenames <- unique(as.character(filenames))
  unknown <- setdiff(
    stats::na.omit(blocks_table$in_filename),
    filenames
  )
  if (length(filenames) > 0L && length(unknown) > 0L) {
    cli_abort(
      c(
        "{.field in_filename} {.val {unknown}} {?is/are} not in this {.field dataset}",
        "i" = "available filename{?s}: {.val {filenames}}"
      )
    )
  }

  # how many files are we working with?
  scans <- if (is_agg) dataset$scans else dataset
  n_files <- if ("uidx" %in% names(scans)) {
    length(unique(scans$uidx))
  } else if ("filename" %in% names(scans)) {
    length(unique(scans$filename))
  } else {
    1L
  }

  # info
  n_blocks <- nrow(blocks_table)
  start <- start_info(
    "is adding {n_blocks} block{?s} to {n_files} file{?s}"
  )

  # add the blocks one at a time so that each one gets the next block number
  # and is checked against the ones that already exist
  root_env <- current_env()
  coverage <- vector("list", n_blocks)
  for (i in seq_len(n_blocks)) {
    out <- add_single_block(dataset, blocks_table[i, ], .env = root_env)
    dataset <- out$dataset
    coverage[[i]] <- out$coverage
  }

  # summary
  # note: a block that fell outside the data everywhere was not added at all
  # (it warned about it), so don't count it here
  n_added <- coverage |> purrr::map_lgl(~ any(.x$n_scans > 0L)) |> sum()
  # the files that actually got a block (not all do if blocks are for specific
  # files or outside the data of some files)
  n_files_added <- coverage |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$n_scans > 0L) |>
    dplyr::distinct(dplyr::across(dplyr::any_of(c("uidx", "filename")))) |>
    nrow()
  if (n_added < n_blocks) {
    finish_info(
      "added {n_added} of {n_blocks} block{?s} to {n_files_added} file{?s}",
      start = start
    )
  } else {
    finish_info(
      "added {n_blocks} block{?s} to {n_files_added} file{?s}",
      start = start
    )
  }
  seq_len(n_blocks) |>
    purrr::map_chr(\(i) {
      desc <- describe_block_coverage(blocks_table[i, ], coverage[[i]])
      paste0("\u00a0", format_inline("{symbol$arrow_right} {desc}"))
    }) |>
    cli_bullets_raw() |>
    cli()

  # return updated dataset
  return(dataset)
}

#' @title Define data block for flow injection
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `orbi_define_block_for_flow_injection()` was renamed [orbi_define_blocks()] since it is not specific to flow injection
#' and can now define several blocks at once.
#' @inheritParams orbi_define_blocks
#' @param sample_name `r lifecycle::badge("deprecated")` renamed to `block_name` since the column it sets names the block rather than necessarily a sample
#' @return see [orbi_define_blocks()]
#' @export
orbi_define_block_for_flow_injection <- function(
  dataset,
  start_time.min = NULL,
  end_time.min = NULL,
  start_scan.no = NULL,
  end_scan.no = NULL,
  block_name = NA_character_,
  sample_name = lifecycle::deprecated()
) {
  # note: always = TRUE so this warns on every call rather than once every 8 hours
  lifecycle::deprecate_warn(
    "1.6.0",
    "orbi_define_block_for_flow_injection()",
    "orbi_define_blocks()",
    details = "The new function is more versatile and can define multiple blocks at once.",
    always = TRUE
  )
  if (lifecycle::is_present(sample_name)) {
    lifecycle::deprecate_warn(
      "1.6.0",
      "orbi_define_block_for_flow_injection(sample_name = )",
      "orbi_define_blocks(block_name = )",
      details = "The `sample_name` column created by the block definitions was renamed to `block_name`.",
      always = TRUE
    )
    block_name <- sample_name
  }
  return(orbi_define_blocks(
    dataset = dataset,
    start_time.min = start_time.min,
    end_time.min = end_time.min,
    start_scan.no = start_scan.no,
    end_scan.no = end_scan.no,
    block_name = block_name
  ))
}

#' @title Binning raw data into blocks for dual inlet analyses
#' @description This function sorts out (bins) data into indivual blocks of reference, sample, changeover time, and startup time.
#' @inheritParams orbi_flag_satellite_peaks
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
#' * `block_name` is the name of the material being measured as defined by the `ref_block_name` and `sample_block_name` parameters
#' * `segment` is an integer defines segments within individual blocks - this will be `NA` until the optional [orbi_segment_blocks()] is called
#' * `data_type` is a text value describing the type of data in each `data_group` - for a list of the main categories, call `orbi_get_options("data_type")`
#' @export
orbi_define_blocks_for_dual_inlet <- function(
  dataset,
  ref_block_time.min,
  change_over_time.min,
  sample_block_time.min = ref_block_time.min,
  startup_time.min = 0,
  ref_block_name = orbi_get_option("di_ref_name"),
  sample_block_name = orbi_get_option("di_sample_name")
) {
  # safety checks
  check_dataset_arg(dataset)
  stopifnot(
    "`ref_block_time.min` must be a single positive number" = !missing(
      ref_block_time.min
    ) &&
      rlang::is_scalar_double(ref_block_time.min) &&
      ref_block_time.min > 0,
    "`change_over_time.min` must be a single positive number" = !missing(
      change_over_time.min
    ) &&
      rlang::is_scalar_double(change_over_time.min) &&
      change_over_time.min > 0,
    "`sample_block_time.min` must be a single positive number" = rlang::is_scalar_double(
      sample_block_time.min
    ) &&
      sample_block_time.min > 0,
    "`startup_time.min` must be a single number (>= 0)" = rlang::is_scalar_double(
      startup_time.min
    ) &&
      startup_time.min >= 0,
    "`ref_block_name` must be a single string" = rlang::is_scalar_character(
      ref_block_name
    ),
    "`sample_block_name` must be a single string" = rlang::is_scalar_character(
      sample_block_name
    )
  )

  # get scans
  is_agg <- is(dataset, "orbi_aggregated_data")
  scans <- if (is_agg) {
    dataset$scans |>
      dplyr::left_join(
        dataset$file_info |> dplyr::select("uidx", "filename"),
        by = "uidx"
      )
  } else {
    dataset
  }

  # check required columns
  check_tibble(
    scans,
    req_cols = c("filename", "scan.no", "time.min"),
    .arg = "dataset"
  )

  by_cols <- "filename"
  if ("uidx" %in% names(scans)) {
    by_cols <- c("uidx", by_cols)
  }

  # info message
  start <- start_info("is running")

  # get single scans (for compatibility with tibble, technically redundant for aggregated data)
  single_scans <- scans |>
    dplyr::select(dplyr::all_of(by_cols), "scan.no", "time.min") |>
    dplyr::distinct() |>
    # make sure it's in the correct order (for data group identification later)
    dplyr::arrange(.data$filename, .data$scan.no)

  # get blocks
  blocks <- single_scans |>
    find_blocks(
      ref_block_time.min = ref_block_time.min,
      sample_block_time.min = sample_block_time.min,
      startup_time.min = startup_time.min
    ) |>
    # add in the ref and sample names
    dplyr::left_join(
      tibble(
        idx = c(0L, 1L, 2L),
        block_name = c(ref_block_name, ref_block_name, sample_block_name),
        startup = c(TRUE, FALSE, FALSE)
      ),
      by = "idx"
    ) |>
    # don't really need the max time and idx
    dplyr::select(-"max_time.min", -"idx")

  # add block information to scans
  single_scans_with_blocks <-
    single_scans |>
    # assign blocks (all time values should be covered)
    dplyr::left_join(blocks, by = by_cols, relationship = "many-to-many") |>
    # find right blocks for data
    dplyr::filter(
      .data$time.min >= .data$start &
        ((!.data$last & .data$time.min < .data$end) |
          (.data$last & .data$time.min <= .data$end))
    ) |>
    # add additional columns
    dplyr::mutate(
      # identify changeover scans
      changeover = .data$block > 1 &
        (.data$time.min - .data$start) <= change_over_time.min,
      # segments (none so far)
      segment = NA_integer_,
      # data type
      data_type = dplyr::case_when(
        # note that there is no unused data ("data_type_unused") yet at this point
        .data$startup ~ orbi_get_option("data_type_startup"),
        .data$changeover ~ orbi_get_option("data_type_changeover"),
        TRUE ~ orbi_get_option("data_type_data")
      ) |>
        # turn into a factor to make it faster to filter on
        as.factor()
    ) |>
    # assign data groups
    determine_data_groups()

  # info message
  finish_info(
    "identified {blocks |> dplyr::filter(.data$block > 0) |> nrow()} blocks ",
    "({blocks |> dplyr::filter(.data$block > 0, .data$block_name == ref_block_name) |> nrow()} {.field {ref_block_name}}, ",
    "{blocks |> dplyr::filter(.data$block > 0, .data$block_name == sample_block_name) |> nrow()} {.field {sample_block_name}}) ",
    "in data from ",
    "{blocks |> dplyr::select('filename') |> dplyr::distinct() |> nrow()} file{?s}",
    start = start
  )

  # combine with the whole dataset
  scans <-
    scans |>
    dplyr::left_join(
      single_scans_with_blocks |>
        dplyr::select(
          dplyr::all_of(by_cols),
          "scan.no",
          "data_group",
          "block",
          "block_name",
          "data_type",
          "segment"
        ),
      by = c(by_cols, "scan.no")
    )

  # return updated dataset
  if (is_agg) {
    # got aggregated data to begin with --> return aggregated data
    dataset$scans <- scans |> dplyr::select(-dplyr::any_of("filename"))
    return(dataset)
  } else {
    # got a plain peaks tibble
    return(scans)
  }
}

#' @title Manually adjust block delimiters
#' @description This function can be used to manually adjust where certain blocks start or end after they have been defined with [orbi_define_blocks()] or [orbi_define_blocks_for_dual_inlet()] using either time or scan number.
#' The adjustments can be provided as vectors in the individual parameters or as a `blocks_table` - whichever is more convenient.
#' Note that adjusting blocks removes all block segmentation. Make sure to call [orbi_segment_blocks()] **after** adjusting block delimiters.
#'
#' @inheritParams orbi_flag_satellite_peaks
#' @param block the block(s) for which to adjust the start and/or end, a single value or a vector for multiple adjustments. Note that blocks are numbered within each file.
#' @param in_filename the file(s) in which to adjust the block(s), a single value or a vector for multiple adjustments, `NA` (the default if not provided) adjusts the block in all files that have it. This must be the `filename` of the file(s) as it appears in the `dataset`.
#' @param shift_start_time.min if provided, the start time of the block will be shifted by this many minutes (use negative numbers to shift back)
#' @param shift_end_time.min if provided, the end time of the block will be shifted by this many minutes (use negative numbers to shift back)
#' @param shift_start_scan.no if provided, the start of the block will be shifted by this many scans (use negative numbers to shift back)
#' @param shift_end_scan.no if provided, the end of the block will be shifted by this many scans (use negative numbers to shift back)
#' @param set_start_time.min if provided, sets the start time of the block as close as possible to this time
#' @param set_end_time.min if provided, sets the end time of the block as close as possible to this time
#' @param set_start_scan.no if provided, sets the start of the block to this scan number (scan must exist in the `dataset`)
#' @param set_end_scan.no if provided, sets the end of the block to this scan number (scan must exist in the `dataset`)
#' @param blocks_table alternative to the individual parameters: a data frame with the column `block` and any of the columns `in_filename`, `shift_start_time.min`, `shift_end_time.min`, `shift_start_scan.no`, `shift_end_scan.no`, `set_start_time.min`, `set_end_time.min`, `set_start_scan.no` and `set_end_scan.no`, one row per adjustment. Any other columns are ignored. If provided, the individual parameters are not used.
#' @details Each adjustment can change the start of a block (with only one of `shift_start_time.min`, `shift_start_scan.no`, `set_start_time.min`, or `set_start_scan.no`) and/or the end of a block (with only one of `shift_end_time.min`, `shift_end_scan.no`, `set_end_time.min`, or `set_end_scan.no`). The adjustments are applied in the order they are provided.
#' @return A data frame (tibble) with block limits altered according to the provided start/end change parameters. Any data that is no longer part of the original block will be marked with the value of `orbi_get_option("data_type_unused")`. Any previously applied segmentation of the adjusted blocks' files will be discarded (`segment` column set to `NA`) to avoid unintended side effects.
#' @examples
#' fpath <- system.file("extdata", "testfile_dual_inlet.isox", package = "isoorbi")
#' df <- orbi_read_isox(file = fpath) |>
#'   orbi_simplify_isox() |>
#'   orbi_define_blocks_for_dual_inlet(
#'     ref_block_time.min = 0.5,
#'     change_over_time.min = 0.1
#'   )
#'
#' # shift the start of block 1 by 6 seconds in all files
#' df |> orbi_adjust_blocks(block = 1, shift_start_time.min = 0.1)
#'
#' # several adjustments at once
#' df |> orbi_adjust_blocks(
#'   block = c(1, 2),
#'   shift_start_time.min = c(0.1, 0.05),
#'   shift_end_time.min = c(NA, -0.05)
#' )
#'
#' # the same via a blocks table
#' df |> orbi_adjust_blocks(
#'   blocks_table = tibble::tibble(
#'     block = c(1, 2),
#'     shift_start_time.min = c(0.1, 0.05),
#'     shift_end_time.min = c(NA, -0.05)
#'   )
#' )
#' @export
orbi_adjust_blocks <- function(
  dataset,
  block = NULL,
  in_filename = NULL,
  shift_start_time.min = NULL,
  shift_end_time.min = NULL,
  shift_start_scan.no = NULL,
  shift_end_scan.no = NULL,
  set_start_time.min = NULL,
  set_end_time.min = NULL,
  set_start_scan.no = NULL,
  set_end_scan.no = NULL,
  blocks_table = NULL
) {
  # safety checks
  check_dataset_arg(dataset)

  # assemble + validate the adjustments
  adjustments <- get_adjustments_table(
    blocks_table = blocks_table,
    block = block,
    in_filename = in_filename,
    shift_start_time.min = shift_start_time.min,
    shift_end_time.min = shift_end_time.min,
    shift_start_scan.no = shift_start_scan.no,
    shift_end_scan.no = shift_end_scan.no,
    set_start_time.min = set_start_time.min,
    set_end_time.min = set_end_time.min,
    set_start_scan.no = set_start_scan.no,
    set_end_scan.no = set_end_scan.no
  )

  # get scans
  is_agg <- is(dataset, "orbi_aggregated_data")
  scans <- if (is_agg) {
    dataset$scans |>
      dplyr::left_join(
        dataset$file_info |> dplyr::select("uidx", "filename"),
        by = "uidx"
      )
  } else {
    dataset
  }

  # dataset columns check
  if (!"block" %in% names(scans)) {
    cli_abort(
      "the {.field dataset} does not seem to have any block definitions yet, make sure to run {.strong orbi_define_blocks()} or {.strong orbi_define_blocks_for_dual_inlet()} first"
    )
  }
  time_cols <- c(
    "shift_start_time.min",
    "shift_end_time.min",
    "set_start_time.min",
    "set_end_time.min"
  )
  check_tibble(
    scans,
    req_cols = c(
      "filename",
      "scan.no",
      if (any(!is.na(as.matrix(adjustments[time_cols])))) "time.min",
      "block",
      "block_name",
      "data_type"
    ),
    .arg = "dataset"
  )

  # get single scans with blocks and data types from the data set
  single_scans <- scans |>
    dplyr::select(
      "filename",
      "scan.no",
      "block",
      "block_name",
      "data_type",
      dplyr::any_of(c("uidx", "time.min", "data_group", "segment"))
    ) |>
    dplyr::distinct()

  # adjustments for specific files need those files to be in the dataset
  filenames <- unique(as.character(single_scans$filename))
  unknown <- setdiff(stats::na.omit(adjustments$in_filename), filenames)
  if (length(unknown) > 0L) {
    cli_abort(
      c(
        "{.field in_filename} {.val {unknown}} {?is/are} not in this {.field dataset}",
        "i" = "available filename{?s}: {.val {filenames}}"
      )
    )
  }

  # info
  start <- start_info(
    "is adjusting {nrow(adjustments)} block{?s}"
  )

  # apply the adjustments one at a time (in each of the files they apply to)
  root_env <- current_env()
  details <- character(0)
  adjusted <- tibble(filename = character(0), block = integer(0))
  for (i in seq_len(nrow(adjustments))) {
    adj <- adjustments[i, ]
    files_with_block <- single_scans |>
      dplyr::filter(.data$block == !!adj$block) |>
      dplyr::pull(.data$filename) |>
      as.character() |>
      unique()
    if (!is.na(adj$in_filename)) {
      # a specific file
      if (!adj$in_filename %in% files_with_block) {
        cli_abort(
          "{.field block} {.val {adj$block}} is not in file {cli::col_blue(adj$in_filename)}"
        )
      }
      files <- adj$in_filename
    } else {
      # all files that have this block
      if (length(files_with_block) == 0L) {
        cli_abort(
          "{.field block} {.val {adj$block}} is not in this {.field dataset}"
        )
      }
      missing <- setdiff(filenames, files_with_block)
      if (length(missing) > 0L) {
        cli_warn(
          "{.field block} {.val {adj$block}} is not in {qty(length(missing))} file{?s} {cli::col_blue(missing)} and was not adjusted there"
        )
      }
      files <- intersect(filenames, files_with_block)
    }
    for (file in files) {
      out <- adjust_single_block(single_scans, adj, file, .env = root_env)
      single_scans <- out$single_scans
      details <- c(details, out$details)
      if (out$changed) {
        adjusted <- adjusted |>
          dplyr::bind_rows(tibble(filename = file, block = adj$block))
      }
    }
  }

  # any changes?
  if (nrow(adjusted) == 0L) {
    finish_info(
      "made no changes to the {.field blocks} as no actual changes were requested",
      start = start
    )
    return(dataset)
  }

  # info message for changes
  n_adjusted <- nrow(dplyr::distinct(adjusted))
  n_files <- length(unique(adjusted$filename))
  finish_info(
    "adjusted {n_adjusted} block{?s} in {n_files} file{?s}",
    start = start
  )
  details |>
    cli_bullets_raw() |>
    cli()

  # combine with the whole dataset
  # (done this way to support both tibble and aggregated data)
  scans <-
    scans |>
    dplyr::select(
      -"block",
      -"block_name",
      -"data_type",
      -dplyr::any_of(c("data_group", "segment"))
    ) |>
    dplyr::left_join(
      single_scans |>
        dplyr::select(
          dplyr::any_of(
            c(
              "uidx",
              "filename",
              "scan.no",
              "data_group",
              "block",
              "block_name",
              "data_type",
              "segment"
            )
          )
        ),
      by = intersect(c("uidx", "filename", "scan.no"), names(scans))
    )

  # return updated dataset
  if (is_agg) {
    # got aggregated data to begin with --> return aggregated data
    dataset$scans <- scans |> dplyr::select(-dplyr::any_of("filename"))
    return(dataset)
  } else {
    # got a plain peaks tibble
    return(scans)
  }
}

#' @title Manually adjust a block delimiter
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `orbi_adjust_block()` was renamed [orbi_adjust_blocks()] since it can now adjust several blocks at once
#' (and identifies the file with `in_filename` instead of `filename`).
#' @inheritParams orbi_adjust_blocks
#' @param block the block for which to adjust the start and/or end
#' @param filename needs to be specified only if the `dataset` has more than one `filename`
#' @return see [orbi_adjust_blocks()]
#' @export
orbi_adjust_block <- function(
  dataset,
  block,
  filename = NULL,
  shift_start_time.min = NULL,
  shift_end_time.min = NULL,
  shift_start_scan.no = NULL,
  shift_end_scan.no = NULL,
  set_start_time.min = NULL,
  set_end_time.min = NULL,
  set_start_scan.no = NULL,
  set_end_scan.no = NULL
) {
  lifecycle::deprecate_warn(
    "1.6.0",
    "orbi_adjust_block()",
    "orbi_adjust_blocks()",
    details = "The new function can adjust several blocks at once and identifies the file with `in_filename` instead of `filename`."
  )
  check_dataset_arg(dataset)

  # this function always required the filename if there is more than one file
  # (orbi_adjust_blocks() adjusts the block in all files instead)
  filenames <- if (is(dataset, "orbi_aggregated_data")) {
    dataset$file_info$filename
  } else {
    dataset[["filename"]]
  }
  if (length(unique(filenames)) > 1 && is.null(filename)) {
    cli_abort(
      "{.field dataset} has data from more than 1 file - specify the {.field filename} argument for block adjustment"
    )
  }

  return(orbi_adjust_blocks(
    dataset = dataset,
    block = if (!missing(block)) block,
    in_filename = filename,
    shift_start_time.min = shift_start_time.min,
    shift_end_time.min = shift_end_time.min,
    shift_start_scan.no = shift_start_scan.no,
    shift_end_scan.no = shift_end_scan.no,
    set_start_time.min = set_start_time.min,
    set_end_time.min = set_end_time.min,
    set_start_scan.no = set_start_scan.no,
    set_end_scan.no = set_end_scan.no
  ))
}

#' @title Segment data blocks
#' @description This step is optional and is intended to make it easy to explore the data within a sample or ref data block. Note that any raw data not identified with `data_type` set to "data" (`orbi_get_option("data_type_data")`) will stay unsegmented. This includes raw data flagged as "startup", "changeover", and "unused".
#' @inheritParams orbi_flag_satellite_peaks
#' @param into_segments segment each data block into this many segments. The result will have exactly this number of segments for each data block except for if there are more segments requested than observations in a group (in which case each observation will be one segment)
#' @param by_scans segment each data block into segments spanning this number of scans. The result will be approximately the requested number of scans per segment, depending on what is the most sensible distribution of the data. For example, in a hypothetical data block with 31 scans, if by_scans = 10, this function will create 3 segments with 11, 10 and 10 scans each (most evenly distributed), instead of 4 segments with 10, 10, 10, 1 (less evenly distributed).
#' @param by_time_interval segment each data block into segments spanning this time interval. The result will have the requested time interval for all segments except usually the last one which is almost always shorter than the requested interval.
#' @export
orbi_segment_blocks <- function(
  dataset,
  into_segments = NULL,
  by_scans = NULL,
  by_time_interval = NULL
) {
  # safety checks
  check_dataset_arg(dataset)
  stopifnot(
    "if set, `into_segments` must be a single positive integer" = is.null(
      into_segments
    ) ||
      (rlang::is_scalar_integerish(into_segments) && into_segments > 0L),
    "if set, `by_scans` must be a single positive integer" = is.null(
      by_scans
    ) ||
      (rlang::is_scalar_integerish(by_scans) && by_scans > 0L),
    "if set, `by_time_interval` must be a single positive number" = is.null(
      by_time_interval
    ) ||
      (rlang::is_scalar_double(by_time_interval) && by_time_interval > 0)
  )
  into_segments <- as.integer(into_segments)
  by_scans <- as.integer(by_scans)

  # get scans
  is_agg <- is(dataset, "orbi_aggregated_data")
  scans <- if (is_agg) {
    dataset$scans |>
      dplyr::left_join(
        dataset$file_info |> dplyr::select("uidx", "filename"),
        by = "uidx"
      )
  } else {
    dataset
  }

  # dataset columns check
  if (!"block" %in% names(scans)) {
    cli_abort(
      "the {.field dataset} does not seem to have any block definitions yet, make sure to run {.strong orbi_define_blocks_for_dual_inlet()} or {.strong orbi_define_blocks()} first"
    )
  }

  req_cols <- c(
    "filename",
    "scan.no",
    "time.min",
    "block",
    "block_name",
    "data_type"
  )
  if ("uidx" %in% names(scans)) {
    req_cols <- c("uidx", req_cols)
  }
  check_tibble(scans, req_cols = req_cols, .arg = "dataset")

  # provide exactly one argument on how to segment
  set_args <- sum(
    !is_empty(into_segments),
    !is_empty(by_time_interval),
    !is_empty(by_scans)
  )
  if (set_args == 0) {
    rlang::abort(
      "set one of the 3 ways to segment: `into_segments`, `by_time_interval`, `by_scans`"
    )
  } else if (set_args > 1) {
    rlang::abort(
      "only set ONE of the 3 ways to segment: `into_segments`, `by_time_interval`, `by_scans`"
    )
  }

  # info
  start <- start_info("is running")
  by_cols <- "filename"
  if ("uidx" %in% names(scans)) {
    by_cols <- c("uidx", by_cols)
  }

  # get single scans (for compatibility with tibble, technically redundant for aggregated data)
  single_scans <- scans |>
    dplyr::select(dplyr::all_of(req_cols)) |>
    dplyr::distinct() |>
    # make sure it's in the correct order (for data group identification later)
    dplyr::arrange(!!!purrr::map(by_cols, sym), .data$scan.no)

  # calculate segments
  segmented_scans <-
    single_scans |>
    # segment
    dplyr::mutate(
      .by = dplyr::all_of(c(by_cols, "block", "data_type")),
      segment = if (.data$data_type[1] == orbi_get_option("data_type_data")) {
        # data type is 'data'
        if (!is_empty(!!into_segments)) {
          segment_by_segs(.data$scan.no, !!into_segments)
        } else if (!is_empty(!!by_scans)) {
          segment_by_scans(.data$scan.no, !!by_scans)
        } else {
          segment_by_time_interval(.data$time.min, !!by_time_interval)
        }
      } else {
        NA_integer_
      }
    ) |>
    # determine data groups
    determine_data_groups()

  # info message
  info_sum <- segmented_scans |>
    dplyr::filter(
      .data$block > 0,
      .data$data_type == orbi_get_option("data_type_data")
    ) |>
    dplyr::select(dplyr::all_of(c(by_cols, "block", "segment", "scan.no"))) |>
    dplyr::distinct() |>
    dplyr::count(!!!purrr::map(by_cols, sym), .data$block, .data$segment) |>
    dplyr::summarise(
      .by = dplyr::all_of(c(by_cols, "block")),
      n_segments = n(),
      n_scans_avg = mean(n)
    )

  segmented_blocks <- scans |>
    dplyr::filter(.data$block > 0) |>
    dplyr::select("filename", "block") |>
    dplyr::distinct()
  n_blocks <- nrow(segmented_blocks)
  n_files <- length(unique(segmented_blocks$filename))
  finish_info(
    "segmented {.field {n_blocks} data block{?s}} ",
    "in {n_files} file{?s} ",
    "creating {info_sum$n_segments |> mean() |> signif(2)} ",
    "segments per block (on average) with ",
    "{info_sum$n_scans_avg |> mean() |> signif(2)} ",
    "scans per segment (on average)",
    start = start
  )

  # combine with the whole dataset
  scans <-
    scans |>
    dplyr::select(
      -"block",
      -"block_name",
      -"data_type",
      -dplyr::any_of(c("data_group", "segment"))
    ) |>
    dplyr::left_join(
      segmented_scans |>
        dplyr::select(
          dplyr::all_of(by_cols),
          "scan.no",
          "data_group",
          "block",
          "block_name",
          "data_type",
          "segment"
        ),
      by = c(by_cols, "scan.no")
    )

  # return updated dataset
  if (is_agg) {
    # got aggregated data to begin with --> return aggregated data
    dataset$scans <- scans |> dplyr::select(-dplyr::any_of("filename"))
    return(dataset)
  } else {
    # got a plain peaks tibble
    return(scans)
  }
}

#' @title Summarize blocks info
#' @description This function provides an overview table `blocks_info` which shows information on blocks in the dataset (block number, sample name, data type, scan number and start time where a block starts, and scan number and end time where a block ends).
#' @inheritParams orbi_adjust_blocks
#' @param .by grouping columns for block info (akin to dplyr's `.by` parameter e.g. in [dplyr::summarize()]). If not set by the user, all columns in the parameter's default values are used, if present in the dataset.
#' @return a block summary or if no blocks defined yet, an empty tibble (with warning)
#' @export
orbi_get_blocks_info <- function(
  dataset,
  .by = c(
    "uidx",
    "filename",
    "injection",
    "data_group",
    "block",
    "block_name",
    "data_type",
    "segment"
  )
) {
  # safety checks
  check_dataset_arg(dataset)

  # get scans
  scans <- if (is(dataset, "orbi_aggregated_data")) {
    dataset$scans |>
      dplyr::left_join(
        dataset$file_info |> dplyr::select("uidx", "filename"),
        by = "uidx"
      )
  } else {
    dataset
  }

  # check required columns
  check_tibble(scans, "filename", .arg = "dataset")
  if (any(grepl("start|end", names(scans)))) {
    # seems already summarized
    check_tibble(
      scans,
      c("start_scan.no", "end_scan.no", "start_time.min", "end_time.min"),
      .arg = "dataset"
    )
    # return
    scan_info <- scans |>
      dplyr::select(
        dplyr::any_of(.by),
        "start_scan.no",
        "end_scan.no",
        "start_time.min",
        "end_time.min"
      )
    return(scan_info)
  }

  # continue with regular blocks summary
  check_tibble(scans, c("scan.no", "time.min"), .arg = "dataset")

  # empty blocks
  empty <- scans |>
    dplyr::summarise(
      .by = dplyr::any_of(c("uidx", "filename")),
      data_group = NA_integer_,
      block = NA_integer_,
      block_name = NA_character_,
      data_type = factor(NA_character_),
      segment = NA_integer_,
      start_scan.no = min(.data$scan.no),
      end_scan.no = max(.data$scan.no),
      start_time.min = min(.data$time.min),
      end_time.min = max(.data$time.min)
    )

  # no information
  if (nrow(scans) == 0) {
    # return empty block info
    return(empty)
  }
  if (!"block" %in% names(scans)) {
    cli_bullets(
      c(
        "!" = "{cli::col_yellow('Warning')}: {.field dataset} does not seem to have any block definitions yet ({.field block} column missing)"
      )
    )
    # return empty block info
    return(empty)
  }

  # summarize block info
  scans_info <- scans |>
    group_if_exists(.by) |>
    dplyr::summarise(
      start_scan.no = min(.data$scan.no),
      end_scan.no = max(.data$scan.no),
      start_time.min = min(.data$time.min),
      end_time.min = max(.data$time.min),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$filename, .data$start_scan.no)
  return(scans_info)
}

#' @title Plot blocks background
#' @description This function can be used to add colored background to a plot of dual-inlet data where different colors signify different data types (data, startup time, changeover time, unused). Note that this function only works with continuous and pseudo-log y axis, not with log y axes.
#'
#' @param plot object with a dataset that has defined blocks
#' @param x which x-axis to use (time vs. scan number). If set to "guess" (the default), the function will try to figure it out from the plot.
#' @param data_only if set to TRUE, only the blocks flagged as "data" (`orbi_get_option("data_type_data")`) are highlighted
#' @param use_data_block_names whether to label the data blocks by their individual `block_name` (if they have one) instead of just as "data" (the default). This allows color coding the background of the different data blocks (e.g. reference vs. sample). All other blocks (e.g. "unused") are always labeled by their data type.
#' @param fill what to use for the fill aesthetic, default is the `block_label`, i.e. the block's `data_type` or, for data blocks with `use_data_block_names = TRUE`, the `block_name`
#' @param fill_colors which colors to use, by default a color-blind friendly color palettes (RColorBrewer, dark2)
#' @param fill_scale use this parameter to replace the entire fill scale rather than just the `fill_colors`
#' @param alpha opacity settings for the background
#' @param show.legend whether to include the background information in the legend
#' @export
#' @importFrom methods is
orbi_add_blocks_to_plot <- function(
  plot,
  x = c("guess", "scan.no", "time.min"),
  data_only = FALSE,
  use_data_block_names = FALSE,
  fill = .data$block_label,
  fill_colors = c(
    "#1B9E77",
    "#D95F02",
    "#7570B3",
    "#E7298A",
    "#66A61E",
    "#E6AB02",
    "#A6761D",
    "#666666"
  ),
  fill_scale = scale_fill_manual("blocks", values = fill_colors),
  alpha = 0.5,
  show.legend = !data_only
) {
  # safety checks
  check_arg(
    plot,
    !missing(plot) && ggplot2::is_ggplot(plot),
    "has to be a ggplot"
  )
  x_column <- arg_match(x)
  check_arg(
    use_data_block_names,
    is_bool(use_data_block_names),
    "must be TRUE or FALSE"
  )

  # check if it's a log y axis
  y_axis <- plot$scales$scales[[which(plot$scales$find("y"))[1]]]
  y_log <- FALSE
  if (!is.null(y_axis)) {
    y_zero <- suppressWarnings(y_axis$transform(0))
    if (y_zero < 0 && is.infinite(y_zero)) {
      y_log <- TRUE
    }
  }

  # find out if it's a scan.no or time.min based plot if x is "guess"
  if (x_column == "guess") {
    x_column <- plot$mapping$x |> as_label()
    if (!x_column %in% c("scan.no", "time.min")) {
      sprintf(
        "cannot guess x-axis for blocks from plot as its aes(x = ) variable is neither 'scan.no' nor 'time.min'"
      ) |>
        warn()
      return(plot)
    }
  }

  # get blocks
  get_blocks <- function(df) {
    if (!has_blocks(df)) {
      abort(
        "this data set does not seem to have any block defintions yet, use a function from the `orbi_define_block...` family to define one or multiple data blocks"
      )
    }
    if (data_only) {
      df <- df |>
        dplyr::filter(.data$data_type == orbi_get_option("data_type_data"))
    }
    blocks <- df |>
      orbi_get_blocks_info() |>
      dplyr::filter(!is.na(.data$block))

    # block labels: the data type or (if requested) the name of data blocks
    data_type <- as.character(blocks$data_type)
    is_named_data <- use_data_block_names &
      data_type == orbi_get_option("data_type_data") &
      !is.na(blocks$block_name)
    block_label <- ifelse(is_named_data, blocks$block_name, data_type)
    # named data blocks first (in the order they occur), then the data types
    blocks$block_label <- factor(
      block_label,
      levels = unique(c(
        block_label[is_named_data],
        levels(factor(blocks$data_type))
      ))
    ) |>
      droplevels()

    blocks |>
      dplyr::mutate(
        .by = dplyr::any_of(c("uidx", "filename")),
        xmin = if (!!x_column == "time.min") {
          .data$start_time.min -
            0.5 *
              (max(.data$end_time.min) - min(.data$start_time.min)) /
              (max(.data$end_scan.no) - min(.data$start_scan.no))
        } else {
          .data$start_scan.no - 0.5
        },
        xmax = if (!!x_column == "time.min") {
          .data$end_time.min +
            0.5 *
              (max(.data$end_time.min) - min(.data$start_time.min)) /
              (max(.data$end_scan.no) - min(.data$start_scan.no))
        } else {
          .data$end_scan.no + 0.5
        }
      )
  }

  # add the rectangle plot as underlying layer
  plot$layers <- c(
    ggplot2::geom_rect(
      data = get_blocks,
      map = ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        color = NULL,
        ymin = if (y_log) 0 else -Inf,
        ymax = Inf,
        fill = {{ fill }}
      ),
      inherit.aes = FALSE,
      alpha = alpha,
      linetype = 0,
      color = NA_character_,
      show.legend = show.legend
    ),
    plot$layers
  )

  # return plot
  plot <- plot +
    {
      {
        fill_scale
      }
    } +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(fill = NA_character_),
        order = 1
      )
    ) +
    ggplot2::guides(
      shape = ggplot2::guide_legend(
        override.aes = list(fill = NA_character_),
        order = 1
      )
    ) +
    # keep the blocks legend after the data legends - without an explicit order
    # ggplot2 does not guarantee a stable sequence across platforms
    ggplot2::guides(fill = ggplot2::guide_legend(order = 2))
  return(plot)
}

# internal functions ------------

# the columns that make up a block definition
block_def_cols <- c(
  "start_time.min",
  "end_time.min",
  "start_scan.no",
  "end_scan.no",
  "block_name",
  "in_filename"
)

# assemble the block definitions from either a blocks_table or the individual
# parameters and make sure every row defines exactly one of the two kinds of block
get_blocks_table <- function(
  blocks_table,
  start_time.min,
  end_time.min,
  start_scan.no,
  end_scan.no,
  block_name,
  in_filename = NULL,
  .env = caller_env()
) {
  if (!is.null(blocks_table)) {
    # provided as a table
    check_arg(
      blocks_table,
      is.data.frame(blocks_table),
      "must be a data frame",
      .arg = "blocks_table",
      .env = .env
    )
    if (nrow(blocks_table) == 0L) {
      cli_abort(
        "{.field blocks_table} must have at least one row",
        call = .env
      )
    }
    if (length(intersect(names(blocks_table), block_def_cols)) == 0L) {
      cli_abort(
        c(
          "{.field blocks_table} has none of the block definition columns",
          "i" = "expected any of {.field {block_def_cols}}",
          "i" = "found {.field {names(blocks_table)}}"
        ),
        call = .env
      )
    }
    # only keep the recognized columns, fill in the missing ones
    blocks_table <- blocks_table |>
      dplyr::select(dplyr::any_of(block_def_cols))
  } else {
    # provided as individual parameters, recycled to the longest one
    args <- list(
      start_time.min = start_time.min,
      end_time.min = end_time.min,
      start_scan.no = start_scan.no,
      end_scan.no = end_scan.no,
      block_name = block_name,
      in_filename = in_filename
    )
    n <- max(c(1L, lengths(args)))
    wrong_length <- names(args)[
      lengths(args) > 0L & lengths(args) != 1L & lengths(args) != n
    ]
    if (length(wrong_length) > 0L) {
      cli_abort(
        c(
          "{.field {wrong_length}} {?has/have} a length that cannot be recycled to the {n} block{?s} being defined",
          "i" = "provide either a single value or one value per block"
        ),
        call = .env
      )
    }
    blocks_table <- args |>
      purrr::compact() |>
      purrr::map(~ rep_len(.x, n)) |>
      tibble::as_tibble()
  }

  # fill in whatever is missing and enforce the types
  for (col in setdiff(block_def_cols, names(blocks_table))) {
    blocks_table[[col]] <- NA
  }
  types <- list(
    start_time.min = c("numeric", "a number"),
    end_time.min = c("numeric", "a number"),
    start_scan.no = c("integerish", "a whole number"),
    end_scan.no = c("integerish", "a whole number"),
    block_name = c("character", "text"),
    in_filename = c("character", "text")
  )
  for (col in block_def_cols) {
    values <- blocks_table[[col]]
    ok <- all(is.na(values)) ||
      switch(
        types[[col]][1],
        numeric = is.numeric(values),
        integerish = is_integerish(values),
        character = is.character(values) || is.factor(values)
      )
    if (!ok) {
      cli_abort(
        "{.field {col}} must be {types[[col]][2]}, not {.obj_type_friendly {values}}",
        call = .env
      )
    }
  }

  # only the end of a block can be infinite (i.e. until the end of the file)
  for (col in c("start_time.min", "start_scan.no")) {
    if (any(is.infinite(blocks_table[[col]]))) {
      cli_abort(
        "{.field {col}} must be finite (only the end of a block can be {.val {Inf}})",
        call = .env
      )
    }
  }
  for (col in c("end_time.min", "end_scan.no")) {
    if (any(blocks_table[[col]] == -Inf, na.rm = TRUE)) {
      cli_abort(
        "{.field {col}} cannot be {.val {-Inf}}, use {.val {Inf}} for the end of the file",
        call = .env
      )
    }
  }
  blocks_table <- blocks_table |>
    dplyr::select(dplyr::all_of(block_def_cols)) |>
    dplyr::mutate(
      start_time.min = as.numeric(.data$start_time.min),
      end_time.min = as.numeric(.data$end_time.min),
      start_scan.no = as.integer(.data$start_scan.no),
      # note: stays numeric so it can be Inf (i.e. until the last scan)
      end_scan.no = as.numeric(.data$end_scan.no),
      block_name = as.character(.data$block_name),
      in_filename = as.character(.data$in_filename)
    )

  # each block has to be defined by time OR by scan, not both and not neither
  by_time <- !is.na(blocks_table$start_time.min) &
    !is.na(blocks_table$end_time.min)
  by_scan <- !is.na(blocks_table$start_scan.no) &
    !is.na(blocks_table$end_scan.no)
  if (any(by_time & by_scan)) {
    rows <- which(by_time & by_scan)
    cli_abort(
      "block definition{?s} {rows} can either be by time or by scan but not both",
      call = .env
    )
  }
  if (any(!by_time & !by_scan)) {
    rows <- which(!by_time & !by_scan)
    cli_abort(
      c(
        "block definition{?s} {rows} {?is/are} incomplete",
        "i" = "each block requires either {.field start_time.min} and {.field end_time.min} or {.field start_scan.no} and {.field end_scan.no}"
      ),
      call = .env
    )
  }

  return(blocks_table)
}

# add a single block (one validated row of a blocks table) to the dataset.
# does not emit any messages, the caller reports on all blocks at once
add_single_block <- function(dataset, block_def, .env = caller_env()) {
  set_by_time <- !is.na(block_def$start_time.min)

  # get scans
  is_agg <- is(dataset, "orbi_aggregated_data")
  scans <- if (is_agg) {
    dataset$scans |>
      dplyr::left_join(
        dataset$file_info |> dplyr::select("uidx", "filename"),
        by = "uidx"
      )
  } else {
    dataset
  }

  # check required columns
  check_tibble(
    scans,
    req_cols = c("filename", "scan.no", if (set_by_time) "time.min"),
    .arg = "dataset",
    .env = .env
  )

  # get single scans with blocks and data types from the data set
  single_scans <- scans |>
    dplyr::select(
      "scan.no",
      "filename",
      dplyr::any_of(c(
        "uidx",
        "time.min",
        "data_group",
        "block",
        "block_name",
        "data_type",
        "segment"
      ))
    ) |>
    dplyr::distinct()

  # make sure columns exist
  if (!"block" %in% names(single_scans)) {
    single_scans$block <- 0L
  }
  if (!"block_name" %in% names(single_scans)) {
    single_scans$block_name <- NA_character_
  }
  if (!"data_type" %in% names(single_scans)) {
    single_scans$data_type <- orbi_get_option("data_type_unused")
  }

  # nest
  single_scans <- single_scans |>
    tidyr::nest(data = -dplyr::any_of(c("uidx", "filename")))
  root_env <- current_env()

  # which files does this block apply to? (all unless it has an in_filename)
  single_scans$in_block_file <- is.na(block_def$in_filename) |
    single_scans$filename == block_def$in_filename

  # find start scans
  # note: a time outside a file's range yields NA (i.e. the block is not added in
  # that file) rather than an error, the same way an out of range scan number does
  if (set_by_time) {
    single_scans <- single_scans |>
      dplyr::mutate(
        start_scan.no = data |>
          purrr::map_int(
            find_scan_from_time,
            block_def$start_time.min,
            "start",
            allow_missing = TRUE,
            .env = root_env
          ),
        end_scan.no = data |>
          purrr::map_int(
            find_scan_from_time,
            block_def$end_time.min,
            "end",
            allow_missing = TRUE,
            .env = root_env
          )
      )
  } else {
    single_scans <- single_scans |>
      dplyr::mutate(
        start_scan.no = !!block_def$start_scan.no,
        # an infinite end means until the last scan of each file
        end_scan.no = if (is.infinite(block_def$end_scan.no)) {
          purrr::map_int(data, ~ max(.x$scan.no))
        } else {
          as.integer(block_def$end_scan.no)
        }
      )
  }

  # files the block does not apply to don't get any part of it
  single_scans <- single_scans |>
    dplyr::mutate(
      start_scan.no = dplyr::if_else(
        .data$in_block_file,
        .data$start_scan.no,
        NA_integer_
      ),
      end_scan.no = dplyr::if_else(
        .data$in_block_file,
        .data$end_scan.no,
        NA_integer_
      )
    )

  # determine new block numbers
  single_scans <- single_scans |>
    dplyr::mutate(
      next_block = purrr::map_int(data, ~ max(.x$block) + 1L)
    ) |>
    tidyr::unnest("data")

  # actualize changes
  out <-
    try_catch_cnds(
      single_scans |>
        # introduce updated segment, block, data type and block_name
        dplyr::mutate(
          new_block = ifelse(
            !is.na(.data$start_scan.no) &
              !is.na(.data$end_scan.no) &
              .data$scan.no >= .data$start_scan.no &
              .data$scan.no <= .data$end_scan.no,
            .data$next_block,
            .data$block
          ),
          block_name = ifelse(
            !is.na(!!block_def$block_name) &
              .data$new_block == .data$next_block,
            !!block_def$block_name,
            .data$block_name
          ),
          data_type = ifelse(
            .data$new_block == .data$next_block,
            orbi_get_option("data_type_data"),
            .data$data_type
          ),
          segment = NA_integer_
        ) |>
        # determine data groups
        determine_data_groups()
    )

  abort_cnds(out$conditions, message = "error trying to update scan blocks")
  single_scans <- out$result

  # what does the block actually cover in each file? (the requested start/end are
  # snapped to the scans that exist, so this can be narrower than what was asked for)
  # note: only for the files the block applies to
  coverage <- single_scans |>
    dplyr::filter(.data$in_block_file) |>
    summarize_block_coverage()

  # warn about files where the block does not cover any scans at all, i.e. where the
  # requested times or scan numbers fall outside what the file recorded (this also
  # catches a reversed range, where the start comes after the end)
  unmatched <- coverage |> dplyr::filter(.data$n_scans == 0L)
  if (nrow(unmatched) > 0L) {
    block_desc <- describe_blocks(block_def)
    n_show <- 5L
    notes <- purrr::map_chr(
      seq_len(min(nrow(unmatched), n_show)),
      \(i) {
        file_range <- if (set_by_time) {
          format_inline(
            "covers {mins_to_text(unmatched$file_start_time[i])} to {mins_to_text(unmatched$file_end_time[i])}"
          )
        } else {
          format_inline(
            "covers scans {unmatched$file_start_scan[i]} to {unmatched$file_end_scan[i]}"
          )
        }
        format_inline(
          "no scans in {col_blue(unmatched$filename[i])} ({file_range})"
        )
      }
    )
    if (nrow(unmatched) > n_show) {
      notes <- c(
        notes,
        format_inline("... and {nrow(unmatched) - n_show} more file{?s}")
      )
    }
    cli_warn(
      c(
        "!" = "{block_desc} is outside the data in {nrow(unmatched)} file{?s} and was not added there",
        notes |> set_names(rep("i", length(notes)))
      ),
      call = .env
    )
  }

  # NOTE: should this provide more information and/or allow an argument to make
  # the new plot definition flexible? (snap_to_blocks = TRUE?)
  if (
    any(single_scans$block > 0L & single_scans$new_block > single_scans$block)
  ) {
    cli_abort(
      "block definition ({describe_blocks(block_def)}) overlaps with an existing block",
      call = .env
    )
  }

  # combine with the whole dataset
  out <-
    try_catch_cnds(
      scans |>
        dplyr::select(
          -dplyr::any_of(c(
            "data_group",
            "block",
            "block_name",
            "data_type",
            "segment"
          ))
        ) |>
        dplyr::left_join(
          single_scans |>
            dplyr::select(
              dplyr::any_of(c("uidx", "filename")),
              "scan.no",
              "data_group",
              "block" = "new_block",
              "block_name",
              "data_type",
              "segment"
            ),
          by = intersect(c("uidx", "filename", "scan.no"), names(scans))
        )
    )

  # stop if errors
  abort_cnds(
    out$conditions,
    message = "error trying to update dataset with new block"
  )
  scans <- out$result

  # return updated dataset together with what the block actually covers
  if (is_agg) {
    dataset$scans <- scans |> dplyr::select(-dplyr::any_of("filename"))
  } else {
    dataset <- scans
  }
  return(list(dataset = dataset, coverage = coverage))
}

# the columns that make up a block adjustment
adjust_def_cols <- c(
  "block",
  "in_filename",
  "shift_start_time.min",
  "shift_end_time.min",
  "shift_start_scan.no",
  "shift_end_scan.no",
  "set_start_time.min",
  "set_end_time.min",
  "set_start_scan.no",
  "set_end_scan.no"
)

# assemble the block adjustments from either a blocks_table or the individual
# parameters and make sure every row is a valid adjustment
get_adjustments_table <- function(blocks_table, ..., .env = caller_env()) {
  if (!is.null(blocks_table)) {
    # provided as a table
    check_arg(
      blocks_table,
      is.data.frame(blocks_table),
      "must be a data frame",
      .arg = "blocks_table",
      .env = .env
    )
    if (nrow(blocks_table) == 0L) {
      cli_abort(
        "{.field blocks_table} must have at least one row",
        call = .env
      )
    }
    if (!"block" %in% names(blocks_table)) {
      cli_abort(
        c(
          "{.field blocks_table} requires a {.field block} column",
          "i" = "found {.field {names(blocks_table)}}"
        ),
        call = .env
      )
    }
    # only keep the recognized columns
    blocks_table <- blocks_table |>
      dplyr::select(dplyr::any_of(adjust_def_cols))
  } else {
    # provided as individual parameters, recycled to the longest one
    args <- list(...)[adjust_def_cols]
    names(args) <- adjust_def_cols
    if (is.null(args$block)) {
      cli_abort(
        "{.field block} is required to identify which block(s) to adjust",
        call = .env
      )
    }
    n <- max(c(1L, lengths(args)))
    wrong_length <- names(args)[
      lengths(args) > 0L & lengths(args) != 1L & lengths(args) != n
    ]
    if (length(wrong_length) > 0L) {
      cli_abort(
        c(
          "{.field {wrong_length}} {?has/have} a length that cannot be recycled to the {n} adjustment{?s} being made",
          "i" = "provide either a single value or one value per adjustment"
        ),
        call = .env
      )
    }
    blocks_table <- args |>
      purrr::compact() |>
      purrr::map(~ rep_len(.x, n)) |>
      tibble::as_tibble()
  }

  # fill in whatever is missing and enforce the types
  for (col in setdiff(adjust_def_cols, names(blocks_table))) {
    blocks_table[[col]] <- NA
  }
  types <- c(
    block = "integerish",
    in_filename = "character",
    shift_start_time.min = "numeric",
    shift_end_time.min = "numeric",
    shift_start_scan.no = "integerish",
    shift_end_scan.no = "integerish",
    set_start_time.min = "numeric",
    set_end_time.min = "numeric",
    set_start_scan.no = "integerish",
    set_end_scan.no = "integerish"
  )
  type_labels <- c(
    integerish = "a whole number",
    numeric = "a number",
    character = "text"
  )
  for (col in adjust_def_cols) {
    values <- blocks_table[[col]]
    ok <- all(is.na(values)) ||
      switch(
        types[[col]],
        numeric = is.numeric(values),
        integerish = is_integerish(values),
        character = is.character(values) || is.factor(values)
      )
    if (!ok) {
      cli_abort(
        "{.field {col}} must be {type_labels[[types[[col]]]]}, not {.obj_type_friendly {values}}",
        call = .env
      )
    }
  }
  blocks_table <- blocks_table |>
    dplyr::select(dplyr::all_of(adjust_def_cols)) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(names(types)[types == "integerish"]),
        as.integer
      ),
      dplyr::across(
        dplyr::all_of(names(types)[types == "numeric"]),
        as.numeric
      ),
      in_filename = as.character(.data$in_filename)
    )

  # every adjustment needs a block
  if (any(is.na(blocks_table$block))) {
    rows <- which(is.na(blocks_table$block))
    cli_abort(
      "block adjustment{?s} {rows} {?is/are} missing the {.field block}",
      call = .env
    )
  }

  # at most one change of the start and one change of the end per adjustment
  start_cols <- c(
    "shift_start_time.min",
    "shift_start_scan.no",
    "set_start_time.min",
    "set_start_scan.no"
  )
  end_cols <- c(
    "shift_end_time.min",
    "shift_end_scan.no",
    "set_end_time.min",
    "set_end_scan.no"
  )
  n_start <- rowSums(!is.na(as.matrix(blocks_table[start_cols])))
  n_end <- rowSums(!is.na(as.matrix(blocks_table[end_cols])))
  if (any(n_start > 1L)) {
    cli_abort(
      "only provide ONE of {.or {.field {start_cols}}} to change the block start (block adjustment{qty(sum(n_start > 1L))}{?s} {which(n_start > 1L)})",
      call = .env
    )
  }
  if (any(n_end > 1L)) {
    cli_abort(
      "only provide ONE of {.or {.field {end_cols}}} to change the block end (block adjustment{qty(sum(n_end > 1L))}{?s} {which(n_end > 1L)})",
      call = .env
    )
  }

  return(blocks_table)
}

# adjust a single block (one validated row of the adjustments table) in a
# single file. does not emit any messages, returns the updated single_scans
# together with whether anything changed and the details of what changed
adjust_single_block <- function(
  single_scans,
  adj,
  filename,
  .env = caller_env()
) {
  block <- adj$block

  # get file scans
  file_scans <- single_scans |>
    dplyr::filter(.data$filename == !!filename) |>
    # make sure it's in the correct order (for data group identification later)
    dplyr::arrange(.data$scan.no)

  # find old start/end
  block_scans <- file_scans |>
    dplyr::filter(
      .data$block == !!block,
      .data$data_type == orbi_get_option("data_type_data")
    )
  old_start_scan <- block_scans$scan.no |> utils::head(1)
  old_end_scan <- block_scans$scan.no |> utils::tail(1)
  old_start_time <- block_scans$time.min |> utils::head(1)
  old_end_time <- block_scans$time.min |> utils::tail(1)

  # find new start
  new_start_scan <- if (!is.na(adj$shift_start_time.min)) {
    find_scan_from_time(
      file_scans,
      old_start_time + adj$shift_start_time.min,
      "start",
      .env = .env
    )
  } else if (!is.na(adj$set_start_time.min)) {
    find_scan_from_time(
      file_scans,
      adj$set_start_time.min,
      "start",
      .env = .env
    )
  } else if (!is.na(adj$shift_start_scan.no)) {
    old_start_scan + adj$shift_start_scan.no
  } else if (!is.na(adj$set_start_scan.no)) {
    adj$set_start_scan.no
  } else {
    # no change
    old_start_scan
  }

  # find new end
  new_end_scan <- if (!is.na(adj$shift_end_time.min)) {
    find_scan_from_time(
      file_scans,
      old_end_time + adj$shift_end_time.min,
      "end",
      .env = .env
    )
  } else if (!is.na(adj$set_end_time.min)) {
    find_scan_from_time(
      file_scans,
      adj$set_end_time.min,
      "end",
      .env = .env
    )
  } else if (!is.na(adj$shift_end_scan.no)) {
    old_end_scan + adj$shift_end_scan.no
  } else if (!is.na(adj$set_end_scan.no)) {
    adj$set_end_scan.no
  } else {
    # no change
    old_end_scan
  }

  old_start_row <- get_scan_row(file_scans, old_start_scan, .env = .env)
  new_start_row <- get_scan_row(file_scans, new_start_scan, .env = .env)
  new_end_row <- get_scan_row(file_scans, new_end_scan, .env = .env)
  new_start_time <- new_start_row$time.min
  new_end_time <- new_end_row$time.min

  # check that the block start/end are valid
  if (new_end_scan <= new_start_scan) {
    cli_abort(
      "invalid scan range adjustment requested for {.field block} {.val {block}} in file {cli::col_blue(filename)} (start: {new_start_scan} end: {new_end_scan}) - block cannot end before it starts",
      call = .env
    )
  }

  # summarize what needs to happen
  change_start <- new_start_scan != old_start_scan
  change_end <- new_end_scan != old_end_scan
  if (!change_start && !change_end) {
    return(list(
      single_scans = single_scans,
      changed = FALSE,
      details = character(0)
    ))
  }
  # actualize changes
  updated_file_scans <- file_scans |>
    dplyr::mutate(
      # update segment
      segment = NA_integer_,
      # update data type
      data_type = dplyr::case_when(
        # new data range
        .data$scan.no >= new_start_scan & .data$scan.no <= new_end_scan ~
          orbi_get_option("data_type_data"),
        # previous data that's now unused
        .data$data_type == orbi_get_option("data_type_data") &
          .data$scan.no >= old_start_scan &
          .data$scan.no <= old_end_scan ~
          orbi_get_option("data_type_unused"),
        # unchanged
        TRUE ~ .data$data_type
      ),
      # update block
      block = ifelse(
        .data$scan.no >= new_start_scan & .data$scan.no <= new_end_scan,
        !!block,
        .data$block
      ),
      # update block name
      block_name = ifelse(
        .data$scan.no >= new_start_scan & .data$scan.no <= new_end_scan,
        old_start_row$block_name,
        .data$block_name
      )
    ) |>
    # determine data groups
    determine_data_groups()

  # which other blocks lost scans to this block and which ones are gone entirely
  # note: block 0 holds the scans that are not part of any block
  taken_from <- file_scans$block[
    file_scans$scan.no >= new_start_scan &
      file_scans$scan.no <= new_end_scan
  ] |>
    unique() |>
    setdiff(c(0L, block))
  removed_blocks <- setdiff(taken_from, updated_file_scans$block)
  shortened_blocks <- setdiff(taken_from, removed_blocks)

  # details of the changes
  label <- format_inline("block {block} in {cli::col_blue(filename)}")
  details <- c(
    if (change_start) {
      format_inline(
        "{label}: moved start from scan {old_start_scan} ({mins_to_text(old_start_time)}) to {new_start_scan} ({mins_to_text(new_start_time)})"
      )
    },
    if (change_end) {
      format_inline(
        "{label}: moved end from scan {old_end_scan} ({mins_to_text(old_end_time)}) to {new_end_scan} ({mins_to_text(new_end_time)})"
      )
    },
    purrr::map_chr(
      shortened_blocks,
      ~ if (.x < block) {
        format_inline("{label}: moved the end of block {.x} to the new start")
      } else {
        format_inline("{label}: moved the start of block {.x} to the new end")
      }
    ),
    if (length(removed_blocks) > 0) {
      format_inline(
        "{label}: removed {qty(length(removed_blocks))} block{?s} {removed_blocks} entirely as a result of the adjustment"
      )
    }
  )
  details <- paste0("\u00a0", format_inline("{symbol$arrow_right} "), details)

  # combine with scans from the other files
  return(list(
    single_scans = dplyr::bind_rows(
      updated_file_scans,
      single_scans |> dplyr::filter(.data$filename != !!filename)
    ),
    changed = TRUE,
    details = details
  ))
}

# what a newly defined block covers in each file: the scans/times it actually spans
# (`n_scans` is 0 for files where the block falls outside the recorded data) plus
# the range the file itself covers, for reporting
summarize_block_coverage <- function(single_scans) {
  file_cols <- intersect(c("uidx", "filename"), names(single_scans))
  has_time <- "time.min" %in% names(single_scans)
  in_block <- single_scans |>
    dplyr::filter(.data$new_block == .data$next_block)
  files <- single_scans |>
    dplyr::summarize(
      .by = dplyr::all_of(file_cols),
      file_start_scan = min(.data$scan.no),
      file_end_scan = max(.data$scan.no),
      file_start_time = if (has_time) min(.data$time.min) else NA_real_,
      file_end_time = if (has_time) max(.data$time.min) else NA_real_
    )
  # the block may not have landed in any file at all, in which case there is
  # nothing to summarize (dplyr would evaluate min()/max() on the empty frame)
  if (nrow(in_block) == 0L) {
    return(
      files |>
        dplyr::mutate(
          n_scans = 0L,
          start_scan = NA_integer_,
          end_scan = NA_integer_,
          start_time = NA_real_,
          end_time = NA_real_
        )
    )
  }
  in_block <- in_block |>
    dplyr::summarize(
      .by = dplyr::all_of(file_cols),
      n_scans = dplyr::n(),
      start_scan = min(.data$scan.no),
      end_scan = max(.data$scan.no),
      start_time = if (has_time) min(.data$time.min) else NA_real_,
      end_time = if (has_time) max(.data$time.min) else NA_real_
    )
  return(
    files |>
      dplyr::left_join(in_block, by = file_cols) |>
      dplyr::mutate(n_scans = dplyr::coalesce(.data$n_scans, 0L))
  )
}

# the label of each block ("block", "block <name>", "block <name> in <file>")
block_labels <- function(blocks_table) {
  # format each name on its own, format_inline() would collapse the whole vector
  name <- blocks_table$block_name |>
    purrr::map_chr(
      ~ if (is.na(.x)) "" else format_inline(" {.field {.x}}")
    )
  file <- blocks_table$in_filename |>
    purrr::map_chr(
      ~ if (is.na(.x)) "" else format_inline(" in {col_blue(.x)}")
    )
  return(sprintf("block%s%s", name, file))
}

# one line description of each block definition, used for warnings and errors
describe_blocks <- function(blocks_table) {
  where <- ifelse(
    !is.na(blocks_table$start_time.min),
    sprintf(
      "%s to %s",
      mins_to_text(blocks_table$start_time.min),
      mins_to_text(blocks_table$end_time.min)
    ),
    sprintf(
      "scan %s to %s",
      blocks_table$start_scan.no,
      blocks_table$end_scan.no
    )
  )
  return(sprintf("%s: %s", block_labels(blocks_table), where))
}

# one line summary of what a block ended up covering, used for the summary info
# note: this reports the scans/times actually covered rather than the ones requested,
# which can differ if the requested range extends beyond the recorded data
describe_block_coverage <- function(block_def, coverage) {
  label <- block_labels(block_def)
  # a block for a specific file already names it in the label
  for_one_file <- !is.na(block_def$in_filename)
  added <- coverage |> dplyr::filter(.data$n_scans > 0L)
  if (nrow(added) == 0L && for_one_file) {
    return(format_inline("{label}: not added, outside the data"))
  } else if (nrow(added) == 0L) {
    return(format_inline(
      "{label}: not added, outside the data in {nrow(coverage)} file{?s}"
    ))
  }
  covers <- format_inline(
    "scans {min(added$start_scan)} to {max(added$end_scan)}"
  )
  if (!all(is.na(added$start_time))) {
    covers <- format_inline(
      "{covers} ({mins_to_text(min(added$start_time))} to {mins_to_text(max(added$end_time))})"
    )
  }
  if (for_one_file) {
    return(format_inline("{label}: covers {covers}"))
  }
  return(format_inline(
    "{label}: covers {covers} in {nrow(added)} file{?s}"
  ))
}

# check if dataset has blocks
has_blocks <- function(dataset) {
  return(all(c("block", "block_name", "data_type") %in% names(dataset)))
}

# helper function to find blocks (internal)
# @param dataset tibble produced by [orbi_define_blocks_for_dual_inlet()]
# @param ref_block_time.min time where the signal is stable when reference is analyzed
# @param sample_block_time.min time where the signal is stable when sample is analyzed
# @param startup_time.min initial time to stabilize spray
find_blocks <- function(
  dataset,
  ref_block_time.min,
  sample_block_time.min = ref_block_time.min,
  startup_time.min = 0
) {
  # type checks
  ## dataset
  check_tibble(dataset, c("filename", "time.min"), .arg = "dataset")
  ## ref_block_time.min
  check_arg(
    ref_block_time.min,
    !missing(ref_block_time.min) &&
      rlang::is_scalar_double(ref_block_time.min) &&
      ref_block_time.min > 0,
    "must be a single positive number"
  )
  ## sample_block_time.min
  check_arg(
    sample_block_time.min,
    rlang::is_scalar_double(
      sample_block_time.min
    ) &&
      sample_block_time.min > 0,
    "must be a single positive number"
  )
  ## startup_time.min
  check_arg(
    startup_time.min,
    rlang::is_scalar_double(
      startup_time.min
    ) &&
      startup_time.min >= 0,
    "must be a single number (>=0)"
  )

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
        interval = 0L,
        idx = 0,
        start = 0,
        length = startup_time.min,
        last = FALSE
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

  # find blocks by uindx filename
  by_cols <- "filename"
  if ("uidx" %in% names(dataset)) {
    by_cols <- c("uidx", by_cols)
  }
  dataset |>
    dplyr::group_by(!!!purrr::map(by_cols, sym)) |>
    dplyr::summarize(
      min_time.min = min(.data$time.min),
      max_time.min = max(.data$time.min),
      intervals = list(find_file_blocks(
        .data$min_time.min,
        .data$max_time.min
      )),
      .groups = "drop"
    ) |>
    tidyr::unnest("intervals")
}

# general helper function to divide up time into intervals (internal)
# @param total_time which time should be divided up into intervals
# @param intervals how many intervals
find_intervals <- function(total_time, intervals) {
  # safety checks
  stopifnot(
    "`total_time` must a single number" = !missing(total_time) &&
      rlang::is_scalar_double(total_time),
    "`intervals` must be one or more numbers" = !missing(intervals) &&
      is.numeric(intervals) &&
      length(intervals) >= 1L
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
  if (is_empty(lengths)) {
    # no interval fits in
    lengths <- total_time
    idx <- 1L
  } else if ((remainder <- total_time - sum(lengths)) > 0) {
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
# @param scans must be scans from a single file
# @param time time stamp where the scan number is required
# @param which searching towards the start or end of the file
find_scan_from_time <- function(
  scans,
  time,
  which = c("start", "end"),
  allow_missing = FALSE,
  .env = caller_env()
) {
  time_scans <- scans |>
    dplyr::filter(
      (!!which == "start" & .data$time.min >= !!time) |
        (!!which == "end" & .data$time.min < !!time)
    ) |>
    dplyr::pull(.data$scan.no)
  time_scan <-
    if (which == "start") {
      utils::head(time_scans, 1)
    } else {
      utils::tail(time_scans, 1)
    }

  # safety check
  if (is_empty(time_scan)) {
    # the caller deals with the missing scan (e.g. by skipping the file)
    if (allow_missing) {
      return(NA_integer_)
    }
    # note: not every caller has the filename available (it can be a nesting key)
    file_info <- if ("filename" %in% names(scans)) {
      format_inline(" for file {cli::col_blue(scans$filename[1])}")
    } else {
      ""
    }
    cli_abort(
      c(
        "invalid {which} time ({mins_to_text(time)}){file_info}",
        "i" = "the time ranges from {mins_to_text(min(scans$time.min))} to {mins_to_text(max(scans$time.min))}"
      ),
      call = .env
    )
  }

  # return
  return(time_scan)
}

# pull out scan rows (and safety check along the way)
# @param scans must be scans from a single file
# @param scan scan number
get_scan_row <- function(scans, scan, .env = caller_env()) {
  if (!is_empty(scan)) {
    scan_row <- scans |>
      dplyr::filter(.data$scan.no == !!scan)

    if (nrow(scan_row) == 1L) {
      return(scan_row)
    } else if (nrow(scan_row) > 1L) {
      sprintf(
        "scan# %s not unique! should never get to this with only one file! %s",
        scan,
        paste(unique(scans$filename), collapse = ", ")
      ) |>
        rlang::abort(call = .env)
    }
  }
  # scan not found --> error
  sprintf(
    "file '%s' does not contain scan# %s - the scans range from %s to %s",
    scans$filename[1],
    scan,
    min(scans$scan.no),
    max(scans$scan.no)
  ) |>
    rlang::abort()
}

# internal function to determine data groups
# @param dataset assumes is in the correct order
determine_data_groups <- function(dataset) {
  dataset |>
    # assign data groups
    dplyr::mutate(
      .by = dplyr::any_of(c("uidx", "filename")),
      .grouping = paste(.data$block, .data$segment, .data$data_type) |>
        factor_in_order() |>
        as.integer(),
      data_group = cumsum(c(0L, diff(.data$.grouping)) != 0) + 1L
    ) |>
    dplyr::select(-".grouping")
}

# internal function to segment into a specific number of segments
# @param scan.no where the segmentation starts
# @param into_segments how many segments
segment_by_segs <- function(scan.no, into_segments) {
  idx <- seq_along(scan.no)
  if (into_segments >= length(scan.no)) {
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
# @param scan.no where the segmentation starts
# @param by_scans number of scans in each segment
segment_by_scans <- function(scan.no, by_scans) {
  # approximates number of scans
  return(segment_by_segs(
    scan.no,
    into_segments = round(length(scan.no) / by_scans)
  ))
}

# internal function to segment by time interval
# @param time.min where the segmentation starts
# @param time_interval length of each segment in minutes
segment_by_time_interval <- function(time.min, time_interval) {
  return(as.integer((time.min - min(time.min)) %/% time_interval + 1L))
}
