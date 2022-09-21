# Common core logic functions

#' @title Simplify IsoX output
#' @description Keep only columns that are essential for isotopocule ratio analysis
#'
#' @param dataset The loaded IsoX data that is to be simplified
#'
#' @return A simplified data frame with the columns: 'filename', 'scan.no', 'time.min', 'compound', 'isotopolog', 'ions.incremental', 'tic', 'it.ms'.
#'
#' @examples
#' fpath <- system.file("extdata", "testfile_Flow_Exploration_small.isox", package="isoorbi")
#' df <- iso_read_isox_file(filepath = fpath) %>% iso_simplify_isox_file()
#'
#' @export

iso_simplify_isox_file <- function(dataset) {
  df.out <- dataset %>% dplyr::select(.data$filename,
                                      .data$scan.no,
                                      .data$time.min,
                                      .data$compound,
                                      .data$isotopolog,
                                      .data$ions.incremental,
                                      .data$tic,
                                      .data$it.ms)
}


#' @title Remove rare isotopocules
#' @description Remove isotopocules that are not consistently detected across scans
#'
#' @param data Simplified IsoX data to be processed
#' @param min.percent Set threshold. Isotopocule must be observed in at least  x percent of scans
#'
#' @return Returns data frame with inconsistent isotopocules removed
#' @export
remove.rare <-
  function(data, min.percent) {
    remove.df <- data %>%
      dplyr::group_by(.data$filename) %>%
      dplyr::mutate(n.scans = max(.data$scan.no) - min(.data$scan.no)) %>% #FIXME: implement a more general solution here
      dplyr::group_by(.data$filename, .data$compound, .data$isotopolog) %>%
      dplyr::mutate(i.scans = length(.data$scan.no)) %>%
      dplyr::filter(.data$i.scans < min.percent / 100 * .data$n.scans) %>% # => update selection in GUI?, add message? used previously `input$rare`
      dplyr::select(-.data$n.scans, -.data$i.scans) %>% droplevels() %>% as.data.frame()


    df <-
      anti_join(
        data,
        remove.df,
        by = c(
          "filename",
          "scan.no",
          "time.min",
          "compound",
          "isotopolog",
          "ions.incremental",
          "tic",
          "it.ms"
        )
      ) #FIXME:More elegant/robust solution?

    return(df)

  }
