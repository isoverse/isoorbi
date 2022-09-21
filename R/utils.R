# Common utility functions

#' @title Remove rare isotopocules
#' @description Remove isotopocules that are not consistently detected across scans
#'
#' @param data Simplified IsoX data to be processed
#' @param min_percent Set threshold. Isotopocule must be observed in at least  x percent of scans
#'
#' @return Returns data frame with inconsistent isotopocules removed
#' @export
remove.rare <-
  function(data, min_percent) {
    remove.df <- data %>%
      dplyr::group_by(.data$filename) %>%
      dplyr::mutate(n.scans = max(.data$scan.no) - min(.data$scan.no)) %>% #FIXME: implement a more general solution here
      dplyr::group_by(.data$filename, .data$compound, .data$isotopolog) %>%
      dplyr::mutate(i.scans = length(.data$scan.no)) %>%
      dplyr::filter(.data$i.scans < min_percent / 100 * .data$n.scans) %>% # => update selection in GUI?, add message? used previously `input$rare`
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
