# Functions specific to dual inlet experiments-------------------------------------


# Define dual inlet experiment -------------------------------------------
#' @title Define dual inlet experimental settings and data processing parameters
#' @description This function defines the experiment and ensure input has required information
#'
#' @param reference Name of the solution in the reference syringe (default = "ref")
#' @param sample Name of the solution in the sample syringe (default = "smpl")
#' @param number.of.blocks Number of infusion blocks
#' @param switch.time Time needed for new solution to arrive after switching the valve
#' @param block.time Time duration of each infusion block in minutes
#' @param startup.time Optional: Time added in beginning for equilibration (in minutes, default = 0)
#' @param segments Optional: Each infusion block can be divided up into several segments (default = 1)
#'
#' @return Returns a data frame used to annotate data from dual inlet analysis
#' @export

orbi_dualInlet_define <-
  function(reference = "ref",
           sample = "smpl",
           number.of.blocks,
           switch.time,
           block.time,
           startup.time = 0,
           segments = 1) {


    # safety checks
    if (missing(number.of.blocks))
      stop("no value for the number of infusion blocks provided", call. = TRUE)
    if (missing(switch.time))
      stop("no value provided for time needed for new solution to arrive after switching the valve", call. = TRUE)
    if (missing(block.time))
      stop("no value provided for the duration of each infusion block", call. = TRUE)

    #basic checks
    if (!is.numeric(number.of.blocks))
      stop("number.of.blocks needs to be a number", call. = TRUE)

    blocks <-
      rep(c(reference, sample), 1E3)[1:number.of.blocks] #limited to maximum 1,000 blocks

    blocks <- rep(blocks, each = segments)

    segment.time <-
      (block.time - switch.time) / segments

    #define start times to integrate (in minutes)
    start.time <-
      0:(number.of.blocks - 1) * block.time + switch.time
    out.start <- c()

    for (i in 1:(length(start.time))) {
      a <-
        seq(start.time[i],
            start.time[i] + block.time - switch.time,
            segment.time)
      out.start  <- append(out.start, utils::head(a,-1))
    }

    start.time <- out.start + startup.time
    end.time <- start.time + segment.time

    #combine all information into an annotation table
    block.number <- 1:length(blocks)
    annotations <- data.frame(
      Block.Number = block.number,
      block = blocks,
      Start.Time = start.time,
      End.Time = end.time
    )
    return(annotations)
  }


# Function: orbi_dualInlet_annotate(data, annotations) ------------------

#' @title Merge dual inlet annotations with data
#' @description This function merges annotations between input data and dual inlet data
#'
#' @param data Filtered dual inlet data
#' @param annotations Annotation table provided by `orbi_dualInlet_define()`
#'
#' @return Returns an annotated data frame from dual inlet experiments
#' @export
orbi_dualInlet_annotate <- function(data, annotations){

  # safety checks
  if (missing(data))
    stop("no data provided", call. = TRUE)

  # basic checks
  if (!(is.data.frame(data)))
    stop("data need to be a R data frame", call. = TRUE)


  if (missing(annotations))
    stop("no annotations provided", call. = TRUE)





  df.data <- data
  df.data$sample_name <- "NA" #normal NA caused issues!
  df.data$block <- 0L  #normal NA caused issues! L makes the zero an integer, not double
  #
  for (i in 1:nrow(annotations)) {

    sample <- annotations$block[i]
    block.no <- annotations$Block.Number[i]

    time.start <- annotations$Start.Time[i]
    time.end <- annotations$End.Time[i]

    df.data[(df.data$time.min > time.start) & (df.data$time.min < time.end), ]$sample_name <- sample
    df.data[(df.data$time.min > time.start) & (df.data$time.min < time.end), ]$block <- block.no

  }


  df<- df.data  %>% dplyr::filter(.data$block >0) %>% dplyr::filter(.data$sample_name != "NA")

  df$sample_name <- as.factor(df$sample_name)
  df$compound <- as.factor(df$compound)
  df$isotopocule <- as.factor(df$isotopocule)

  return(df)

}
