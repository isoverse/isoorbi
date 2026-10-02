# pretty formatting ===========

# convert bytes to pretty text
bytes_to_text <- function(bytes) {
  bytes |>
    purrr::map_chr(
      ~ if (is.na(.x)) {
        return(NA_character_)
      } else {
        .x |> structure(class = "object_size") |> format(units = "auto")
      }
    )
}

# predefined metric prefixes for number formatting
.metric_prefixes <- set_names(
  c(1e-15, 1e-12, 1e-9, 1e-6, 1e-3, 1, 1e3, 1e6, 1e9, 1e12),
  c("f", "p", "n", "\u00b5", "m", "", "k", "M", "G", "T")
)

# convert numbers to pretty text
numbers_to_text <- function(
  x,
  signif = 3,
  format = "%s%s",
  consider_zero = 1e-18,
  trim_ws = TRUE
) {
  stopifnot(!missing(x) && is.numeric(x))
  if (is_empty(x)) {
    return(character(0))
  }
  # best prefix
  prefix <- x |>
    abs() |>
    purrr::map_int(
      ~ max(1, which(.x / .metric_prefixes >= 1))
    )

  # deal with zeros
  is_zero <- abs(x) < consider_zero
  x[is_zero] <- 0
  prefix[is_zero] <- which(names(.metric_prefixes) == "")

  # convert
  out <- signif(
    x / .metric_prefixes[prefix],
    digits = signif
  ) |>
    sprintf(fmt = format, names(.metric_prefixes)[prefix])

  # special cases?
  is_special <- is.na(x) | is.infinite(x) | is.nan(x)
  out[is_special] <- as.character(x[is_special])

  # trim whitespaces?
  if (trim_ws) {
    out <- trimws(out)
  }

  # return
  return(out)
}

# convert seconds to pretty time
secs_to_text <- function(secs) {
  stopifnot(is.numeric(secs))
  out <- tibble(
    idx = seq_along(secs),
    d = floor(secs / 86400),
    h = floor((secs / 3600) %% 24),
    m = floor((secs / 60) %% 60),
    s = round(secs %% 60, 1),
    ms = round((secs * 1000L) %% 1000),
    small = secs < 1
  ) |>
    tidyr::pivot_longer(cols = -c("idx", "small")) |>
    dplyr::filter(
      is.na(.data$value) |
        (.data$small & .data$name == "ms") |
        (!.data$small & .data$value > 0 & .data$name != "ms")
    ) |>
    dplyr::mutate(
      label = dplyr::if_else(
        !is.na(.data$value),
        paste0(.data$value, .data$name),
        NA_character_
      )
    ) |>
    dplyr::summarise(
      .by = "idx",
      out = dplyr::if_else(
        all(is.na(.data$label)),
        NA_character_,
        paste(.data$label[!is.na(.data$label)], collapse = " ")
      )
    ) |>
    dplyr::pull("out")

  # special cases (Inf, -Inf) pass through as-is
  is_inf <- is.infinite(secs)
  out[is_inf] <- as.character(secs[is_inf])
  return(out)
}

# convert minutes to pretty time
mins_to_text <- function(mins) {
  secs_to_text(mins * 60)
}
