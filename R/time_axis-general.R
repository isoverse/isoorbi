get_timebase <- function(interval, cutoff) {
  tb <- c(
    "years" = 365 * 86400,
    "months" = 30 * 86400,
    "weeks" = 7 * 86400,
    "days" = 86400,
    "h" = 3600,
    "m" = 60,
    "s" = 1,
    "ms" = 1e-3,
    "\u00b5s" = 1e-6,
    "ns" = 1e-9,
    "ps" = 1e-12,
    "fs" = 1e-15
  )
  base <- tb[interval >= cutoff * tb][1]
  if (is.na(base)) {
    base <- tail(tb, 1)
  }
  return(base)
}

# generates a pretty breaks duration function that expects seconds
breaks_pretty_duration <- function(n = 5, cutoff = 0.5) {
  # pretty breaks
  function(secs) {
    interval <- pretty(secs, n) |> diff() |> min(na.rm = TRUE)
    base <- interval |> get_timebase(cutoff)
    breaks <- pretty(secs / base, n) * base
    return(breaks)
  }
}

# generates a labels duration function that expects seconds
labels_duration <- function(cutoff = 0.5, short_format = FALSE) {
  sep <- if (short_format) "" else " "
  unit <- function(long, short) if (short_format) short else long
  # smallest number of decimals (capped at 9 = nanoseconds) that represents a
  # sub-unit `interval` to within rounding, i.e. enough to tell consecutive
  # breaks apart (0 for intervals >= 1). A relative tolerance both stops early on
  # round intervals and absorbs the floating-point cancellation noise produced by
  # diff() of large values (e.g. 180.000001 - 180 is not exactly 1e-6).
  decimals_for <- function(interval) {
    if (!is.finite(interval) || interval <= 0 || interval >= 1) {
      return(0L)
    }
    d <- 0L
    while (d < 9L && abs(round(interval, d) - interval) > 1e-6 * interval) {
      d <- d + 1L
    }
    d
  }
  # pretty labels
  function(secs) {
    interval <- suppressWarnings(min(diff(secs), na.rm = TRUE))
    base <- interval |> get_timebase(cutoff)
    max_abs <- suppressWarnings(max(abs(secs), na.rm = TRUE))

    # Use the HH:MM:SS style whenever the resolution is at least 1 s OR the
    # values themselves reach a full second. This keeps large times in their
    # m:s / h:m:s format and merely adds fractional seconds for sub-second
    # break spacing (e.g. 3:00.001 min) instead of collapsing the whole value
    # into a huge millisecond/microsecond number that loses all precision.
    use_hms <- names(base) %in%
      c("h", "m", "s") ||
      (is.finite(max_abs) && max_abs >= 1)

    if (use_hms) {
      # fractional-second decimals for sub-second break spacing
      dec <- decimals_for(interval)
      h <- secs %/% 3600
      m <- secs %/% 60 %% 60
      s <- secs %% 60
      has_h <- any(h > 0, na.rm = TRUE)
      has_m <- any(m > 0, na.rm = TRUE)
      has_s <- dec > 0 || any(s > 0, na.rm = TRUE)

      # seconds field: `pad = TRUE` zero-pads to two integer digits (when it
      # trails an h/m component); `dec` fractional digits are appended when there
      # are sub-second differences to show
      fmt_secs <- function(pad) {
        if (dec > 0) {
          formatC(
            s,
            format = "f",
            digits = dec,
            width = if (pad) dec + 3L else 0L,
            flag = "0"
          )
        } else if (pad) {
          sprintf("%02.0f", s)
        } else {
          format(signif(s, 3), scientific = FALSE)
        }
      }

      if (has_h) {
        label <- unit("hours", "hr")
        if (has_s) {
          # HH:MM:SS(.sss)
          out <- paste0(sprintf("%.0f:%02.0f:", h, m), fmt_secs(TRUE))
        } else if (has_m) {
          out <- sprintf("%.0f:%02.0f", h, m) # HH:MM
        } else {
          out <- sprintf("%.0f", h) # HH
        }
      } else if (has_m) {
        label <- unit("min", "m")
        if (has_s) {
          out <- paste0(sprintf("%.0f:", m), fmt_secs(TRUE)) # MM:SS(.sss)
        } else {
          out <- sprintf("%.0f", m) # MM
        }
      } else {
        label <- unit("secs", "s")
        out <- fmt_secs(FALSE) # SS(.sss)
      }
      out <- paste0(out, sep, label)
    } else {
      # genuine sub-second domain (all values < 1 s): show the value in the
      # chosen unit (ms, \u00b5s, ns, ...) with enough decimals to tell breaks apart
      dec_sub <- decimals_for(interval / base)
      out <- paste0(
        formatC(secs / base, format = "f", digits = dec_sub),
        sep,
        names(base)
      )
    }
    out[is.na(secs)] <- NA
    return(out)
  }
}
