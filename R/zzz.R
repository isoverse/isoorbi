# on package load
.onAttach <- function(libname, pkgname) {
  # register default aggregator
  orbi_start_aggregator("default") |>
    orbi_add_to_aggregator(
      "file_info",
      "filename",
      source = "FileName",
      func = "sub",
      args = list(pattern = ".raw", replacement = "", fixed = TRUE)
    ) |>
    orbi_add_to_aggregator(
      "file_info",
      "\\1",
      source = "(.*)",
      regexp = TRUE
    ) |>
    orbi_add_to_aggregator("scans", "scan.no", cast = "as.integer") |>
    orbi_add_to_aggregator(
      "scans",
      "time.min",
      source = "StartTime",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "tic",
      source = "TIC",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "it.ms",
      source = "Ion Injection Time (ms)",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "resolution",
      source = c("FT Resolution", "Orbitrap Resolution"),
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "basePeakIntensity",
      source = "BasePeakIntensity",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "rawOvFtT",
      source = "RawOvFtT",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "intensCompFactor",
      source = "OT Intens Comp Factor",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator("scans", "agc", source = "AGC") |>
    orbi_add_to_aggregator(
      "scans",
      "agcTarget",
      source = "AGC Target",
      cast = "as.integer"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "microscans",
      source = "Micro Scan Count",
      cast = "as.integer"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "numberLockmassesFound",
      source = "Number of LM Found",
      cast = "as.integer"
    ) |>
    orbi_add_to_aggregator(
      "scans",
      "analyzerTemperature",
      source = "Analyzer Temperature",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator("peaks", "scan.no", cast = "as.integer") |>
    orbi_add_to_aggregator(
      "peaks",
      "mzMeasured",
      "mass",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator("peaks", "intensity", cast = "as.numeric") |>
    orbi_add_to_aggregator("peaks", "baseline", cast = "as.numeric") |>
    orbi_add_to_aggregator(
      "peaks",
      "peakNoise",
      "noise",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "peaks",
      "peakResolution",
      "resolution",
      cast = "as.numeric"
    ) |>
    orbi_add_to_aggregator(
      "peaks",
      "isRefPeak",
      "is_ref",
      cast = "as.logical"
    ) |>
    orbi_add_to_aggregator(
      "peaks",
      "isLockPeak",
      "is_lock_peak",
      cast = "as.logical"
    ) |>
    orbi_add_to_aggregator("spectra", "scan.no", cast = "as.integer") |>
    orbi_add_to_aggregator("spectra", "mz", "mass", cast = "as.numeric") |>
    orbi_add_to_aggregator("spectra", "intensity", cast = "as.numeric") |>
    orbi_register_aggregator()

  # if we're knitting, enable full ansi output (turn off with orbi_options(auto_use_ansi = FALSE))
  if (
    orbi_get_option("auto_use_ansi") &&
      requireNamespace("knitr", quietly = TRUE) &&
      requireNamespace("fansi", quietly = TRUE)
  ) {
    # are we in the process of knitting html?
    if (knitr::is_html_output()) {
      options(cli.num_colors = 256)
      utils::capture.output(fansi::set_knit_hooks(
        knitr::knit_hooks,
        which = c('output', 'warning', 'error', 'message')
      ))
    }
  }
}
