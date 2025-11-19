# Plot mass spectra

This function visualizes mass spectra from aggregated raw file data. The
spectra have to be be previously read in with
`include_spectra = c(1, 10, 100)` in
[`orbi_read_raw()`](https://isoorbi.isoverse.org/reference/orbi_read_raw.md).
By default, this function tries to visualize different isotopcule ranges
(monoisotopic peak, M+1, M+2, M+3). To focus only on isotopcules of
interest, run
[`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md)
and
[`orbi_filter_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.md)
first.

## Usage

``` r
orbi_plot_spectra(
  aggregated_data,
  mz_min = 0,
  mz_max = Inf,
  mz_base_peak = NULL,
  mz_focus_nominal_offsets = 0:4,
  max_scans = 6,
  max_files = 4,
  label_peaks = TRUE,
  show_filenames = TRUE,
  show_ref_and_lock_peaks = TRUE,
  show_focus_backgrounds = TRUE,
  background_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
    "#A6761D", "#666666", "#BBBBBB")
)
```

## Arguments

- aggregated_data:

  data aggregated by
  [`orbi_aggregate_raw()`](https://isoorbi.isoverse.org/reference/orbi_aggregate_raw.md)
  and, optionally, with isotopocules already identified by
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md),
  and (also optionally), alreadty filtered with
  [`orbi_filter_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.md)

- mz_min:

  which mz to start the main plot window at. By default include all.

- mz_max:

  which mz to end the main plot window at. By default include all.

- mz_base_peak:

  where is the base peak at (approximately)?. If not specified (the
  default), takes the largest peak in the `mz_min` to `mz_max` window.

- mz_focus_nominal_offsets:

  which panels to visualize? 0 = whole spectrum, 1 = spectrum around
  monoisotopic peak + 1 mu (M+1), 2 = M+2, etc. By default includes the
  whole spectrum and up to +1, +2, +3, and +4 peaks (if they exist). To
  visualize only the whole spectrum, use `mz_focus_nominal_offsets = 0`.
  Likewise, to visualize only the area around the monoisotopic peak +1,
  provide `mz_focus_monimal_offsets = 1` (or `= c(1, 2)` for both +1 and
  +2 windows).

- max_scans:

  spectra from how many scans to show at most. By default up to 6 (the
  number of available linetypes). To show only the spectrum from a
  single scan, set `max_scans = 1`. If more than 6 scan spectra are
  allowed (and there are more than 6 loaded in the `aggregated_data`),
  turns of the linetype aesthetic.

- max_files:

  spectra from how many files to show at most. Each file is shown as an
  additional line of panels.

- label_peaks:

  whether to label the peaks in the M+1/2/3 panels. If isotopcules are
  already identified from
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md),
  uses the isotopcule names, otherwise the m/z values. Peaks that are
  missing (identified by
  [`orbi_identify_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_identify_isotopocules.md))
  in all spectra are highlighted in red. To avoid labeling
  unidentified/missing peaks, run
  [`orbi_filter_isotopocules()`](https://isoorbi.isoverse.org/reference/orbi_filter_isotopocules.md)
  first.

- show_filenames:

  whether to show the filename in the first panel of reach row (usually
  the full spectrum panel)

- show_ref_and_lock_peaks:

  whether to show reference and lock mass peaks in the spectrum

- show_focus_backgrounds:

  whether to highlight the M+x panels with specific background colors
  that match them with the mass bands highlighted in the first panel

- background_colors:

  the colors to use for the background highlighting
