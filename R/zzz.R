# on package load
.onLoad <- function(libname, pkgname) {
  # if we're knitting, enable full ansi output (turn off with orbi_options(auto_use_ansi = FALSE))
  if (
    orbi_get_option("auto_use_ansi") &&
      requireNamespace("knitr", quietly = TRUE) &&
      requireNamespace("fansi", quietly = TRUE)
  ) {
    # are we in the process of knitting html?
    if (knitr::is_html_output()) {
      options(cli.num_colors = 256)
      capture.output(fansi::set_knit_hooks(
        knitr::knit_hooks,
        which = c('output', 'warning', 'error', 'message')
      ))
    }
  }
}
