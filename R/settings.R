#' Set package settings
#'
#' Use this function to change the default package settings. When calling this function, only specify the settings you want to change, everything else will remain unchanged. The default value for each parameter is what the package uses by default for each setting.
#'
#' FIXME: needs documentation completion
#' FIXME: needs tests to change settings and then get the value back
#'
#' @param di_ref_name the text label for dual inlet reference blocks
#' @param di_sample_name the text label for dual inlet sample blocks
#' @param reset_all if set to TRUE, will reset all settings back to their defaults
#' @return invisible list of all settings (see \link{orbi_get_settings()})
#' @export
orbi_set_settings <- function(
    di_ref_name = "ref",
    di_sample_name = "sam",
    data_type_data =  "data",
    data_type_startup = "startup",
    data_type_changeover = "changeover",
    data_type_unused = "unused",
    reset_all = FALSE
) {

  # set settings that user supplies
  if (!missing(di_ref_name) | reset_all)
    set_setting("di_ref_name", di_ref_name)
  if (!missing(di_sample_name)| reset_all)
    set_setting("di_sample_name", di_sample_name)
  if (!missing(data_type_data)| reset_all)
    set_setting("data_type_data", data_type_data)
  if (!missing(data_type_startup)| reset_all)
    set_setting("data_type_startup", data_type_startup)
  if (!missing(data_type_changeover)| reset_all)
    set_setting("data_type_changeover", data_type_changeover)
  if (!missing(data_type_unused)| reset_all)
    set_setting("data_type_unused", data_type_unused)

  # return all settings invisibly
  return(invisible(orbi_get_settings()))
}

#' Get all isoorbi package settings
#' @param pattern an optional parameter with a regular expression pattern by which to sub-select the returned settings
#' @return list of all package settings and their values
#' @examples
#' orbi_get_settings()
#' @export
orbi_get_settings <- function(pattern = NULL) {
  all_opts <- options()
  pkg_pattern <- sprintf("^%s", get_pkg_settings_prefix())
  pkg_options <- all_opts[grepl(pkg_pattern, names(all_opts))]
  pkg_options <- pkg_options |> setNames(gsub(pkg_pattern, "", names(pkg_options)))
  if (!is.null(pattern)) {
    pkg_options <- pkg_options[grepl(pattern, names(pkg_options))]
  }
  return(pkg_options)
}

# initialize default settings on package loading (internal function)
# this is run once when the first package function is called (even with :::)
.onLoad <- function(libname, pkgname) {
  orbi_set_settings(reset_all = TRUE)
}

# package settings prefix
get_pkg_settings_prefix <- function() {
  "isoorbi."
}

# retrieve package settings (internal function)
setting <- function(name) {

  # check R options
  value <- getOption(paste0(get_pkg_settings_prefix(), name))
  if (is.null(value)) {
    # setting doesn't exist
    stop("isoorbi setting '", name, "' does not exist", call. = FALSE)
  }

  # return default value
  return(value)
}

# set package setting (internal function)
set_setting <- function(name, value) {
  options(list(value) |> setNames(paste0(get_pkg_settings_prefix(), name)))
  return(invisible(value))
}
