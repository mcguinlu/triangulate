#' Check that required columns are present
#'
#' Validates whether key variables are present in the dataset.
#' Can toggle between a basic ("minimal") check or a "full" check including columns needed for bias adjustment.
#'
#' @param dat A data frame (long or wide format)
#' @param mode Check mode: either "minimal" or "full"
#'
#' @return Throws an error if required columns are missing. Otherwise returns TRUE (invisibly).
#' @export
tri_dat_check <- function(dat, mode = c("minimal", "full")) {
  mode <- match.arg(mode)

  required_cols <- switch(
    mode,
    minimal = c("result_id", "study", "type", "yi", "vi"),
    full = c("result_id", "study", "type", "yi", "vi", "domain", "j", "d", "t")
  )

  missing_cols <- setdiff(required_cols, colnames(dat))

  if (length(missing_cols) > 0) {
    stop("tri_dat_check(): The following required column(s) are missing: ",
         paste(missing_cols, collapse = ", "))
  }

  message("tri_dat_check(): All expected columns are present (mode = '", mode, "').")
  invisible(TRUE)
}

