#' Title
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
tri_dat_check <- function(dat) {
  cols <- colnames(dat)

  args <- c("result_id", "study", "type", "yi", "vi")

  for (x in args) {
    stop_fn(x,cols = cols)
  }

  message("Looks good!\nAll expected columns are present in the dataset.")
}

stop_fn <- function(x, cols = cols){
  if (!(x %in% cols)) {
    stop(paste0("Column '",x,"' is missing from dataset"))
  }
}
