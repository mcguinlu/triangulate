#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.

# --- Imports for functions used across the package ---
# General
#' @importFrom magrittr %>%
#' @importFrom graphics par
#' @importFrom stats offset

# From packages flagged in warnings:
#' @importFrom metafor rma addpoly forest
#' @importFrom purrr map
#' @importFrom scales alpha

NULL

