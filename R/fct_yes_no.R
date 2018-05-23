#' @export
fct_yes_no <- function (...) {
  factor(..., levels = c(FALSE, TRUE), labels = c("No", "Yes"))
}
