#' Format table columns
#'
#' @note For use with `DT` package
#' @seealso formatID
#' @importFrom DT formatCurrency
#'
#' @export
formatCommas <- function (table, columns) {

  DT::formatCurrency(table, columns, currency = "")

}
