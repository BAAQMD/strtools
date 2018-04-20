#' formatID
#'
#' @note For use with `DT` package
#' @note Exploits the (private!) `DT:::formatColumns` function
#' @seealso formatCommas
#'
#' @export
formatID <- function (table, columns, prefix = "#") {

  require(DT)

  tplPrefix <- function (cols, prefix) {
    JS_template <- "var d = parseInt(data[%d]); $('td:eq(%d)', row).html(isNaN(d) ? '' : '%s' + (d).toString());"
    sprintf(JS_template, cols, cols, prefix)
  }

  DT:::formatColumns(table, columns, tplPrefix, prefix)

}
