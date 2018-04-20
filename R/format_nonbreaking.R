#' Non-breaking spaces and hyphens
#'
#' @export
format_nonbreaking <- function (x) {
  nbsp <- str_replace_all(x, " ", "\u00A0") # non-breaking spaces
  str_replace_all(nbsp, "-", "\u2011")      # non-breaking hyphens
}
