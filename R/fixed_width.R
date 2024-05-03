#' To format numbers and strings. See help("format") for more information.
#'
#' @rdname fixed_width
#' @export
fixed_width <- function (x, width, ...) {
  # TODO: .Deprecated("str_fixed_width")
  UseMethod("fixed_width")
}

#' @rdname fixed_width
#' @export
fixed_width.character <- function (x, width, justify = "left", ...) {
  x[is.na(x)] <- ""
  x %>% 
    encodeString(width = width, justify = justify, ...) %>% 
    # add line below to account for x.length > width
    strtrim(width)
  
}

#' @rdname fixed_width
#' @export
fixed_width.numeric <- function (x, width, digits = 0, ...) {
  x %>%
    replace(!is.finite(.), NA) %>%
    round(digits = digits) %>%
    format(width = width, nsmall = digits, ...) %>%
    str_replace_all("NA", "") %>%
    fixed_width.character(width = width, justify = "right", ...)
}

#' @rdname fixed_width
#' @export
fixed_width.integer <- function (x, width, digits = 0, ...) {
  fixed_width.numeric(as.numeric(x), width = width, digits = digits, ...)
}

#' @rdname fixed_width
#' @export
fixed_width.default <- fixed_width.numeric
