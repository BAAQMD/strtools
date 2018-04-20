#' Unpack a vector of "packed integers" (like facility IDs or category IDs)
#'
#' @examples
#' input_data <- data_frame(cat_ids = c("c(1, 3)", "c(1:4)"))
#' unpack_integers(input_data, var_name = "cat_ids")
#' unpack_integers(input_data)
#'
#' @export
unpack_integers <- function (input_data, var_name, ..., verbose = getOption("verbose"))  {

  msg <- function (...) if(isTRUE(verbose)) message("[unpack_integers] ", ...)

  msg("unpacking ", var_name)
  parsed <- mutate_at(input_data, vars(var_name), funs(parse_integers), ...)
  unpacked <- unnest_(parsed, var_name)

  return(unpacked)

}
