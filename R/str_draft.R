#' str_draft
#'
#' Outputs "DRAFT" and the current date. #'
#' If in a git repo, also appends the first 7 characters of the latest commit.
#'
#' Useful for watermarking figures, tables, etc.
#'
#' @param ... (optional) appended with [stringr::str_c()].
#'
#' @importFrom stringr str_glue str_sub str_c
#' @importFrom git2r last_commit
#'
#' @export
str_draft <- function (...) {

  stamp <- str_glue("DRAFT {str_date()}")

  try({
    last_commit <- git2r::last_commit()
    commit_id <- str_sub(last_commit$sha, 1, 7)
    stamp <- str_c(stamp, str_glue(" [{commit_id]}]"))
  }, silent = TRUE)

  stamp <- str_c(stamp, ...)

  return(stamp)

}
