#' str_hash
#'
#' Hash one or more characters, element-wise and in parallel.
#'
#' @param ... one or more character vectors (must be same length)
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_c
#' @importFrom digest digest
#'
#' @export
str_hash <- function (
  ...,
  algo = c(
    "md5", "sha1", "crc32", "sha256", "sha512",
    "xxhash32", "xxhash64", "murmur32", "spookyhash",
    "blake3"),
  serialize = FALSE
) {

  algo <- match.arg(algo)
  x <- stringr::str_c(...)
  purrr::map_chr(x, digest::digest, algo = algo, serialize = serialize)
}

#' @rdname str_hash
str_crc32 <- function (...) {
  str_hash(..., algo = "crc32")
}

#' @rdname str_hash
str_xxhash32 <- function (...) {
  str_hash(..., algo = "xxhash32")
}

#' @rdname str_hash
str_xxhash64 <- function (...) {
  str_hash(..., algo = "xxhash64")
}

#' @rdname str_hash
str_md5 <- function (...) {
  str_hash(..., algo = "md5")
}
