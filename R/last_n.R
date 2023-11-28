
#' Last n letters
#'
#' @param s character vector.
#' @param n integer.
#'
#' @return character vector. The elements of the result vector are last n letters
#'  of input vector s.
#' @export
#'
#' @examples
#' last_n(c("abc", "wxyz"), 2)
last_n <- function(s, n) {
  stop <- nchar(s)
  start <- stop - n + 1
  substr(s, start, stop)
}
