#' Checking if number is odd
#'
#' Evaluates the oddness of a number.
#'
#' @param x vector of integers or a single integer.
#'
#' @return Logical, true if number is odd.
#'
#' @examples
#'
#' is_odd(rbinom(1, size = 2, prob = .5))
#'
#' @export

is_odd <- function(x) {
  as.logical(x %% 2)
}


#' Checking if number is even
#'
#' Evaluates the evenness of a number.
#'
#' @param x vector of integers or a single integer.
#'
#' @return Logical, true if number is integer.
#'
#' @examples
#'
#' is_even(rbinom(1, size = 2, prob = .5))
#'
#' @export


is_even <- function(x) {

  !is_odd(x)

}
