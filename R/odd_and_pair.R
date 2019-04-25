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

  result = vector(length = length(x))

  for(i in 1:length(x)) {

    result[i] = ifelse(x[i] %% 2 == 1, TRUE, FALSE)

  }

  return(result)

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

  return(is_odd(x))

}
