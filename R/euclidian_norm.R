#' Euclidian Norm
#'
#' Calculates the euclidian norm of a vector
#'
#' @param v must be a numerical vector
#'
#' @return The vector's norm
#'
#'
#' @examples
#'
#' v = rnorm(n = 200)
#' euclidian_norm(v)
#'
#' @export


euclidian_norm <- function(v) {

  soma = sum(x^2)
  total = sqrt(soma)

  return(total)

}

