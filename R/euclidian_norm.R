#' N-Norm of a vector
#'
#' Calculates the  n-norm of a vector
#'
#' @param v must be a numerical vector
#' @param n must be an integer. Defaults to 2.
#' @return The vector's norm
#'
#'
#' @examples
#'
#' v = rnorm(n = 200)
#' euclidian_norm(v)]
#' @importFrom pracma nthroot
#' @export

norm <- function(v, n = 2) {

  s = sum(x^n)
  norm = pracma::nthroot(s, n = n)

  return(norm)

}

