#' Gram-Schmidt Process
#'
#' Applies the Gram-Schimdt Process to a set of vectors
#'
#' @param X a matrix of numerical vectors
#' @param type Can be either "orthonormal" or "orthogonal". If "Orthonormal" then resulting vectores are all normalized to the standard basis.
#'
#' @return The resulting matrix
#'
#'
#' @examples
#'
#' v = rnorm(n = 200)
#' A = matrix(v, ncol = 5)
#'
#' B = GS_process(A)
#'
#' @export



 GS_process <- function(X, type = "orthogonal") {


   if (is.matrix(X) != TRUE) {

     stop("X must be a matrix")
   }

   d = nrow(X)
   A = X

   for(i in 2:col(X)) {

     A[,i] = ((sum(A[,1]*A[,i]))/sum(A[,1]*A[,1]))*A[,1]

   }

   if(type == "orthonormal") {

     for(i in 1:col(A)) {

       A[,i] = euclidian_norm(A[,i])

     }
   }


   return(A)


 }
