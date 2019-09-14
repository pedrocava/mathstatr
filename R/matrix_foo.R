# #' Functional Matrix
# #'
# #'  Given 2 vectors ``x`` and ``y``, ``foo_matrix()`` applies a function to all pairs of elements and outputs a matrix with its results.
# #'
# #' @param x a vector
# #' @param y a vector
# #' @param FUN function to be applied to all pairs x-y, defaults to ``sum()``
# #'
# #' @import purrr
# #' @import dplyr
# #' @import tibble
# #'
# #'
#
#
#
#
# foo_matrix <- function(x, y, FUN = sum()) {
#
#   if(length(x) != length(y)) {warning("Resulting matrix won't be squared as vectors have different lengths")}
#
#   fun <- purrr::partial(FUN)
#
#   matrix <- matrix(NA, nrow = length(x), ncol = length(y))
#
#
#   for(i in 1:length(x)){
#
#     for(j in 1:length(y)) {
#
#
#       matrix[i,j] = fun(x[i], y[j])
#
#
#     }
#   }
# }
