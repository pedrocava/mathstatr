#' Matrix of operations
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'




foo_matrix <- function(x, y, FUN = "sum") {

  if(length(x) != length(y)) {Return("X and Y must have equal lengths")}

  FUN <- purrr::partial(.f = as.function(FUN))



  table = table(x, y) %>% matrix(nrow = length(x))

  for(i in 1:length(x)){




  }
}
