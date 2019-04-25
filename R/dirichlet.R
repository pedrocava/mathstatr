#' Dirichlet Energy
#'
#' Calculates the Dirichlet Energy of a data vector.
#'
#' @param x data. Vecotrs
#' @param standartize Logical. If set to TRUE, then final result is divided by the the amount of entries. Defaults to FALSE.
#'
#' @return The signal's Dirichlet Energy. If x is a data frame, then the output is a vector.
#'
#' @examples
#'
#' a <- seq(1, 100)
#' dirichlet_energy(a)
#'
#' @export


dirichlet_energy <- function(x, standartize = FALSE) {

  if(standartize == TRUE) {

    d = length(x)

  }

  else {

   d = 1

  }


### if x is a data.frame --------------------

  if(is.data.frame(x) == TRUE) {

    mid = vector()
    energy = vector()

    for(j in 1:ncol(x)) {

      for(i in 1:(length(x)-1)) {

      mid[i] = (x[i,j] - x[i+1,j])^2

      energy[j] = sum(mid)/d

      }
    }

    names(energy) = names(x)

    return(energy)

    }

##### if x is a vector ---------------------

  mid = vector()

  for(i in 1:(length(x)-1)) {

    mid[i] = (x[i] - x[i+1])^2

  }

  energy = sum(mid)/d

  return(energy)

}


