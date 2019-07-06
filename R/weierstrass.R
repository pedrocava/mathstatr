#' Weierstrass Function
#'
#' Evaluates the Weierstrass Function
#'
#' @param x value in which in function should be evaluated. Can be an integer, double or vector.
#' @param a parameter of the function. Must be positive and smaller than 1. Defaults to .5.
#' @param b parameter of the function. Must be positive and odd. Defaults to 5. For convergence it is necessary that ab be larger than (1 + (3/2)pi).
#' @param eps parameter for convergence of the calculation. Series is evaluated up until the marginal value is smaller than eps. Defaults to .001.
#' @param max parameter for convergence of the calculation. Maximum number of iterations. Defaults to 100.
#'
#' @return Output of the Weierstrass Function
#'
#' @examples
#'
#' a = seq(1, 100)
#' weierstrass(a)
#'
#' @export

weierstrass <- function(x,
                        a = .5,
                        b = 15,
                        eps = .01,
                        max = 1000
                        ) {

####### checking for conditions of convergence and some flow control --------------------

  if(((a < 0) | (a > 1)) == TRUE) {
    print("a must be positive and smaller than 1")
  }

  Atest = ifelse((a < 0) | (a > 1),
                 TRUE,
                 FALSE)
###

  if(is_odd(b) == FALSE) {
    print("b must be odd")
    }

###

  if(is_odd(b) == FALSE) {

    if(Atest == TRUE) {

      message("a must be smaller than 1")
      message("b must be odd")
      stop()

    }

    message("b must be odd")
    stop()

  }


  if(a*b < (1 + (3/2)*pi)) {

    text1 = paste("a =", a, " ---",
                  "b =", b, " ---",
                  "a*b=", a*b, " ---",
                  "(1 + (3/2)*pi)=", (1 + (3/2)*pi))
  print(text1)

  stop("a*b must be larger than (1 + (3/2)*pi) or else the series diverges")

    }

########### Calculating series of a^n * cos(b^n pi x) -----------------------


  ### if x is an integer or dbl


  if(is.vector(x) == FALSE) {

  f = vector()

  for(n in 1:max){

    a = ((a^n)*cos((b^n)*pi*x))

    if(a > eps) {

    f[n] = a

    fn = FALSE

    } else {

      fn = TRUE
    }


    if(fn == TRUE) break

    }

  f = sum(f)

  return(f)

  }
 else {

  vector = vector()

  for(i in 1:length(x)) {

  f = vector()

  for(n in 1:max){

    a = ((a^n)*cos((b^n)*pi*x[i]))

    if(a > eps) {

      f[n] = a

      fn = FALSE

    } else {

      fn = TRUE
    }

    if(fn == TRUE) break

  }

  vector[i] = sum(f)

  }

 }
}




