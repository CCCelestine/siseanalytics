#' moyenne
#'
#' @param x a vector
#'
#' @return a number
#' @export
#' @importFrom stats na.omit
#' @import magrittr
#' @examples
#' moyenne(c(1,2,3))
#' moyenne(c(1,3,5))
moyenne <- function(x){
  x <- x %>% na.omit()
  sum(x)/length(x)
  #test
}
