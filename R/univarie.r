#V de Cramer
#x = variable qualitative
#y = variable classe d'appartenance
#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @import questionr
#' @export
#'
#' @examples
vcramer<-function(x,y){
  #tableau de contingence
  tableau=table(x,y)
  #V de Cramer
  #0 (absence de liaison) et 1 (liaison parfaite)
  res=cramer.v(tableau)
  return(res)
}
