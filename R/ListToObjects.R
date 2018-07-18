#' Extract objects from a list
#'
#' @param L A list
#' @param envir The environment into which variables are created
#'
#' @return No return
#' @description
#' Extract objects from a list to the global environment
#' @details
#' This function is for internal usage only.
#' @examples
#' ListToObjects(list(x=5,A="Hello",M=matrix(1:8,nr=2)),envir=.GlobalEnv)
#' @export
#'
ListToObjects <- function(L,envir=.GlobalEnv) {
  ii <- 1
  for (ii in 1:length(L)) {
    text. <- paste(  "assign('",names(L)[ii],"',L[[" , ii ,"]],pos=envir)",sep="")
    eval(parse(text = text.))
  }
}
