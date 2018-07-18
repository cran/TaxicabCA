#' Summary of the Taxicab analysis
#'
#' @param object A tca tcaObject produced by the function tca
#' @param ... Unused
#'
#' @return A list
#' @details
#' Shows the unstandardized dispersion values
#' @export
#'
#' @examples
#' summary(tca(rodent))
summary.tca <- function(object,...) {
  if (!("tca" %in% class(object))) {
  warning("Calling summary.tca on non-tca class")
    return(NULL)
  }
  dispersion <- object$dispersion
  rowScores <- object$rowScores
  colScores <- object$colScores
  rowMass <- object$rowMass
  colMass <- object$colMass
  nAxes <- object$nAxes
  dataName <- object$dataName
  algorithm <- object$algorithm
  dataMatrixTotal <- object$dataMatrixTotal
  dataMatrix <- object$dataMatrix
  rowColCombined <- object$rowColCombined

  dispersion <- t(as.matrix(dispersion))
  rownames(dispersion) <- "Dispersion"
  Re <- list(dataName = dataName, algorithm = algorithm)
  cat(paste("Data name:", dataName, "\n\n"))
  cat(paste("Algorithm used:", algorithm, "\n\n"))
  cat("Dispersion\n\n")
  temp <- print(round(dispersion, 6))
  Re <- c(Re, list(dispersion = temp))
  invisible(Re)
}
