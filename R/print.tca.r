#' Print result of Taxicab Analysis in easily readable format
#'
#' @param x A tca tcaObject produced by the function tca
#' @param ... Unused
#' @return An invisible list containing formated outputs
#' @export
#'
#' @examples
#' print(tca(rodent))
print.tca <- function(x,...) {
  if (!("tca" %in% class(x))) {
    warning("Calling print.tca on non-tca class")
    return(NULL)
  }
  if (!("tca" %in% class(x)))
    return(NULL)

  dispersion <- x$dispersion
  rowScores <- x$rowScores
  colScores <- x$colScores
  rowMass <- x$rowMass
  colMass <- x$colMass
  nAxes <- x$nAxes
  dataName <- x$dataName
  algorithm <- x$algorithm
  dataMatrixTotal <- x$dataMatrixTotal
  dataMatrix <- x$dataMatrix
  rowColCombined <- x$rowColCombined



  dispersion <- t(as.matrix(dispersion))
  rownames(dispersion) <- "Dispersion"
  Re <- list(dataName = dataName, algorithm = algorithm)
  cat(paste("Taxicab Correspondance Analysis", "\n\n"))
  cat(paste("Data name:", dataName, "\n\n"))
  cat(paste("Algorithm used:", algorithm, "\n\n"))
  if (algorithm == "Genetic") {
    cat("Note: The genetic algorithm option is experimental. Convergence of the genetic algorithm is not garanteed.  \n\n")
  }
  cat("Dispersion\n\n")
  temp <- print(round(dispersion, 6))
  Re <- c(Re, list(dispersion = temp))
  cat("\n\n")
  cat("Taxicab column factor scores (x 1000) and mass (X 1000)\n\n")
  temp <- print(round(1000 * rbind(colScores, colMass), 0))
  Re <- c(Re, list(colScores = temp))
  cat("\n\n")
  cat("Taxicab row factor scores (x 1000) and mass (X 1000)\n\n")
  temp <- print(round(1000 * head(cbind(rowScores, rowMass),
    50), 0))
  if (50 < nrow(rowScores))
    cat(paste("\n... (", nrow(rowScores) - 50, " rows not printed)",
      sep = ""))
  Re <- c(Re, list(rowScores = temp))
  cat("\n\n")
  invisible(Re)
}
