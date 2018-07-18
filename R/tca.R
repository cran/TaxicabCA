#' Taxicab Correspondance analysis
#'
#' Computes the Taxicab correspondance analysis of a matrix of non-negative numbers
#' @param Y A m x n matrix of non-negative numbers.  If Y is not a matrix, the 'as.matrix' transformation will be attempted.  Missing values are not allowed.
#' @param nAxes Number of axes to compute
#' @param dataName A name to be used to identify the outputs in 'plot' and 'saveTCA' ()
#' @param combineCollinearRows Should collinear rows be combined?
#' @param combineCollinearCols Should collinear columns be combined?
#' @param algorithm Algorthim requested - may be abreviated to first two letters
#' @param returnInputMatrix Will the input matrix be returned
#' @param exhaustiveAlgorithmMaxnCol Maximum size for exhaustive search
#' @param L1MaxDeltaMax Change of L1 norm acceptable for convergence in iterative searches
#' @param verbose Report progress (default) or not
#' @return A list with class 'tca' containing the following components:
#' \item{dispersion}{A nAxes-length vector of  matrix of column contributions}
#' \item{rowScores}{A m x nAxes matrix of column contributions}
#' \item{colScores}{A nAxes x n matrix of row contributions}
#' \item{rowMass}{Row weights: apply(Y,1,sum)/sum(Y)}
#' \item{colMass}{Column weights: apply(Y,2,sum)/sum(Y)}
#' \item{dataName}{A name to be used to identify the output in 'plot' and 'save'}
#' \item{algorithm}{Algorithm used (may be different from the algorythm requested)}
#' \item{dataMatrixTotal}{Sum of the input matrix entries}
#' \item{dataMatrix}{The matrix used in the computation}
#' \item{rowColCombined}{A list describing removed or combined rows and columns, if any}
#' @details
#' Computations are carried out on the transposed matrix if nrow(Y) < ncol(Y).  In the following, we assume that nrow(Y) >= ncol(Y)
#'
#' Row and column names will be created if necessary.
#'
#' Zeros rows and columns are removed.
#'
#' If ncol(Y) <= exhaustiveAlgorithmMaxnCol the exhaustive algorithm used unless otherwise specified.
#'
#' If ncol(Y) > exhaustiveAlgorithmMaxnCol the genetic algorithm used unless otherwise specified.
#'
#' Algorithm = exhaustive is overridden if ncol(Y) > exhaustiveAlgorithmMaxnCol.
#'
#' For ncol(Y) <= exhaustiveAlgorithmMaxnCol, the user may want to specify algorithm = genetic is nrow(Y) is very large, since exhaustive computation may be slow.
#'
#' If ncol(Y) <= exhaustiveAlgorithmMaxnCol the genetic algorithm is used unless otherwise specified.
#'
#' (ncol(Y) = 20 appears to be the maximum practical on 2017 vintage Intel-based desktops).
#'
#' @importFrom utils installed.packages
#' @export
#'
#' @examples
#' tca(rodent,nAxes=4)
#' tca(rodent,nAxes=4,combineCollinearRows=c(TRUE,FALSE))
tca <- function(Y, nAxes = 2, dataName = NULL, combineCollinearRows = c(F,
  T), combineCollinearCols = c(F, T), algorithm = c("exhaustive",
   "criss-cross","genetic"), returnInputMatrix = c(T, F), verbose = (nAxes > 2), exhaustiveAlgorithmMaxnCol = 20,
  L1MaxDeltaMax = 10^-10) {




  ## Check matrix class
  Y <- try(as.matrix(Y))
  if (class(Y) != "matrix") {
    # cat("\n Invalid input: cannot be coerced into a numerical matrix")
    stop("Input cannot be coerced into a matrix")
    return(NULL)
  }

  if (min(dim(Y)) < 2) {
    # cat("\n Invalid input: cannot be coerced into a numerical matrix")
    stop(paste("Invalid matrix dimensions (",paste(dim(Y),collapse="x"),")",sep=""))
    return(NULL)
  }


  ## Check for non-numeric
  if (mode(Y) != "numeric") {
    # cat("\n Invalid input: cannot be coerced into a numerical matrix")
    stop("Input contains non-numerical values")
    return(NULL)
  }

  ## Check input matrix for NA
  if (sum(is.na(Y)) != 0) {
    stop("Missing values are not allowed")
    return(NULL)
  }
  ## Check input matrix for negative values
  if (sum(Y < 0, na.rm = T) != 0) {
    stop("Negative values are not allowed")
    return(NULL)
  }


  ## Check/create data Name
  if (is.null(dataName)) {
    yNames <- as.list(match.call())$Y
    datetime <- gsub(":", ".", substr(Sys.time(), 1, 16),
      fixed = T)
    dataName <- paste(yNames)
  }
  dataName <- dataName[1]

  ## Remove/combined rows
  combineCollinearRows <- combineCollinearRows[1]
  combineCollinearCols <- combineCollinearCols[1]
  rowColCombined <- NULL
  returnInputMatrix <- returnInputMatrix[1]
  if (!(combineCollinearRows %in% c(T, F)))
    cat("\n Invalid 'combineCollinearRows' value")
  if (!(combineCollinearCols %in% c(T, F)))
    cat("\n Invalid 'combineCollinearCols' value")

  if (!(returnInputMatrix %in% c(T, F)))
    cat("\n Invalid 'returnInputMatrix' value")
  if (!(combineCollinearCols %in% c(T, F)))
    cat("\n Invalid 'combineCollinearCols' value")




  nRow <- nrow(Y)
  nCol <- ncol(Y)

  ## Create row and column names if they are missing
  if (is.null(rownames(Y)))
    rownames(Y) <- paste("R", substr(10^ceiling(log(10^-10 +
      nRow, 10)) + (1:nRow), 2, 1 + ceiling(log(10^-10 +
      nRow, 10))), sep = "")
  if (is.null(colnames(Y)))
    colnames(Y) <- paste("C", substr(10^ceiling(log(10^-10 +
      nCol, 10)) + (1:nCol), 2, 1 + ceiling(log(10^-10 +
      nCol, 10))), sep = "")

  Y <- CombineCollinearRowsCols(Y)
  if (combineCollinearRows)
    Y <- CombineCollinearRowsCols(Y, rows = T)
  if (combineCollinearCols)
    Y <- CombineCollinearRowsCols(Y, cols = T)


  ## Use smallest number of columns
  matrixTransposed <- F
  if (nrow(Y) < ncol(Y)) {
    matrixTransposed <- T
    Y <- t(Y)
  }

  nRow <- nrow(Y)
  nCol <- ncol(Y)

  nAxes <- min(c(nAxes, dim(Y) - 1))

  algorithm <- algorithm[1]

  if (!is.null("algorithm")) {
    algorithm <- tolower(substr(algorithm, 1, 2))
    if (!(algorithm %in% c("ex", "ge", "cr"))) {
      algorithm <- NULL
    }
    if (algorithm == "ex" & nCol > exhaustiveAlgorithmMaxnCol) {
      algorithm <- "cr"
    }
  }

  if (is.null(algorithm)) {
    if (nCol <= exhaustiveAlgorithmMaxnCol) {
      algorithm <- "ex"
    } else {
      algorithm <- "cr"
    }
  }

  if (algorithm == "ge" & !("GA" %in% installed.packages())) {
    algorithm <- "cr"
    warning("Package GA not found.  Defaulting to criss-cross algorithm.")
  }

  if (algorithm == "ge") {
    cat("The genetic algorithm option is experimental. Convergence of the genetic algorithm is not garanteed.  \n\n")
  }

  axesNames <- paste("Axis", substr(10^ceiling(log(10^-10 +
    nAxes, 10)) + (1:nAxes), 2, 1 + ceiling(log(10^-10 +
    nAxes, 10))), sep = "")


  Toti <- matrix(rowSums(Y), ncol = 1)
  TOT <- colSums(Toti)
  rownames(Toti) <- rownames(Y)
  Totj <- matrix(colSums(Y), nrow = 1)
  colnames(Toti) <- rownames(Totj) <- "MASS"
  colnames(Totj) <- colnames(Y)

  Ti <- Toti/TOT
  Tj <- Totj/TOT


  # pResidual <- (Y/TOT) - (Ti %*% Tj)
  pResidual <- (Y - (matrix(rowSums(Y), ncol = 1) %*% (matrix(colSums(Y),
    nrow = 1))/TOT))/TOT
  A <- matrix(0, nrow = nRow, ncol = nAxes)
  rownames(A) <- rownames(Y)
  B <- matrix(0, ncol = nCol, nrow = nAxes)
  colnames(B) <- colnames(Y)
  lambda <- rep(NaN, nAxes)  # matrix(0, nrow = nAxes, ncol = 1)
  names(lambda) <- axesNames
  colnames(A) <- rownames(B) <- axesNames

  t0 <- Sys.time()
  t1 <- Sys.time()
  difftime(t1, t0, units = "secs")

  # nCol <- 25
  memoryUsed <- 2^(nCol - 1) * nCol * 4
  memoryUsed/(2^20)


  if (algorithm == "ex") {
    algorithmUsed <- "Exhaustive"
    SearchFunction <- SearchExhaustive
  } else if (algorithm == "ge") {
    if (requireNamespace("GA", quietly = T)) {
      algorithmUsed <- "Genetic"
      SearchFunction <- SearchGeneticAlgoritm
    } else { algorithm <- "cr"
    algorithmUsed <- "Criss-cross"
    SearchFunction <- SearchCrissCross
    }
  } else if (algorithm == "cr") {
    algorithmUsed <- "Criss-cross"
    SearchFunction <- SearchCrissCross
  } else {
    return(NULL)
  }



#  iterationMax <- 20


  iiAxis <- 1
  ## Loop computing lambda, FF et GG
  for (iiAxis in 1:nAxes) {
    if (verbose)
      cat(paste("Computing axis no", iiAxis,"\n"))
    axisRes <- SearchFunction(pResidual)
    lambda[iiAxis] <- axisRes$L1Max
    A[, iiAxis] <- pResidual %*% t(axisRes$uMax)
    v <- sign(A[, iiAxis, drop = F])
    B[iiAxis, ] <- t(v) %*% pResidual
    pResidual <- pResidual - A[, iiAxis, drop = F] %*% (B[iiAxis,
      , drop = F]/lambda[iiAxis])
    # pResiduals <- c(pResiduals, list(pResidual))
  }



  FF <- A/as.vector(Ti)
  GG <- t(B * matrix(1/Tj, nrow = nAxes, ncol = nCol, byrow = T))


  if (matrixTransposed) {
    A. <- A
    A <- t(B)
    B <- t(A.)
    rm(A.)
    FF. <- FF
    FF <- t(GG)
    GG <- t(FF.)
    rm(FF.)
    u <- sign(FF)
  }


  # ANorm <- t(t(A)/lambda) BNorm <- (1/lambda)*B


  v <- sign(GG)
  u <- sign(FF)

  rowScores <- as.vector(1/Ti) * A
  colScores <- B/as.vector(Tj)

  rowScores <- A/as.vector(Ti)
  colScores <- B * matrix(1/Tj, nrow = nAxes, ncol = nCol, byrow = T)

  if (F) {
    Max <- max(c(range(abs(rowScores[, 1:2])), range(abs(colScores[1:2,
      ]))))
    plot(rowScores[, 1], rowScores[, 2], col = "red",
      xlim = c(-Max, Max), ylim = c(-Max, Max), type = "n")
    text(rowScores[1, ], rowScores[2, ], rownames(Y),
      col = "red")
    # points(colScores[,1],colScores[,2],col='blue',pch=19,cex=2)
    text(colScores[, 1], colScores[, 2], colnames(Y),
      col = "blue")
  }

  if (!returnInputMatrix)
    Y <- NA
  L <- list(dispersion = lambda, rowScores = rowScores,
    colScores = colScores, rowMass = Ti, colMass = Tj,
    nAxes = nAxes, dataName = dataName, algorithm = algorithmUsed,
    dataMatrixTotal = TOT, dataMatrix = Y, rowColCombined = NULL)
  class(L) <- c(class(L), "tca")
  # save(L, file = 'L.RData')
  return(L)




}  # Function end


