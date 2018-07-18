#' Removes rows and columns of zeros and optionnally, row or column duplicates
#'
#' @param Y A matrix or an object that can be coerced to a matrix
#' @param rows Logical: Will duplicate rows be removed?
#' @param cols Logical: Will duplicate columns be removed?
#' @return A matrix with rows and columns removed as requested
#' @details
#' Rows and columns of zeros will be removed.
#'
#' A matrix of zeros will be returned as matrix with 0 row and 0 column.
#'
#' If rows 1,2,3 are combined, the name of row 1 is kept. Similarly for columns.
#'
#' @examples
#' CombineCollinearRowsCols(matrix(1:3,nrow=3,ncol=2),cols=TRUE)
#'
#' CombineCollinearRowsCols(cbind(matrix(1:3,nrow=3,ncol=2),rep(0,3)),cols=TRUE)
#'
#' CombineCollinearRowsCols(cbind(matrix(1:3,nrow=3,ncol=2),rep(0,3)))
#'
#' CombineCollinearRowsCols(matrix(0,nrow=3,ncol=3))
#'
#' CombineCollinearRowsCols(rodent,TRUE,FALSE)
#'
#' @export
CombineCollinearRowsCols <- function(Y, rows = F, cols = F) {
  # print(Y)
  try(Y <- as.matrix(Y))
  if (class(Y) != "matrix")
    return(NULL)
  if (sum(abs(Y)) == 0)
    return(matrix(NA, nrow = 0, ncol = 0))
  if (!cols & nrow(Y) == 1)
    return(sum(Y))
  if (!rows & ncol(Y) == 1)
    return(sum(Y))
  Y <- Y[apply(abs(Y), 1, sum) != 0, , drop = F]
  if (ncol(Y) > 1)
    Y <- Y[, apply(abs(Y), 2, sum) != 0, drop = F]
  if (rows == F & cols == F)
    return(Y)

  if ((rows == F & cols == T)) {
    Y <- t(CombineCollinearRowsCols(t(Y), rows = T, cols = F))
    return(Y)
  }
  if ((rows == T & cols == T)) {
    Y <- CombineCollinearRowsCols(Y, rows = T)
    Y <- t(CombineCollinearRowsCols(t(Y), rows = T))
    return(Y)
  }

  ExtractColinearRowsBasis <- function(Y, indexCol = 1) {
    OK <- sum(Y[, indexCol]) != 0
    while ((!OK) & indexCol < ncol(Y)) {
      indexCol <- indexCol + 1
      OK <- sum(Y[, indexCol] != 0)
    }
    keep <- which(Y[, indexCol] != 0)
    Y.Remainer <- Y[-keep, , drop = F]
    Y.Kept <- Y[keep, , drop = F]
    Y.Kept <- (1/Y.Kept[, indexCol]) * Y.Kept
    M <- unique(Y.Kept[(duplicated(Y.Kept, MARGIN = 1)),
      , drop = F], MARGIN = 1)
    if (nrow(Y.Remainer) > 0 & indexCol < ncol(Y)) {
      M1 <- ExtractColinearRowsBasis(Y.Remainer, indexCol = indexCol +
        1)
      M <- rbind(M, M1)
    }
    # return(list(M=unique(Y.Kept[duplicated( Y.Kept
    # ,MARGIN=1),],MARGIN=1),indexCol=IndexCol))
    return(M)
  }
  CheckVectorCollinearity <- function(V, W) {
    keep <- !(V == 0 & W == 0)
    V <- V[keep]
    W <- W[keep]
    if (length(V) != length(W) | sum(is.na(V)) > 0 | sum(is.na(W)) >
      0)
      return(NULL)
    return(length(unique(round(V/W, 10))) == 1)
  }

  Basis <- ExtractColinearRowsBasis(Y)
  if (nrow(Basis) == 0)
    return(Y)
  rowsToRemove <- c()
  ii <- 1
  rowsCombined <- list()
  for (ii in 1:nrow(Basis)) {
    rowsKept <- which(apply(Y, 1, CheckVectorCollinearity,
      W = Basis[ii, ]))
    rowsCombined <- c(rowsCombined, list(names(rowsKept)))
    names(rowsCombined)[length(rowsCombined)] <- names(rowsKept)[1]

    Y[rowsKept[1], ] <- apply(Y[rowsKept, , drop = F], 2,
      sum)
    if (!is.null(rownames(Y))) rownames(Y)[rowsKept[1]] <- paste(names(rowsKept),collapse="+")
    rowsToRemove <- c(rowsToRemove, rowsKept[-1])
  }
  if (length(rowsCombined) > 0) {
    print("Rows/columns combined")
    print(rowsCombined)
  }
  return(Y[-rowsToRemove, ])

}

