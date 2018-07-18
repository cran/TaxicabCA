#' Search a taxicab principal component via exhaustive search
#'
#' @param pResidual A matrix of of non-negative numbers
#' @return A list: L1Max = maximum L1 norm; uMax = Linf unit vector giving the maximum L1 norm
#' @details
#' This function is for internal usage only.
#'
#' The vector uMax is normalize to uMax[1] = 1
#' @examples
#' SearchExhaustive(matrix(-3:8,nrow=4,ncol=3))
#'
#' @export
SearchExhaustive <- function(pResidual) {
  nCol <- ncol(pResidual)
  # xs <- nCol - 1 Maximum floating point matrix accepted: 2^25
  # entries
  jjStep <- 2^min(c(2^(nCol - 1), floor(27 - log(nrow(pResidual),
    2))))

  U <- rbind(rep(1, 2^(nCol - 1)), CreateAllBinaries(nCol -
    1, c(-1, 1)))  # JA # Directions of projections

  steps.n <- (2^(nCol - 1))/jjStep
  jjStep <- min(c(ncol(U),jjStep))
  L1Max <- -Inf
  jjCol <- 1
  # if (steps.n > 1) cat(paste('\n',steps.n,' required'))
  t1 <- proc.time()["elapsed"]
  while (jjCol < ncol(U)) {
    if ((proc.time()["elapsed"] - t1) > 2) {
      cat("*")
      t1 <- proc.time()["elapsed"]
    }
    sums <- colSums(abs(pResidual %*% U[, jjCol:(jjCol +
      jjStep - 1), drop = F]))
    L1Max. <- max(sums)
    if (L1Max. > L1Max) {
      L1Max <- L1Max.
      uMax <- U[, jjCol + which.max(sums) - 1,drop=F]
      # print(jjCol+which.max(sums)-1)
    }
    # print(c(jjCol,ncol(U),round(100000*L1Max)))
    jjCol <- jjCol + jjStep
  }
uMax <- t(uMax)
  colnames(uMax) <- colnames(pResidual)
  L <- list(L1Max = L1Max, uMax = uMax)
  return(L)
}

