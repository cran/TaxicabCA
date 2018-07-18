#' Search a taxicab principal component using the criss-cross algorithm
#' @param pResidual A matrix of of non-negative numbers
#' @param iterationMax Maximum number of iterations
#' @return A list: L1Max = maximum L1 norm; uMax = Linf unit vector giving the maximum L1 norm
#' @details
#' This function is for internal usage only.
#'
#' The vector uMax is normalize to uMax[1] = 1
#' @examples
#' SearchCrissCross(matrix(-3:8,nrow=4,ncol=3))
#'
#' @export
SearchCrissCross <- function(pResidual, iterationMax = 20) {
#  print("Running search Criss Cross")
  # if (F) {
  #   Y <- TextualData29
  #   # Y <- rodent Y <- TextualData29
  #   Y <- as.matrix(Y)
  #   TOT <- sum(Y)
  #   pResidual <- (Y - (matrix(rowSums(Y), ncol = 1) %*% (matrix(colSums(Y),
  #     nrow = 1))/TOT))/TOT
  #   # rm(Y)
  # }
  nRow <- nrow(pResidual)
  nCol <- ncol(pResidual)
  # lambdas <- rep(NaN, nRow) A <- matrix(0, nrow = nRow, ncol =
  # nRow) rownames(A) <- rownames(pResidual) colnames(A) <-
  # rownames(pResidual) B <- matrix(0, nrow = nCol, ncol = nRow)
  ii <- 1
  iiMax <- -Inf
  uMax <- matrix(NA, ncol = nCol, nrow = 1)
  colnames(uMax) <- colnames(pResidual)
  temp <- svd(pResidual)$v[1, ] > 0
  temp <- (-1)^temp
  temp <- temp * ((-1)^(temp[1] == -1))

  O <- order(apply(abs(pResidual), 1, max), decreasing = T)
  us <- rbind(temp, sign(pResidual[O, ]))
  us <- unique(us, MARGIN = 1)
  us <- matrix((-1)^(us[, 1] == -1), nrow = nrow(us), ncol = ncol(us)) *
    us
  t1 <- proc.time()["elapsed"]
  # print(c('nrows us',nrow(us)))
  for (ii in 1:nrow(us)) {
    if ((proc.time()["elapsed"] - t1) > 2) {
      cat("*")
      t1 <- proc.time()["elapsed"]
    }
    # ii <-6
    u <- us[ii, , drop = F]
    continue <- T
    jj <- 0
    while (continue & jj < iterationMax) {

      jj <- jj + 1
      a <- pResidual %*% t(u)
      v <- sign(a)
      LambdaA <- t(a) %*% v
      b <- t(v) %*% pResidual
      u <- sign(b)
      LambdaB <- u %*% t(b)
      continue <- (LambdaB - LambdaA > 10^-6)
    }
    if (LambdaA > iiMax) {
      iiMax <- LambdaA
      uMax <- u
    }
  }
  L <- list(L1Max = iiMax, uMax = uMax)
  return(L)
}
