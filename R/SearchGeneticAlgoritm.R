#' Search a taxicab principal component using the genetic algorithm
#' @param pResidual A matrix of of non-negative numbers
#' @return A list: L1Max = maximum L1 norm; uMax = Linf unit vector giving the maximum L1 norm
#' @details
#' This function is for internal usage only.
#'
#' The vector uMax is normalize to uMax[1] = 1
#' @examples
#' SearchGeneticAlgoritm(matrix(-3:8,nrow=4,ncol=3))
#'
#' @export
SearchGeneticAlgoritm <- function(pResidual) {
  if (requireNamespace("GA", quietly = T)) {
    res.ga <-
      GA::ga(
        type = "binary",
        ComputeLambda,
        pResidual = pResidual,
        nBits = ncol(pResidual),
        monitor = F
      )
    uMax <- matrix((-1) ^ res.ga@solution[1,],
                   ncol = ncol(pResidual),
                   nrow = 1)
    uMax <- uMax * (-1) ^ (uMax[1] == -1)
    return(list(L1Max = max(res.ga@fitness), uMax = uMax))
  } else {
    return(NULL)
  }
}
