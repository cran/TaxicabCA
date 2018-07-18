#' L1 norm of a projection
#'
#' @param pResidual A matrix with nc columns
#' @param uFT A vector of 0s and 1s of lenght nc: (-1)^uFT is a unit vector in Linf  norm
#' @return L1 norm of the pResidual x (-1)^uFT
#' @details
#' This function is for internal usage only.
#' @examples
#' ComputeLambda(uFT=c(FALSE,TRUE,FALSE),pResidual=matrix(1:15,nr=5,nc=3))
#'
#' @export
ComputeLambda <- function(uFT, pResidual) {
  u <- matrix((-1)^uFT, nrow = ncol(pResidual), ncol = 1)
  a <- pResidual %*% u
  # v <- sign(a)
  return(lambda = sum(abs(a)))
}
