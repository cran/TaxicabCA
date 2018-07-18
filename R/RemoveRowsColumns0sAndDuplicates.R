#' Removes rows and columns of zeros and optionnally, row or column duplicates
#'
#' @param Y A matrix
#' @param zeros Logical Will rows and columns of zeros be removed?
#' @param rows Logical Will duplicate rows be removed?
#' @param cols Logical Will duplicate columns be removed?
#' @return A matrix with rows and columns removed as requested
#' @examples
#' RemoveRowsColumns0sAndDuplicates(matrix(1:3,nrow=3,ncol=2),cols=TRUE)
#'
#' RemoveRowsColumns0sAndDuplicates(cbind(matrix(1:3,nrow=3,ncol=2),rep(0,3)),cols=TRUE)
#'
#' RemoveRowsColumns0sAndDuplicates(cbind(matrix(1:3,nrow=3,ncol=2),rep(0,3)),zeros=TRUE)
#'
#' RemoveRowsColumns0sAndDuplicates(matrix(0,nrow=3,ncol=3),zeros=TRUE)
#'
#' @export
RemoveRowsColumns0sAndDuplicates <- function(Y, rows = F, cols = F,
  zeros = F) {
  ## Remove 0 Rows and 0 Columns
  dim(Y)
  if (zeros) {
    Y <- Y[apply(abs(Y), 1, sum) != 0, , drop = F]
    dim(Y)
    Y <- Y[, apply(abs(Y), 2, sum) != 0, drop = F]
    dim(Y)
  }

  ## Remove duplicated rows and columns
  dim(Y)
  if (rows) {
    temp <- which(duplicated(Y, MARGIN = 1))

    Y <- Y[-temp, , drop = F]
  }
  dim(Y)
  if (cols) {
    temp <- which(duplicated(Y, MARGIN = 2))

    Y <- Y[, -temp, drop = F]
    dim(Y)
  }
  return(Y)
}
