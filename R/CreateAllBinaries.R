#' Create all 2^n n-tuples of ab[1] and ab[2]
#'
#' @param n An integer > 0
#' @param ab A vector of length 2
#' @return a n x 2^n matrix
#' @details
#' This function is for internal usage only.
#' @examples
#'  CreateAllBinaries(3,c(0,1))
#'
#' @export
CreateAllBinaries <- function(n = 1, ab = c(0, 1)) {
  # Creates all 2^n n-tuples of ab[1] and ab[2] Test input;
  # return NaN if input is not appropriate Result has 2^n rows
  if ((length(ab) != 2) | (n <= 0) | (n%%1 != 0)) {
    return(NaN)
  }
  # Create a recursive function... it calls itself if n > 1
  if (n == 1) {
    return(matrix(ab, ncol = 2))
  } else {
    Result <- matrix(ab[1], ncol = 2^n, nrow = n)
    Result[2:n, 1:(2^(n - 1))] <- CreateAllBinaries(n - 1, ab)
    # Result[1,1:(2^(n - 1))] <- ab[1]
    Result[1, (1 + 2^(n - 1)):(2^n)] <- ab[2]
    Result[2:n, (1 + 2^(n - 1)):2^n] <- Result[2:n, 1:(2^(n -
      1))]
    return(Result)
  }
}
