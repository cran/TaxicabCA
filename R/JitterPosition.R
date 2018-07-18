#' Moves points that are close together
#'
#' @param x The x-coordinates of the points
#' @param y The y-coordinates of the points
#' @param jitterDefault A positive number controlling the maximum jitter
#' @param Near A positive number controlling the definition of "near"
#'
#' @details
#' This function is for internal usage only.
#'
#'@return A list giving the new x and y coordinates, and, for each point, its number neighbours and a suggested relative size for the plotting symbol
#' @examples
#' JitterPosition(c(1:5,2,2,4,4,4),c(1:5,2,2,4,4,4))
#'
#' @importFrom stats cutree dist hclust princomp runif
#' @export
#'
JitterPosition <- function(x,y, jitterDefault = .01, Near = 0.045) {
  # This function find cluster of points and moves around their
  # 'position'
  if (is.null(x) | is.null(y)) {
    return(list(xNew=NULL,yNew=NULL,nNeighbours=1,sizeRelative=1))
  }
  xy <- cbind(as.vector(x),as.vector(y))
  Res <- hclust(dist(xy))
  # plot(Res)
  Groups <- cutree(Res, h = Near) #
  # Groups <- cutree(Res, h = max(Res$height)/5) #
  Groupsn <- table(Groups)
  xNew <- x
  yNew <- y
  xDelta <- jitterDefault * diff(range(x))
  yDelta <- jitterDefault * diff(range(y))
  nNeighbours <- merge(data.frame(Groups=Groups),data.frame(Groupsn),by.x="Groups",by.y="Groups")$Freq
  # cbind(x,y,nNeighbours)
  # Gr <- '7'
  for (Gr in names(Groupsn)) {
    if (Groupsn[Gr] > 1) {
      Keep <- Groups == as.integer(Gr)
      # print(Keep)
      O <- 2*pi* (-1+order(princomp(xy[Keep, ])$scores[, 1])/(Groupsn[Gr]))
      O <- O + pi/2
      # Position[Keep] <- 0 + 1 * order(xy[Keep,2])%%4
      Dist <- (1:Groupsn[Gr])/Groupsn[Gr]
      Dist <- 1
      Dist <- runif(Groupsn[Gr])
      xNew[Keep] <- x[Keep] + cos(O)*xDelta*Dist*(Groupsn[Gr]-1)^.25
                        yNew[Keep]  <- y[Keep] + sin(O)*yDelta*Dist*(Groupsn[Gr]-1)^.25
    }
  }
  # print(Position)
  sizeRelative <- 1.25 / (1 + nNeighbours/4)
  # cbind(x,y,nNeighbours,sizeRelative)
  return(list(xNew=xNew,yNew=yNew,nNeighbours=nNeighbours,sizeRelative=sizeRelative))
}
