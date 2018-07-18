#' Creates a symmetric plot from a tca-class object
#'
#' @param x A tca-class object created by tca
#' @param y Unused
#' @param axes The two axes to be plotted
#' @param labels.rc Two numbers: 0 Symbol only; 1 Label only; 2 Symbol and label
#' @param col.rc Colors for rows and columns contributions
#' @param pch.rc Plotting characters for rows and columns contributions
#' @param mass.rc Logical: Will the area of plotting characters be proportional to mass
#' @param cex.rc An overall size factor
#' @param jitter Logical: Will close points be moved slightly?
#' @param ... Unused.
#'
#' @return None
#' @details
#' If the number of rows is very large, labels will not be printed.
#'
#' In this version, jitter is coerced.
#'
###' @importFrom graphics abline plot points strheight strwidth text
#' @import graphics

#' @examples
#' plot(tca(rodent),labels=c(0,1))
#'
#' @export
plot.tca <- function(x,
                     y = NULL,
                     axes = c(1, 2),
                     labels.rc = c(0,
                                   1),
                     col.rc = c("blue", "red"),
                     pch.rc = c(16, 21, 17, 24),
                     mass.rc = c(F, F),
                     cex.rc = c(NA, NA),
                     jitter = c(T, F),...) {

    if (!("tca" %in% class(x))) {
    warning("Calling plot.tca on non-tca class")
    return(NULL)
  }

  # rm(list = setdiff(ls(), 'x'))


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


  iiAxis <- axes[1]
  jjAxis <- axes[2]

  dispersionx <- dispersion[iiAxis]
  dispersiony <- dispersion[jjAxis]

  ax <- rowScores[, iiAxis, drop = F]
  ay <- rowScores[, jjAxis, drop = F]
  bx <- colScores[iiAxis, , drop = F]
  by <- colScores[jjAxis, , drop = F]



  if (is.na(pch.rc[1])) {
    ax <- NULL
    ay <- NULL
  }
  if (is.na(pch.rc[2])) {
    bx <- NULL
    by <- NULL
  }


  aLabels <- rownames(ax)
  aCol <- col.rc[1]
  a.cex.rc <- cex.rc[1]
  bLabels <- colnames(bx)
  bCol <- col.rc[2]
  b.cex.rc <- cex.rc[2]

  if (is.na(a.cex.rc)) {
    a.cex.rc <- 0.4 + 1 / (1 + length(ax) / 60)
  }
  if (is.na(b.cex.rc)) {
    b.cex.rc <- 0.4 + 1 / (1 + length(bx) / 60)
  }



  xLim <- range(c(ax, bx)) + c(-0.1, 0.1) * diff(range(c(ax,
                                                         bx)))
  yLim <- range(c(ay, by)) + c(-0.1, 0.1) * diff(range(c(ay,
                                                         by)))

  temp <- max(abs((c(xLim, yLim))))
  yLim <- xLim <- c(-temp, temp)

  plot(
    NA,
    NA,
    xlab = paste(
      "Axis ",
      iiAxis,
      " (disp.: ",
      sprintf("%6.4f",
              dispersionx),
      ")",
      sep = ""
    ),
    ylab = paste(
      "Axis ",
      jjAxis,
      " (disp.: ",
      sprintf("%6.4f", dispersiony),
      ")",
      sep = ""
    ),
    main = dataName,
    xlim = xLim,
    ylim = yLim,
    type = "n"
  )
  near <- (strheight("A") + strwidth("AB")) * 0.5 * 1.5
  axy <- JitterPosition(ax, ay, Near= near)
  ax <- axy$x
  ay <- axy$y
  a.sizerelative <- axy$sizeRelative

  bxy <- JitterPosition(bx, by, Near= near)
  bx <- bxy$x
  by <- bxy$y
  b.sizeRelative <- bxy$sizeRelative


  if (labels.rc[1] != 1 & !is.null(ax))
    points(ax,
           ay,
           col = aCol,
           pch = pch.rc[1],
           cex = a.cex.rc)
  if (labels.rc[1] != 0 & !is.null(ax)) {
    text(ax,
         ay,
         aLabels,
         col = aCol,
         pos = if (labels.rc[1] ==
                   2)
           1,
         cex = a.cex.rc)
  }
  if (labels.rc[2] != 1 & !is.null(bx))
    points(bx,
           by,
           col = bCol,
           pch = pch.rc[2],
           cex = b.cex.rc)
  if (labels.rc[2] != 0 & !is.null(bx)) {
    text(bx,
         by,
         bLabels,
         col = bCol,
         pos = if (labels.rc[2] ==
                   2)
           1,
         cex = b.cex.rc)
  }

  abline(h = 0, lty = "dashed")
  abline(v = 0, lty = "dashed")
}
