#' Save tca results to a folder
#'
#' @param tcaObject a tca-class object created by tca
#' @param path Location of the folder
#' @param folder Name of the folder
#' @param what What to save: all items specified will be saved
#' @param plotAxes A k x 2 matrix giving pairs of axes to plot and save
#' @param graphicDevice Format(s) of plots saved.  Plots can be saved in more than one format
#' @param csvFormat Format of csv files (North American or European)
#'
#' @return Figure
#' @importFrom utils head write.csv write.csv2
#' @import grDevices
#' @export
#' @examples
#' saveTCA(tca(rodent),path=tempdir())
saveTCA <- function(tcaObject, path  , folder = NULL, what = c("report",
  "csv", "plot", "dataMatrix", "tcaObject"), plotAxes = matrix((1:2),
  nrow = 1, ncol = 2, byrow = T), graphicDevice =  c(
    "pdf",
    "postscript",
    "xfig",
    "bitmap",
    "pictex",
    "cairo_pdf",
    "cairo_ps",
    "svg",
    "png",
    "jpeg",
    "bmp",
    "tiff"
  ), csvFormat = c("csv",
  "csv2")) {
  if (!("tca" %in% class(tcaObject))) return(NULL)



  what <- tolower(substr(what, 1, 2))
  csvFormat <- csvFormat[1]
  graphicDevice <- graphicDevice[1]

  # Prepare inputs
  # -----------------------------------------------------------

  dispersion <- tcaObject$dispersion
  rowScores <- tcaObject$rowScores
  colScores <- tcaObject$colScores
  rowMass <- tcaObject$rowMass
  colMass <- tcaObject$colMass
  nAxes <- tcaObject$nAxes
  dataName <- tcaObject$dataName
  algorithm <- tcaObject$algorithm
  dataMatrixTotal <- tcaObject$dataMatrixTotal
  dataMatrix <- tcaObject$dataMatrix
  rowColCombined <- tcaObject$rowColCombined


  nAxes <- length(dispersion)
  keep <- plotAxes[,1] <= nAxes & plotAxes[,2] <= nAxes
  plotAxes <- plotAxes[keep,,drop=F]

  if (is.null(folder)) {
    folderName <- dataName
  } else {
    folderName <- folder
  }

  # dirs <- list.dirs(path = path, full.names = F, recursive = F)

  folderName <- paste("TCA",folderName,sep="_")
  folderName. <- folderName
  folderName <- paste(path, "/", folderName, sep = "")

  dir.ii <- 0
  folderName. <- folderName
  while(file.info(folderName)$isdir & length(list.files(folderName) > 0)) {
    dir.ii <- dir.ii+1
    folderName <- paste(folderName.,"(",dir.ii,")",sep="")
  }


  if (is.na(file.info(folderName)$isdir)) {
    re <- dir.create(folderName, recursive = T)
    if (!re) {
      warning("\n================================\n Directory could not be created\n")
      return(NULL)
    }
  }
  if (!is.na(file.info(folderName)$isdir)) {
    if (!file.info(folderName)$isdir) {
      warning("\n================================\n Directory could not be created\n")
      return(NULL)
    }
  }




  # Save csv files
  # ----------------------------------------------------------

  if ("cs" %in% what) {

    dispersion <- matrix(dispersion, ncol = 1)
    rownames(dispersion) <- rownames(colScores)
    colnames(dispersion) <- "dispersion"

    saveList <- c("dispersion", "rowScores", "colScores",
      "rowMass", "colMass", "dataMatrixTotal", "dataMatrix")

      temp <- NULL

    item <- saveList[2]
    for (item in saveList) {
      eval(parse(text = paste("temp <-", item)))
      # if (class(temp) %in% c('numeric','matrix'))
      if (grepl("2", csvFormat)) {
        write.csv2(temp, file = paste(folderName, "/",
          item, ".csv", sep = ""))
      } else {
        write.csv(temp, file = paste(folderName, "/",
          item, ".csv", sep = ""))
      }
    }
  }

  # Save plots files
  # ----------------------------------------------------------
  graphicDevice <- intersect(
    graphicDevice,
    c(
      "pdf",
      "postscript",
      "xfig",
      "bitmap",
      "pictex",
      "cairo_pdf",
      "cairo_ps",
      "svg",
      "png",
      "jpeg",
      "bmp",
      "tiff"
    )
  )
  if ("pl" %in% what  & length(graphicDevice) == 0) {
    warning("Unavailable graphic device specified.  Figures will not be saved.")
  }

  outputDev <- NULL
  if ("pl" %in% what & length(graphicDevice) > 0) {
                       eval(parse(text = paste("outputDev <-", graphicDevice)))

    ii <- -1
    if (!is.matrix(plotAxes)) plotAxes <- as.matrix(plotAxes)

    for (ii in 1:nrow(plotAxes)) {
      fileName <- paste(folderName, "/", "TCA Plot", " ",
                        plotAxes[ii,1], "x", plotAxes[ii,2], ".", graphicDevice, sep = "")
      outputDev(fileName)
     plot.tca(tcaObject,axes=plotAxes[ii,])
     dev.off()
    }
  }
  # Write Report
  # ------------------------------------------------------------
  if ("re" %in% what) {
    sink(paste(folderName, "report.txt", sep = "/"), split = T)
    print.tca(tcaObject)
    while (sink.number() > 0) {
      sink()
    }
  }

return(folderName)
}






