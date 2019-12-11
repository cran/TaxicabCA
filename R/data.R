#' Rodent species abundance
#'
#' Counts of rodents by species and site
#'
#' @format A data frame of counts with 28 rows and 9 columns
#' \itemize{
#'   \item Columns: Rodent species
#'   \item Rows: Location
#' }
#' @usage data(rodent)
#' @name rodent
#' @source Bolger et al. 1997, Response of rodents to habitat fragmentation in coastal Southern California, Ecological Applications 7, 552-563 (as modified and distributed in a University of British Columbia Zoology Department workshop)
#' @examples
#' tca(rodent,nAxes=4)
"rodent"

#' Counts of archeological objects
#'
#' Frequency of object types across the 19 huts of the P. Milazzese settlement in north-eastern Sicily.
#'
#' @format A data frame of frequencies with 31 rows and 19 columns
#' \itemize{
#'   \item Columns: Hut
#'   \item Rows: Object Type
#' }
#' @usage data(milazzese)
#' @name milazzese
#' @source Alberti, G., 2013, Making Sense of Contingency Tables in Archaeology: the Aid of Correspondence Analysis to Intra-Site Activity Areas Research, Journal of Data Science 11, 479-499

#' @examples
#' tca(milazzese,nAxes=6,algorithm = "criss-cross")
"milazzese"

