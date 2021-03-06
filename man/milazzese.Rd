% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{milazzese}
\alias{milazzese}
\title{Counts of archeological objects}
\format{A data frame of frequencies with 31 rows and 19 columns
\itemize{
  \item Columns: Hut
  \item Rows: Object Type
}}
\source{
Alberti, G., 2013, Making Sense of Contingency Tables in Archaeology: the Aid of Correspondence Analysis to Intra-Site Activity Areas Research, Journal of Data Science 11, 479-499
}
\usage{
data(milazzese)
}
\description{
Frequency of object types across the 19 huts of the P. Milazzese settlement in north-eastern Sicily.
}
\examples{
tca(milazzese,nAxes=6,algorithm = "criss-cross")
}
\keyword{datasets}
