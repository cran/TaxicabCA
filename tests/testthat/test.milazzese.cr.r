context("Test with milazzese data with cr")
library(TaxicabCA)

algorithm <- "cr"
Data <- milazzese
axisTested <- 8
# rowTested <- 4
# colTested <- 5

dispersion <-
  c(
    0.3413669, 0.3012575, 0.2781216, 0.2561321, 0.2551284, 0.2379143, 0.2009879, 0.1972494, 0.1769695, 0.1707820 )

rowScores <-
  c(-0.20797116,0.36450810,-0.53637358,0.66515444 )
 names(rowScores) <-
   c("R01" ,       "R02",        "R03",        "R04")

colScores <-
  c(  0.24158556 , -0.08601719  ,0.16888127 , 0.23112907 )
 names(colScores) <-
   c("M01" ,       "M02",        "M03",        "M04")


dispersion <- round(dispersion, 7)
nAxes <- length(dispersion)
if (nAxes < 10) {
  names(dispersion) <- paste("Axis", 1:nAxes, sep = "")
} else {
  names(dispersion) <- paste("Axis", substr(101:150, 2, 3)[1:nAxes],sep="")
}

rowScores <- round(rowScores, 7)
colScores <- round(colScores, 7)


re.tca <- tca(Data, nAxes = nAxes, algorithm = algorithm)

test_that("Dispersion", {
  expect_equal(round(re.tca$dispersion, 7), dispersion)
})

test_that("Column, scores", {
  expect_equal(round(re.tca$colScores[axisTested,1:4], 7), colScores)
})

test_that("Row, scores", {
  expect_equal(round(re.tca$rowScores[1:4, axisTested],7), rowScores)
})
