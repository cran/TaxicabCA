context("Test with rodent data with ex")
library(TaxicabCA)

algorithm <- "ex"
Data <- t(rodent)
axisTested <- 6

dispersion <-
  c(
    0.47792638,
    0.42232907,
    0.34739066,
    0.13832745,
    0.11965277,
    0.09097790,
    0.06066077,
    0.01020825
  )

colScores <-
  c(-0.03530946 , 0.00735461, -0.03583644 , -0.13723286 )
 names(colScores) <-
  c("Florida"   ,  "Sandmark"  ,  "34street"  ,  "Balboaterr")

rowScores <-
  c(1.07193837  ,   -0.04460425 ,    -0.02191538  ,    0.04369462 )
 names(rowScores) <-
  c("Rt.rattus"   ,    "Mus.musculus"   , "Pm.californicus",
    "Pm.eremicus")
 nAxes <- length(dispersion)
dispersion <- round(dispersion, 8)
if (nAxes < 10) {
  names(dispersion) <- paste("Axis", 1:nAxes, sep = "")
} else {
  names(dispersion) <- paste("Axis", substr(101:150, 2, 3)[1:nAxes])
}

rowScores <- round(rowScores, 8)
colScores <- round(colScores, 8)


re.tca <- tca(Data, nAxes = nAxes, algorithm = algorithm)

test_that("Dispersion", {
  expect_equal(round(re.tca$dispersion, 8), dispersion)
})

test_that("Column scores", {
  expect_equal(round(re.tca$colScores[axisTested,1:4], 8), colScores)
})

test_that("Row scores", {
  expect_equal(round(re.tca$rowScores[1:4, axisTested], 8), rowScores)
})
