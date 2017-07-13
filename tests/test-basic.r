context("basic")

library(RSelenium)
library(testthat)

remDr <- remoteDriver()
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:6268"

test_that("can connect to app", {  
  remDr$navigate(appURL)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "Wallace")  
})

test_that("Tabs are present", {  
  webElems <- remDr$findElements("id", "tabs")
  appCtrlLabels <- sapply(webElems, function(x){x$getElementText()})
  appCtrlLabels <- unlist(strsplit(appCtrlLabels[[1]], "\n"))
  expect_equal(appCtrlLabels[1], "Intro")  
  expect_equal(appCtrlLabels[2], "1 Occ Data")  
  expect_equal(appCtrlLabels[3], "2 Process Occs")  
  expect_equal(appCtrlLabels[4], "3 Env Data")  
  expect_equal(appCtrlLabels[5], "4 Process Envs")  
  expect_equal(appCtrlLabels[6], "5 Partition Occs")  
  expect_equal(appCtrlLabels[7], "6 Model")  
  expect_equal(appCtrlLabels[8], "Session Code")  
})
remDr$close()