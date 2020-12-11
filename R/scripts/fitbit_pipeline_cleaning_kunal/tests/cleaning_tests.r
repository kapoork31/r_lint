source("scripts/fitbit_pipeline_cleaning_kunal/src/utils/fitbit_cleaning_utils.R")
library(testthat)
library(readr)

file_extension = 'scripts/fitbit_pipeline_cleaning_kunal/data/raw_fitbit_data.csv'
#dir = paste(paste(strsplit(getwd(),'/devices')[1],'scripts',sep = '/'),file_extension,sep = '/')
rawFitbitData  <- readr::read_csv(file_extension)

x = 1

test_that("dropIndexes functions returns vector with given indexes removed", {
    
    fitbitData  <-data.frame(value = runif(100, min=0, max=20),value = runif(100, min=0, max=50))
    removeIndexes = c(1,2,3,4,5,6,7,8,9,10)
    dataIndexesRemoved = dropIndexes(fitbitData,removeIndexes)
    expect_equal(nrow(fitbitData)-length(removeIndexes), nrow(dataIndexesRemoved))
})


test_that("propDiff functions returns proportinal difference between 2 values", {
    
    propDifference = propDiff(6,4)
    expect_equal(propDifference, 50)
})


test_that("removeRepPointsdata functions that removes repeating values", {
    
    cleanedData = removeRepPointsdata(rawFitbitData)
    expect_equal(colnames(rawFitbitData), colnames(cleanedData))
    expect_lte(nrow(cleanedData),nrow(rawFitbitData))
})


test_that("removeRandomPoints functions that removes random values based on time", {
    
    cleanedData = removeRandomPoints(rawFitbitData)
    expect_equal(colnames(rawFitbitData), colnames(cleanedData))
    expect_lte(nrow(cleanedData),nrow(rawFitbitData))
})


test_that("remove70Leniant1 functions that removes values of 70 that are errors based on values around them and time", {
    
    cleanedData = remove70Leniant1(rawFitbitData)
    expect_equal(colnames(rawFitbitData), colnames(cleanedData))
    expect_lte(nrow(cleanedData),nrow(rawFitbitData))
})


test_that("removeOutlierPoints functions that removes values based on being outlier value vs neighbourhood of values", {
    
    cleanedData = removeOutlierPoints(rawFitbitData)
    expect_equal(colnames(rawFitbitData), colnames(cleanedData))
    expect_lte(nrow(cleanedData),nrow(rawFitbitData))
})

