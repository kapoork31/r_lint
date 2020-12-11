#############################################################
##  This file runs tests on featurisaton utils
##
#############################################################

###################################
# Load libraries and source files
########################## #########

source(file.path("~",
                 "scripts",
                 "fitbit_pipeline_cleaning_kunal",
                 "src",
                 "utils",
                 "fitbit_utils_3.R"
)
)

library(testthat)
library(readr)

conn <- xap.conn

file_extension <- file.path(
    "fitbit_pipeline_cleaning_kunal",
    "data",
    "clean_fitbit_data.csv")


dir <- sub("devices", "scripts", getwd())
dir <- file.path(dir, file_extension)

clean_fitbit_data  <- readr::read_csv(dir)

thresh_table <- "mvpa_thresh_11_9_4_5" # thresh table name

test_that("count total steps in day", {

    step_count <- StepCount(clean_fitbit_data, valCol = "value_steps")
    expect_type(step_count, "integer")
    expect_equal(step_count, sum(clean_fitbit_data$value_steps, na.rm = TRUE))

})


test_that("mean steps per hour", {

    avg_step_hours <- MeanHourlySteps(clean_fitbit_data, valCol = "value_steps")
    expect_type(avg_step_hours, "double")

})

test_that("ActiveHoursCount, number of hours wiht > 500 steps", {

    active_step_hours <- ActiveHoursCount(clean_fitbit_data,
                                      valCol = "value_steps")
    expect_type(active_step_hours, "integer")
    expect_equal(active_step_hours, 7)

})

test_that("LowActiveHoursCount, number of hours witt <= 500 steps", {

    low_active_step_hours <- LowActiveHoursCount(
        clean_fitbit_data,
        valCol = "value_steps"
    )

    expect_type(low_active_step_hours, "integer")
    expect_equal(low_active_step_hours, 13)

})


test_that("GetActiveWindow, start and end of active window", {

    active_window <- GetActiveWindow(clean_fitbit_data, valCol = "value_steps")
    expect_length(active_window, 2)
    expect_setequal(colnames(active_window), c("startWindow", "endWindow"))

})


test_that("RestingHrProxy, calculate rhr", {

    rhr <- RestingHrProxy(clean_fitbit_data, valCol = "value_hr")
    expect_type(rhr, "double")

})


test_that("HourlyInterval, calculate rhr", {

    intervals <- HourlyInterval(clean_fitbit_data, valCol = "value_hr")
    expect_length(intervals, 3)
    expect_setequal(colnames(intervals),
                    c("maxHourly",
                      "minHourly",
                      "medianHourly")
    )

})


test_that("MinuteWearCount, calculate rhr", {

    wear_time <- MinuteWearCount(clean_fitbit_data)
    expect_lte(wear_time, 1440)

})

test_that("CountMinutes, count consecutive minutes", {

    is_high <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    consec <- CountMinutes(is_high, 5)
    expect_equal(consec, length(is_high))

})

test_that("ActiveMinutes, count consecutive minutes", {

    active_minute_steps <- ActiveMinutes(
        clean_fitbit_data,
        valCol = "value_steps",
        threshold = 7
    )

    expect_type(active_minute_steps, "double")

})

test_that("CoefficientOfVariation, calc cov of numerical vector", {

    steps <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    numerical_values <- data.frame(value_steps = steps)
    cov <- CoefficientOfVariation(numerical_values, valCol = "value_steps")
    expect_equal(cov, 0)

})

test_that("activeMinutesRegThresh, thresh data per person per date", {

    thresh_data <- activeMinutesRegThresh(
        clean_fitbit_data$userid[1],
        as.Date(clean_fitbit_data$time[1]),
        thresh_table,
        conn)

    expect_type(thresh_data, "list")
})

test_that("switchTh, count number of times hr values goes above a threshold", {

    numerical_vector <- c(1, 121, 1, 121, 1, 121)
    switch_count <- switchTh(numerical_vector, 120)
    expect_equal(switch_count, 3)
})


test_that("mvpaCountNeighbourTimesPrev, count mvpa minutes", {

    active_hr <- clean_fitbit_data %>% filter(!!as.name(hr_column) > 120)
    fs_data <- clean_fitbit_data %>% select(!!as.name(fs_column), time)
    times_in <- mvpaCountNeighbourTimesPrev(active_hr,
                                            fs_data,
                                            fs_column = fs_column,
                                            windowSize = 15
    )

    expect_type(times_in[1], "double")
})


test_that("consecutive, count mvpa minutes with consecutive minimum", {

    active_hr <- clean_fitbit_data %>% filter(!!as.name(hr_column) > 120)
    fs_data <- clean_fitbit_data %>% select(!!as.name(fs_column), time)
    times_in <- mvpaCountNeighbourTimesPrev(active_hr,
                                            fs_data,
                                            fs_column = fs_column,
                                            windowSize = 15
    )

    mvpa_5_consec <- consecutive(times_in, 5)
    expect_type(mvpa_5_consec, "double")

})

test_that("stepNorm,
    count avg steps in period where hr is in a certain group", {

        active_hr <- clean_fitbit_data %>% filter(!!as.name(hr_column) > 120)
        fs_data <- clean_fitbit_data %>% select(!!as.name(fs_column), time)
        times_in <- mvpaCountNeighbourTimesPrev(active_hr,
                                                fs_data,
                                                fs_column = fs_column,
                                                windowSize = 15
        )
        step_norm_mvpa <- stepNorm(times_in, fs_data)
        expect_type(step_norm_mvpa, "double")
})
