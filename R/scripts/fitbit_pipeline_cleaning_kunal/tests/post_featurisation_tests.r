library(testthat)
library(readr)
library(lubridate)

conn <- xap.conn
fully_featurised_data_table <- "fitbit_featurise_table_2020_12"
fully_featurised_data <- dbGetQuery(conn,
                                    sprintf("SELECT * FROM \"%s\"",
                                    fully_featurised_data_table)
                                    )

link_name <- "patient_key_id_list"
link <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", link_name))
sex_table <- "sex1"

test_that("merge_gender_age", {

    merged <- merge_gender_age(fully_featurised_data_table,
                                sex_table,
                                link,
                                conn
                            )
    expect_equal(nrow(merged), nrow(fully_featurised_data))

})

test_that("outliers, find outliers", {

    merged <- merge_gender_age(fully_featurised_data_table,
                                sex_table,
                                link,
                                conn
                            )
    columns_to_check <- "mvpa_15PrevMethod"
    outlier_tagged <- OutlierFunc3(merged, c(columns_to_check))
    outlier_column <- paste(columns_to_check, "_isOut", sep = "")

    expect_equal(nrow(merged), nrow(outlier_tagged))
    expect_that(outlier_column %in% colnames(outlier_tagged), is_true())

})
