#############################################################
##  This file runs cleaning step integration test
##  cleans a tiny chunk of data
## checks produced dataset has correct number of rows
#############################################################

###################################
# Load libraries and source files
########################## #########
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(DBI)
library(dbplyr)
library(tidyverse)
library(testthat)


source(file.path("~",
                 "scripts",
                 "fitbit_pipeline_cleaning_kunal",
                 "src",
                 "utils",
                 "test_new_batch_system_data_2.R"
)
)

source(file.path("~",
                 "scripts",
                 "fitbit_pipeline_cleaning_kunal",
                 "src",
                 "utils",
                 "fitbit_meta.R"
)
)

writeLines("############ clean hr Data ############")

link_name <- "linkMoresecure"
conn <- xap.conn
sql_code <- sprintf("SELECT * FROM \"%s\"", link_name)
link <- dbGetQuery(conn, sql_code)

# Specify dataset names
start_date <- "2018-09-15"
end_date <- "2018-10-01"
k_raw_hr_table_name <- "heart_rate_vals_2020_618"
raw_hr_meta <- "raw_hr_meta_integration_test"
conn <- xap.conn

output_table_name <- "hr_clean_integration_test"
clean_hr_meta <- "hr_clean_meta_integration"


if (dbExistsTable(conn, output_table_name)) {

    dbRemoveTable(conn, output_table_name)
}

if (dbExistsTable(conn, clean_hr_meta)) {

    dbRemoveTable(conn, clean_hr_meta)
}

if (dbExistsTable(conn, raw_hr_meta)) {

    dbRemoveTable(conn, raw_hr_meta)
}


write_raw_hr_meta_data(
    conn,
    k_raw_hr_table_name,
    raw_hr_meta
)

clean_all(
  conn, start_date, end_date,
  k_raw_hr_table_name, output_table_name,
  link, raw_hr_meta, clean_hr_meta
)


nrow_cleaned_days <- dbGetQuery(conn,
    sprintf("SELECT count(distinct(date)) FROM \"%s\"",
        clean_hr_meta
    )
)

test_that("test number of days cleaned", {

    expect_equal(nrow_cleaned_days %>% pull (count), 15)
})
