#############################################################
##  This file runs featurisation integration test
##  featurises small amount of data
##  checks resulting dataset has correct number of rows
#############################################################

###################################
# Load libraries and source files
########################## #########
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(DBI)
library(tidyr)
library(rlang)
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

link_name <- "linkMoresecure"
conn <- xap.conn
sql_code <- sprintf("SELECT * FROM \"%s\"", link_name)
link <- dbGetQuery(conn, sql_code)

thresh_table <- "mvpa_thresh_11_9_4_5"

# Specify dataset names
start_date <- "2018-09-15"
end_date <- "2018-10-01"
k_raw_hr_table_name <- "hr_clean_integration_test"
k_raw_hr_table_name_meta <- "hr_clean_meta_integration"
k_raw_steps_table_name <- "fs_new"
k_raw_steps_table_name_meta <- "fs_meta_integration"
output_table_name <- "fitbit_featurise_table_integration_test"
output_table_name_meta <- "fitbit_featurise_table_meta_integration_test"


if (dbExistsTable(conn, output_table_name)) {

    dbRemoveTable(conn, output_table_name)
}

if (dbExistsTable(conn, output_table_name_meta)) {

    dbRemoveTable(conn, output_table_name_meta)
}

if (dbExistsTable(conn, k_raw_steps_table_name_meta)) {

    dbRemoveTable(conn, k_raw_steps_table_name_meta)
}


write_fs_meta_data(
    conn,
    k_raw_steps_table_name,
    k_raw_steps_table_name_meta
)

writeLines("############ featurise hr Data ############")


featurise_all(conn, start_date, end_date,
            k_raw_hr_table_name, k_raw_steps_table_name,
            k_raw_hr_table_name_meta, k_raw_steps_table_name_meta,
            output_table_name, link, output_table_name_meta, thresh_table)
            

nrow_featurised_days <- dbGetQuery(conn,
    sprintf("SELECT count(distinct(date)) FROM \"%s\"",
        output_table_name
    )
)

test_that("test number of days featurised", {

    expect_equal(nrow_featurised_days %>% pull (count), 15)
})
