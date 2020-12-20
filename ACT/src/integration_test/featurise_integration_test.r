#############################################################
##  This file runs the featurise step on act data
##
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

options(digits.secs = 3)

source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "pipeline_utils_kunal_2.R"
    )
)


source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "treatment_grouping_utils_kunal.R"
    )
)

conn <- xap.conn
link_table <- "linkMoresecure"
devices_clean_table_name <- "devices_clean_test"
devices <- fetch_all_devices(conn, devices_clean_table_name, link_table)

start_date <- "2018-09-01"
end_date <- "2018-09-29"
input_table_name <- "act_labeled_integration_test"
output_table_name <- "act_featurisation_integration_test"
time_col_name <- "actDate"
label_treatment_meta <- "act_labeled_meta_treatment_integration_test"
output_table_name_meta <- "act_featurisation_meta_integration_test"
pipeline_step_fn <- featurise_step

if (dbExistsTable(conn, output_table_name)) {

    dbRemoveTable(conn, output_table_name)
}

if (dbExistsTable(conn, output_table_name_meta)) {

    dbRemoveTable(conn, output_table_name_meta)
}

if (dbExistsTable(conn, label_treatment_meta)) {

    dbRemoveTable(conn, label_treatment_meta)
}

write_session_meta_data_lbld(
    conn,
    input_table_name,
    label_treatment_meta
)

process_unprocessed_patients(
  conn,
  input_table_name,
  output_table_name,
  start_date,
  end_date,
  time_col_name,
  devices_clean_table_name,
  link_table,
  pipeline_step_fn,
  label_treatment_meta,
  output_table_name_meta
)

nrow_feat_treatments <- dbGetQuery(conn,
    sprintf("SELECT count(*) FROM \"%s\"", output_table_name
    )
)

test_that("test number of sessions labelled", {

    expect_equal(nrow_feat_treatments %>% pull (count), 21)
})
