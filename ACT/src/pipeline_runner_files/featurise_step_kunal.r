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
input_table_name <- "act_labeled_new_2020_12"
output_table_name <- "act_featurised_2020_12"
time_col_name <- "act_date"
raw_table_name <- "pressure_raw_sept_oct"
label_treatment_meta <- "labeled_treatment_meta_2020_12"
output_table_name_meta <- "act_featurised_meta_2020_12"
pipeline_step_fn <- featurise_step

write_session_meta_data_lbld(conn, input_table_name, label_treatment_meta)

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
