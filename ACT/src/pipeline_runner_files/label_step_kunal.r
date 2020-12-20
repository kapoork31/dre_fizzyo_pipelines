#############################################################
##  This file runs labelelling step on the act data
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

start_date <- "2018-09-01"
end_date <- "2018-10-01"
input_table_name <- "act_clean_2020_12"
output_table_name <- "act_labeled_new_2020_12"
time_col_name <- "time"
pipeline_step_fn <- label_step
raw_table_name <- "pressure_raw_sept_oct"
meta_table_name <- "clean_meta_sessions_2020_12"
label_meta_table_name <- "label_meta_sessions_2020_12"

write_session_meta_data(conn, input_table_name, meta_table_name)

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
    raw_table_name,
    meta_table_name,
    label_meta_table_name
)
