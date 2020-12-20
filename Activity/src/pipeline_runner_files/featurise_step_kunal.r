#############################################################
##  This file extracts the features from the fitbit dataset
##
## Input datasets: raw footsteps, heart rate data
## Outputs: featurised dataset in the DRE
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

source(file.path("~",
                 "scripts",
                 "fitbit_pipeline_cleaning_kunal",
                 "tests",
                 "featurisation_tests.r"
                 )
       )


link_name <- "linkMoresecure"
conn <- xap.conn
sql_code <- sprintf("SELECT * FROM \"%s\"", link_name)
link <- dbGetQuery(conn, sql_code)

thresh_table <- "mvpa_thresh_11_9_4_5"
start_date <- "2019-01-10"
end_date <- "2019-01-20"
k_raw_hr_table_name <- "heart_rate_vals_2020_618_clean"
k_raw_hr_table_name_meta <- "heart_rate_vals_2020_618_clean_meta"
k_raw_steps_table_name <- "fs_new"
k_raw_steps_table_name_meta <- "fs_new_meta"
output_table_name <- "fitbit_featurise_table_2020_08"
output_table_name_meta <- "fitbit_featurise_table_meta_2020_08"

#write_fs_meta_data(conn, k_raw_steps_table_name, k_raw_steps_table_name_meta)

writeLines("############ featurise hr Data ############")


featurise_all(conn, start_date, end_date,
            k_raw_hr_table_name, k_raw_steps_table_name,
            k_raw_hr_table_name_meta, k_raw_steps_table_name_meta,
            output_table_name, link, output_table_name_meta, thresh_table)
