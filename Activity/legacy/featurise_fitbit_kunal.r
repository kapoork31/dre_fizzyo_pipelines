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

thresh_table <- 'mvpa_thresh_11_9_4_5'
start_date <- "2019-01-10"
end_date <- "2019-01-20"
kRawHRTableName <- "heart_rate_vals_2020_618_clean"
kRawHRTableName_meta <- "heart_rate_vals_2020_618_clean_meta"
kRawStepsTableName <- "fs_new"
kRawStepsTableName_meta <- 'fs_new_meta'
outputTableName <- 'fitbit_featurise_table_2020_08'
outputTableNameMeta = 'fitbit_featurise_table_meta_2020_08'

#WriteFsMetaData(conn, kRawStepsTableName,kRawStepsTableName_meta)

writeLines("############ featurise hr Data ############")


featuriseAll(conn,startDate,endDate,kRawHRTableName,kRawStepsTableName,kRawHRTableName_meta,kRawStepsTableName_meta,outputTableName,link,outputTableNameMeta)



hr_clean_meta <- kRawHRTableName_meta
fs_meta <- kRawStepsTableName_meta
input_table_name_hr <- kRawHRTableName
input_table_name_fs <- kRawStepsTableName
outputTableName <- 'fitbit_featurise_table_date_test_new_may_2020'
outputTableNameMeta <- 'fitbit_featurise_table_date_test_new_may_2020_meta'
kRawHRTableName <- "hr_clean_date_april_2020_new_remove70s_1"
kRawHRTableName_meta <- "hr_clean_date_meta_april_2020_new_remove70s_1"
startDate <- "2019-01-10"
endDate <- "2019-01-20"
patient <- 'cf518ed4-ed66-449d-a8ca-14cee26e80a9'