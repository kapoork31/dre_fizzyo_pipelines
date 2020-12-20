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
library(dbplyr)
library(tidyverse)


source(
  file.path(
    "~",
    "scripts",
    "fitbit_pipeline_cleaning_kunal",
    "src",
    "partition",
    "partition_batch.R"
  )
)

source(
  file.path(
    "~",
    "scripts",
    "fitbit_pipeline_cleaning_kunal",
    "src",
    "utils",
    "fitbit_meta.R"
  )
)

source(
  file.path(
    "~",
    "scripts",
    "fitbit_pipeline_cleaning_kunal",
    "tests",
    "cleaning_tests.R"
  )
)

writeLines("############ clean hr Data ############")

link_name <- "linkMoresecure"
conn <- xap.conn
sql_code <- sprintf("SELECT * FROM \"%s\"", link_name)
link <- dbGetQuery(conn, sql_code)

id_partition_1 <- link$fizzyo_hub_id[1:50]

# Specify dataset names
start_date <- "2019-07-01"
end_date <- "2020-07-03"

k_raw_hr_table_name <- "heart_rate_vals_2020_618"
raw_hr_meta <- "heart_rate_vals_2020_618_meta"
conn <- xap.conn

output_table_name <- "heart_rate_vals_2020_618_clean_partition_1"
clean_hr_meta <- "heart_rate_vals_2020_618_clean_meta_partition_1"

#write_raw_hr_meta_data(conn, k_raw_hr_table_name, raw_hr_meta)

clean_all(
  conn,
  start_date,
  end_date,
  k_raw_hr_table_name,
  output_table_name,
  link,
  raw_hr_meta,
  clean_hr_meta,
  id_partition_1
)
