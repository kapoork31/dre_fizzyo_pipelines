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
        "act_post_featurising_steps_utils.r"
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
        "act_schema_utils_kunal.r"
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
        "act_outliers.R"
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
        "act_validation_utils_kunal.r"
    )
)

conn <- xap.conn
link_name <- "patient_key_id_list"

featurised_act_table_name <-
    "act_featurisation_integration_test"

games_data <- "games_all"
sex_table <- "sex1"
output_table_name <- "act_featurised_post_integration_test"

if (dbExistsTable(conn, output_table_name)) {

    dbRemoveTable(conn, output_table_name)
}

post_featurisation(
    featurised_act_table_name,
    games_data,
    sex_table,
    link_name,
    output_table_name,
    conn
)

nrow_features <- dbGetQuery(conn,
    sprintf("SELECT count(*) FROM \"%s\"",
        featurised_act_table_name
    )
)

nrow_post_features <- dbGetQuery(conn,
    sprintf("SELECT count(*) FROM \"%s\"",
        output_table_name
    )
)

test_that("test number of sessions labelled", {

    expect_equal(nrow_post_features %>% pull (count),
        nrow_features %>% pull (count))
})
