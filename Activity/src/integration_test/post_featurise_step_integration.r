#############################################################
# post featurisation integration test
# tests post featurisation process works as expected
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
                "post_featurisation_utils.R"
                )
        )

source(file.path("~",
                "scripts",
                "fitbit_pipeline_cleaning_kunal",
                "src",
                "utils",
                "fitbit_validation.R"
                )
        )

source(file.path("~",
                "scripts",
                "fitbit_pipeline_cleaning_kunal",
                "src",
                "utils",
                "outliers.R"
                )
        )


conn <- xap.conn
link_name <- "patient_key_id_list"
featurised_fitbit_table <- "fitbit_featurise_table_integration_test"
sex_table <- "sex1"
output_table_name <- "fitbit_post_featurise_table_integration_test"

if (dbExistsTable(conn, output_table_name)) {

    dbRemoveTable(conn, output_table_name)
}

post_featurisation(featurised_fitbit_table,
                    sex_table,
                    link_name,
                    output_table_name,
                    conn
                )

nrow_post_featurised_days <- dbGetQuery(conn,
    sprintf("SELECT count(distinct(date)) FROM \"%s\"",
        output_table_name
    )
)

nrow_featurised_days <- dbGetQuery(conn,
    sprintf("SELECT count(distinct(date)) FROM \"%s\"",
        featurised_fitbit_table
    )
)

test_that("test number of days featurised", {

    expect_equal(nrow_post_featurised_days %>% pull (count),
        nrow_featurised_days %>% pull (count))
})
