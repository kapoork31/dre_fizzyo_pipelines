#############################################################
##  This file runs tests on utils folder code
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
library(testthat)


###########################
# source utils
utils_folder <- file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils"
)

for (f in list.files(utils_folder)){

    file_src <- file.path(
        utils_folder,
        f
    )

    source(file_src)
}


############################## unit tests

source(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "tests",
        "cleaning_tests.r"
    )
)

source(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "tests",
        "featurisation_tests.r"
    )
)

source(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "tests",
        "post_featurisation_tests.r"
    )
)

########################
# integration tests

source(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "integration_test",
        "clean_step_integration.r"
    )
)

source(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "integration_test",
        "featurise_step_integration.r"
    )
)

source(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "integration_test",
        "post_featurise_step_integration.r"
    )
)
