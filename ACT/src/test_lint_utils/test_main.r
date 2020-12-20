#############################################################
##  This file runs the clean step on act data
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

#################################
#source files
utils_folder <- file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils")

for (f in list.files(utils_folder)){

    file_src <- file.path(
        utils_folder,
        f
    )

    source(file_src)
}

############################## unit tests

test_file(file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "tests",
        "cleaning_tests.r"
    )
)

test_file(file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "tests",
        "labelling_tests.r"
    )
)

test_file(file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "tests",
        "featurisation_tests.r"
    )
)

test_file(file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "tests",
        "post_featurisation_tests.r"
    )
)

########################
# integration tests

source(file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "integration_test",
        "clean_step_integration_test.r"
    )
)

source(file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "integration_test",
        "label_step_integration_test.r"
    )
)

source(file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "integration_test",
        "featurise_integration_test.r"
    )
)

source(file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "integration_test",
        "post_featurise_integration_test.r"
    )
)
