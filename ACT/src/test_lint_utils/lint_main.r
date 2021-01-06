#############################################################
##  This file lints select files
##
#############################################################

###################################
# Load libraries and source files
########################## #########

###########################
# source utils
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

############################
# lint test files
lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "tests",
        "cleaning_tests.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "tests",
        "labelling_tests.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "tests",
        "featurisation_tests.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "tests",
        "post_featurisation_tests.R"
    )
)

############################
# lint integration test files

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "integration_test",
        "clean_step_integration_test.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "integration_test",
        "label_step_integration_test.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "integration_test",
        "featurise_integration_test.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "integration_test",
        "post_featurise_integration_test.R"
    )
)

############################
# lint pipeline_runner_files

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "pipeline_runner_files",
        "clean_step_kunal.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "pipeline_runner_files",
        "label_step_kunal.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "pipeline_runner_files",
        "featurise_step_kunal.R"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "pipeline_runner_files",
        "post_featurise_step_kunal.R"
    )
)

############################
# lint pipeline_runner_files
# need to source these files as well

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_cleaning_utils_kunal.r"
    )
)


lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_featurisation_utils_kunal.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_labelling_utils_kunal.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_outliers.r"
    )
)

lintr::lint(
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

lintr::lint(
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

lintr::lint(
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

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "pipeline_utils_kunal_2.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "treatment_grouping_utils_kunal.r"
    )
)


############################
# lint test lint utils files

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "test_lint_utils",
        "lint_main.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "test_lint_utils",
        "test_main.r"
    )
)
