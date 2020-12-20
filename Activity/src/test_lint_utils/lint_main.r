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

############################
# lint test files

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "tests",
        "cleaning_tests.r"
    )
)


lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "tests",
        "featurisation_tests.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "tests",
        "post_featurisation_tests.r"
    )
)

############################
# lint integration test files

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "integration_test",
        "clean_step_integration.r"
    )
)


lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "integration_test",
        "featurise_step_integration.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "integration_test",
        "post_featurise_step_integration.r"
    )
)


############################
# lint pipeline_runner_files

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "pipeline_runner_files",
        "clean_hr_date.r"
    )
)


lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "pipeline_runner_files",
        "featurise_step_kunal.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "pipeline_runner_files",
        "post_featurisation_step.r"
    )
)


############################
# lint utils files

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils",
        "fitbit_cleaning_utils.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils",
        "fitbit_meta.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils",
        "fitbit_schema_utils.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils",
        "fitbit_utils_5_fix_lint.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils",
        "fitbit_validation.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils",
        "partition_batch.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils",
        "post_featurisation_utils.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "utils",
        "test_new_batch_system_data_2.r"
    )
)
############################
# lint test lint utils files

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "test_lint_utils",
        "lint_main.r"
    )
)

lintr::lint(
    file.path(
        "~",
        "scripts",
        "fitbit_pipeline_cleaning_kunal",
        "src",
        "test_lint_utils",
        "test_main.r"
    )
)
