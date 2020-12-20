Activity data

    data
        - clean_fitbit_data.csv
            - cleaned fitbit data used in featurising tests
        - raw_fitbit_data.csv
            - raw fitbit data used in cleaning tests

    legacy
        - contains legacy r utils usesd in poc of prototype

    src
        - EDA
            - contains r and sql scripts used for EDA in the DRE
        - integration_test
            - clean_step_integration.r
                - tests full fitbit cleaning process
            - featurise_step_integration.r
                - tests full fitbit featurising process
            - post_featurise_step_integration.r
                - tests full fitbit post featurising process
    
        - pipeline_runner_files
            - clean_hr_date.r
                - runs full fitbit cleaning process
            - featurise_step_kunal.r
                - runs full fitbit featurising process
            - post_featurisation_step.r
                - runs full fitbit post featurising process

        test_lint_utils
            - lint_main.r
                - runs r lint on certain files in the directory
            - test_main.r
                - runs tests in tests and integration_test folder

        utils
            - fitbit_cleaning_utils.r
                - fitbit cleaning functions
            - fitbit_meta.r
                - map raw data to meta data for quicker retrieval
            - fitbit_schema_utils.r
                - contains expected schema for fitbit datasets
            - fitbit_utils_3.r
                - unlinted featurisation function for cleaned fitbit data
            - fitbit_utils_5_fix_lint.r
                - linted featurisation function for cleaned fitbit data
            - fitbit_validation.r
                - contains functions to validate hr and fs data
            - outliers.r
                - functions to detect outliers via tukey method
            - post_featurisation_utils.r
                - functions to featurise featurised data
            - test_new_batch_system_data_2.r
                - universal pipeline orchestrator functions

    tests
        - cleaning_tests.r
            - tests cleaning functions in fitbit_cleaning_utils.r
        - featurisation_tests.r
            - tests featurisation functions in fitbit_utils_5_fix_lint.r
        - post_featurisation_tests.r
            - tests post featurisation functions in post_featurisation_utils.r


Steps Order
   
    1. Cleaning
        - removes outliers data in hr data

    2. Featurising
        - various hr and fs daily level features

    3. Post featurising
        - add features such as day of week, season etc to featurised data
