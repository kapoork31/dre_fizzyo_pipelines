ACT

    data
        - cleaned_act_data.csv
            - cleaned act data used in labelling tests
        - featurised_act_data.csv
            - featurised act data used in post featurising tests
        - labelled_act_data.csv
            - labelled act data used in featurising tests
        - raw_act_data.csv
            - raw act data used in cleaning tests

    legacy_utils
        - contains legacy r utils usesd in poc of prototype

    src
        - EDA
            - contains r and sql scripts used for EDA in the DRE
        - integration_test
            - clean_step_integration_test.r
                - tests full ACT cleaning process
            - label_step_integration_test.r
                - tests full ACT labelling process
            - featurise_integration_test.r
                - tests full ACT featurising process
            - post_featurise_integration_test.r
                - tests full ACT post featurising process
    
        - pipeline_runner_files
            - clean_step_kunal.r
                - runs full ACT cleaning process
            - label_step_kunal.r
                - runs full ACT labelling process
            - featurise_step_kunal.r
                - runs full ACT featurising process
            - post_featurise_step_kunal.r
                - runs full ACT post featurising process
        - pipeline_runner_files_blind
            - contains scripts to run all steps but for blind phase of the project

        test_lint_utils
            - lint_main.r
                - runs r lint on certain files in the directory
            - test_main.r
                - runs tests in tests and integration_test folder

        utils
            - act_cleaning_utils_kunal.r
                - act cleaning functions
            - act_featurisation_utils_kunal.r
                - act featurising functions
            - act_labelling_utils_kunal.r
                - act labelling functions
            - act_outliers.r
                - function to detect outliers via tukey method
            - act_post_featurising_steps_utils.r
                - act post featurising functions
            - act_schema_utils_kunal.r
                - contains expected schema for act datasets
            - act_validation_utils_kunal.r
                - functions to validate raw act data
            - pipeline_utils_kunal_2.r
                - universal pipeline orchestrator function
            - treatment_grouping_utils_kunal.r
                - functions to create meta tables

    tests
        - cleaning_tests.r
            - tests cleaning functions in act_cleaning_utils_kunal
        - featurisation_tests.r
            - tests featurisation functions in act_featurisation_utils_kunal
        - labelling_tests.r
            - tests labelling functions in act_labelling_utils_kunal
        - post_featurisation_tests.r
            - tests post featurisation functions in act_post_featurising_steps_utils


Steps Order
   
    1. Cleaning
        - detrending signal and duplicate removal

    2. Labelling
        - breath and set counting, mapping sessions to treatments

    3. Featurising
        - various treatment level breath and break based features
        - aggregation across day and week of certain features

    4. Post featurising
        - add features such as day of week, season etc to featurised data