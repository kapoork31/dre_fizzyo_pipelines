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
featurised_fitbit_table <- "fitbit_featurise_table_2020_12"
sex_table <- "sex1"
output_table_name <- "fitbit_featurise_table_2020_12_post_featurised"

post_featurisation(featurised_fitbit_table,
                    sex_table,
                    link_name,
                    output_table_name,
                    conn
                )
