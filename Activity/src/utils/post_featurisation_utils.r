################################################################################
## This file contains the following fitbit post featurisation functions:
##
## post_featurisation: recieve featurised fitbit data and call
##  various post feauturisation functions
## merge_gender_age: add columns to featurise data like week, age etc
################################################################################

mvpa_column <- "mvpa_15_prev_method"

post_featurisation <- function(featurised_fitbit_table,
                                sex_table,
                                link_name,
                                output_table_name,
                                conn
                                ){

    # convert raw hr to meta with userid,name
    #
    # Args:
    #   featurised_fitbit_table: name of featurised fitbit table | string
    #   sex_table: name of gender table | string
    #   link_name: name of patient link table | string
    #   output_table_name: name of post featurised fitbit table | string
    #   conn: db connection | string

    sql_code <- sprintf("SELECT * FROM \"%s\"", link_name)
    link <- dbGetQuery(conn, sql_code)
    full_merge <- merge_gender_age(featurised_fitbit_table,
                                    sex_table,
                                    link,
                                    conn
                                )
    outliers <- outlier_func3(full_merge, c("mvpa_15_prev_method"))
    validate_data_schema(outliers, post_features_schema_2020_12)
    dbWriteTable(conn,
                    output_table_name,
                    outliers,
                    overwrite = TRUE,
                    row.names = FALSE
                )
}


merge_gender_age <- function(featurised_fitbit_table, sex_table, link, conn){

    # convert raw hr to meta with userid,name
    #
    # Args:
    #   featurised_fitbit_table: name of featurised fitbit table | string
    #   sex_table: name of gender table | string
    #   link_name: name of patient link table | string
    #   conn: db connection | string
    #   returns: demographics:
    #               featurised fitbit table with
    #               new features such as week, age etc

    sex <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", sex_table))
    fitbit_featurised <- dbGetQuery(conn,
                                        sprintf("SELECT * FROM \"%s\"",
                                        featurised_fitbit_table)
                                    )
    validate_data_schema(fitbit_featurised, features_2020_12_schema)

    demographics <- fitbit_featurised %>%
        inner_join(select(link,
                            fizzyo_hub_id,
                            study_email,
                            age_recruited,
                            date_recruited),
                            by = c("userid" = "fizzyo_hub_id")) %>%
        inner_join(select(sex, study_email, gender), by = "study_email") %>%
        mutate(gap = as.numeric(difftime(date,
                                date_recruited,
                                unit = "weeks")) / 52.25,
                decimal_age = round(age_recruited + gap, digits = 2),
                day_in_study = as.integer(date - date_recruited),
                day_of_week = weekdays(date),
                week_n = week(date),
                month = lubridate::month(date),
                year = lubridate::year(date)) %>%
        select(-c(gap))

    demographics$season[demographics$month %in% c(12, 1, 2)] <-  1
    demographics$season[demographics$month %in% c(3, 4, 5)] <-  2
    demographics$season[demographics$month %in% c(6, 7, 8)] <-  3
    demographics$season[demographics$month %in% c(9, 10, 11) ] <-  4

    demographics <- demographics %>%
        group_by(userid, year, week_n) %>%
        mutate(mvpa_week = sum(!!as.name(mvpa_column)),
                mvpa_week_score = (mvpa_week / 420)) %>%
        as.data.frame()

    demographics <- demographics %>% mutate_if(is.integer, as.numeric)
    # convert all int columns to numeric, easier for schema evaluation.

    return(demographics)
}
