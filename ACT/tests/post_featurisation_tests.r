source(
  file.path(
    "~",
    "scripts",
    "ACT_v3",
    "act_pipeline",
    "src",
    "utils",
    "act_post_featurising_steps_utils.R"
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

library(testthat)
library(readr)
library(lubridate)

conn <- xap.conn

fully_featurised_data_table <-
    "act_featurisation_integration_test"

fully_featurised_data <-
    dbGetQuery(
        conn,
        sprintf("SELECT * FROM \"%s\"",
            fully_featurised_data_table
        )
    )

link_name <- "patient_key_id_list"
link <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", link_name))
featurised_act_table_name <- "act_featurisation_integration_test"
games <- "games_all"
sex_table <- "sex1"

test_that("merge_act_games", {

    m <- merge_act_games(featurised_act_table_name, games, link, conn)
    expect_equal(nrow(m), nrow(fully_featurised_data))

})

test_that("merge_stages_games", {

    m1 <- merge_act_games(featurised_act_table_name, games, link, conn)
    m2 <- merge_stages_games(m1, link)
    expect_equal(nrow(m1), nrow(m2))

})

test_that("actual_date", {

    m1 <- merge_act_games(featurised_act_table_name, games, link, conn)
    p_data <- link[link$patient_record_id == m1[1, "patient_id"], ]
    dates <- actual_date(p_data)

    expect_that(is.na(dates[1]), is_false())

})

test_that("merge_gender_age", {

    m1 <- merge_act_games(featurised_act_table_name, games, link, conn)
    m2 <- merge_stages_games(m1, link)
    m3 <- merge_gender_age(m2, sex_table, link, conn)
    expect_equal(nrow(m2), nrow(m3))
})

test_that("outliers", {

    m1 <- merge_act_games(featurised_act_table_name, games, link, conn)
    m2 <- merge_stages_games(m1, link)
    m3 <- merge_gender_age(m2, sex_table, link, conn)
    columns_to_check <- "breath_count"
    outlier_tagged <- outlier_func3(m3, c(columns_to_check))
    outlier_column <- paste(columns_to_check, "_is_out", sep = "")

    expect_equal(nrow(m3), nrow(outlier_tagged))
    expect_true(outlier_column %in% colnames(outlier_tagged))

})
