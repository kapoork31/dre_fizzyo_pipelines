library(DBI)
library(dplyr)
library(dbplyr)
library(lubridate)

post_featurisation <- function(featurised_act_table_name, games_data,
                                sex_table, link_name, merged_table_name,
                                conn){

    link <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", link_name))
    games_merged <- merge_act_games(featurised_act_table_name,
                                    games_data,
                                    link,
                                    conn
                                 )

    stages_merged <- merge_stages_games(games_merged, link)
    full_merge <- merge_gender_age(stages_merged, sex_table, link, conn)
    outliers <- outlier_func3(full_merge, c("breath_count"))
    validate_data_schema(outliers, act_post_featurisation_schema)
    dbWriteTable(conn, merged_table_name,
                    outliers, overwrite = TRUE,
                    row.names = FALSE
                )
}


merge_act_games  <- function(featurised_act_table_name, games_data, link, conn){

    act_merged <- data.frame()
    act  <- dbGetQuery(conn,
        sprintf("SELECT * FROM \"%s\"", featurised_act_table_name))

    validate_data_schema(act, act_features_schema)

    games <- dbGetQuery(conn,
        sprintf("SELECT * FROM \"%s\"", games_data))

    games <- merge(games, link[, c("fizzyo_hub_id", "patient_record_id")],
        by.x = "userid", by.y = "fizzyo_hub_id")

    act$games <- 0
    act$games_played <- 0

    for (p in unique(act$patient_id)){

        temppr <- act[act$patient_id == p, ]
        tempg <- games[games$patient_record_id == p, ]
        tempg_dates <- unique(tempg$date)
        temppr_dates <- unique(temppr$act_date)
        dates_match <- intersect(tempg_dates, temppr_dates)
        temppr$games[temppr$act_date %in% dates_match] <-  1

        for (d in as.list(tempg_dates)){

            unique_games <- unique(tempg[tempg$date == d, "display_name"])
            unique_games <- sort(unique_games)
            unique_games <- paste0(unique_games, collapse = ",")
            temppr$games_played[temppr$act_date == d] <- unique_games
        }

        act_merged <- rbind(temppr, act_merged)
    }

    return (act_merged)
}


actual_date <- function(p_data){

    start_date <- p_data$date_recruited + 1
    fdate <- p_data$date_feedback_start

    if (is.na(fdate)){

        fdate <- start_date + 60
    }

    if (!is.na(p_data$date_withdrawn)){

        fdate <- min(as.Date(fdate), as.Date(p_data$date_withdrawn))
    }

    g_start <- p_data$date_gaming_start
    if (is.na(g_start)){

        g_start <- start_date + 120
    }

    if (!is.na(p_data$date_withdrawn)){

        g_start <- min(as.Date(g_start), as.Date(p_data$date_withdrawn))
    }

    g_end <- p_data$date_gaming_ended
    if (is.na(g_end)){

        g_end <- g_start + 240
    }
    if (!is.na(p_data$date_withdrawn)){

        g_end <- min(as.Date(g_end), as.Date(p_data$date_withdrawn))
    }

    f_end <- p_data$date_feedback_end

    if (is.na(f_end)){

        f_end <- fdate + 365
    }
    if (!is.na(p_data$date_withdrawn)){

        f_end <- min(as.Date(f_end), as.Date(p_data$date_withdrawn))
    }

    dates <- c(start_date, fdate, g_start, g_end, f_end, p_data$date_withdrawn)
    return(dates)
}

merge_stages_games <- function(act_games, link){

    act_phases_merged <- data.frame()
    act_games$phase  <-  "blind"

    for (p in unique(act_games$patient_id)){

        p_data <- link[link$patient_record_id == p, ]
        temp <-  act_games[act_games$patient_id == p, ]
        temp <-  temp[order(temp$act_date), ]
        phase_dates <-  actual_date(p_data)
        temp$phase[temp$act_date >= phase_dates[2]] <- "feedback"
        temp$phase[temp$act_date >= phase_dates[3]] <- "gaming"
        temp$phase[temp$act_date >= phase_dates[4]] <- "gaming ended"
        temp$phase[temp$act_date >= phase_dates[5]] <- "feedback ended"

        act_phases_merged <- rbind(act_phases_merged, temp)
    }

    return(act_phases_merged)
}


merge_gender_age <- function(games_phases_merged, sex_table, link, conn){

    sex <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", sex_table))

    demographics <-  games_phases_merged %>%
        inner_join(select(link, patient_record_id, study_email,
            age_recruited, date_recruited),
            by = c("patient_id" = "patient_record_id")) %>%
        inner_join(select(sex, study_email, gender), by = "study_email") %>%
        mutate(
            gap = as.numeric(difftime(act_date,
                date_recruited, unit = "weeks")) / 52.25,
            decimal_age = round(age_recruited + gap, digits = 2),
            day_in_study = as.integer(act_date - date_recruited),
            day_of_week = weekdays(act_date),
            month = lubridate::month(act_date)) %>%
        group_by(patient_id, act_date) %>%
        mutate(day_treatment = row_number(),
        tr_start_time = strftime(treatment_start, format = "%H:%M:%S"),
        tr_start_time = as.POSIXct(paste(Sys.Date(), tr_start_time, sep = ""),
            format = "%Y-%m-%d %H:%M:%OS")) %>%
        select(-c(gap)) %>%
        as.data.frame()

    demographics$season <-  0
    demographics$season[demographics$month %in% c(12, 1, 2)] <-  1
    demographics$season[demographics$month %in% c(3, 4, 5)] <-  2
    demographics$season[demographics$month %in% c(6, 7, 8)] <-  3
    demographics$season[demographics$month %in% c(9, 10, 11) ] <-  4

    demographics <- demographics %>% mutate_if(is.integer, as.numeric)
        # convert all int columns to numeric, easier for schema evaluation.

    return(demographics)
}
