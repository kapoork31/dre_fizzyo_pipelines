write_raw_hr_meta_data <- function(conn, k_raw_hr_table_name, raw_hr_meta) {

    session_meta_data <- tbl(conn, k_raw_hr_table_name) %>%
        mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
        select(date, userid, time) %>%
        group_by(date, userid) %>%
        summarize() %>%
        collect()

    DBI::dbWriteTable(conn, raw_hr_meta, session_meta_data,
                        overwrite = TRUE, row.names = FALSE)

    writeLines("############ wrote raw hr meta ############")

}

write_fs_meta_data <- function(conn, fs_table, fs_table_meta) {

    writeLines("############ wrote clean hr meta ############")


    session_meta_data <- tbl(conn, fs_table) %>%
            mutate(date = to_date(as.character(time), "YYYY-MM-DD")) %>%
            select(date, userid, time) %>%
            group_by(date, userid) %>%
            summarize() %>%
        collect()

    DBI::dbWriteTable(conn, fs_table_meta, session_meta_data,
                        overwrite = TRUE, row.names = FALSE)

    writeLines("############ wrote fs meta ############")

}
