SELECT date, userid FROM hr_clean_date_meta
WHERE date, userid  NOT IN (SELECT date, userid  FROM fitbit_featurise_table_date_test)

drop table duplicate_featurised_fitbit

SELECT date, userid, COUNT(*) AS Count
into duplicate_featurised_fitbit
FROM fitbit_featurise_table_2020_08
GROUP BY date, userid
ORDER BY Count DESC;

select distinct(userid) from duplicate_featurised_fitbit where count > 1


SELECT date, userid, COUNT(*) AS Count
into duplicate_featurised_fitbit_meta
FROM fitbit_featurise_table_meta_2020_08
GROUP BY date, userid


select * from duplicate_featurised_fitbit where count > 1
select distinct(userid) from duplicate_featurised_fitbit where count > 1

select count(*) from heart_rate_vals_2020_618_clean_meta where date <= '2019-06-01' 
select count(*) from fitbit_featurise_table_meta_2020_08 where date >= '2018-07-01'
select count(*) from fitbit_featurise_table_2020_12 where date <= '2019-06-01' 


select count(*) from heart_rate_vals_2020_618_meta
select count(*) from heart_rate_vals_2020_618_clean_meta


select count(*) from fitbit_featurise_table_meta_2020_08
select count(*) from heart_rate_vals_2020_618_meta

select count(*) from fitbit_featurise_table_2020_08
