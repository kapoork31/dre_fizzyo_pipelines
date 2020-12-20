select count(distinct(userid)) from raw_hr_meta where date >= '2018-09-01' AND date < '2019-04-01'
select count(distinct(userid)) from hr_clean_date_meta_april_2020_new_remove70s_1 where date >='2018-09-01' AND date < '2019-04-01'

select count(*) from raw_hr_meta where date >= '2018-09-01' AND date < '2019-04-01'
select count(*) from hr_clean_date_meta_april_2020_new_remove70s_1 where date >= '2018-09-01' AND date < '2019-04-01'


select count(distinct(userid)) from raw_hr_meta where date >= '2019-01-01' AND date < '2019-04-01'
select count(distinct(userid)) from hr_clean_date_meta_april_2020_new_remove70s_1 where date >= '2019-01-01' AND date < '2019-04-01'

select count(distinct(userid)) from raw_hr_meta where date >= '2019-04-01' AND date < '2019-07-01'
select count(distinct(userid)) from hr_clean_date_meta_april_2020_new_remove70s_1 where date >= '2019-04-01' AND date < '2019-07-01'

select count(distinct(userid)) from raw_hr_meta where date >= '2019-07-01' AND date < '2019-11-01'
select count(distinct(userid)) from hr_clean_date_meta_april_2020_new_remove70s_1 where date >= '2019-07-01' AND date < '2019-11-01'






select distinct(userid) from raw_hr_meta where date >= '2019-07-01' AND date < '2019-10-01'
and userid NOT IN (select distinct(userid) from hr_clean_date_meta_april_2020 where date >= '2019-07-01' AND date < '2019-10-01');

select * from raw_hr_meta where date >= '2019-07-01' AND date <= '2019-10-01' and userid = '70c23a9b-4267-4c47-881e-cf67263dbaf3'

select value,count(value) into raw_hr_counts_freq from heart_rate_vals_2020_618 where value >= 60 and value <= 80 and time < '2018-11-09' group by value order by value
select value,count(value)  into clean_hr_counts_freq from heart_rate_vals_2020_618_clean where value >= 60 and value <= 80 and time < '2018-11-09' group by value order by value


select value,count(value) from hr_clean_date_april_2020_new_remove70s where value >= 60 and value <= 80  group by value order by value


select max(time) from hr_clean_date_april_2020_new_remove70s

select * from "fitbit_featurise_table_meta_partition_1" where userid = '46629efd-0c1e-44ee-abe7-683c7aeb72f6'

drop table fitbit_featurise_partition_1;
drop table fitbit_featurise_table_meta_partition_1;
