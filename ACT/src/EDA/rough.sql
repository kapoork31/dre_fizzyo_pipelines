drop table duplicate_pr_clean

SELECT "actDate","treatmentId", COUNT(*) AS Count
into duplicate_pr_clean
FROM act_featurised_blind_2020_12_v2
GROUP BY "actDate","treatmentId"
ORDER BY Count DESC;

select * from duplicate_pr_clean where count > 1
select count(*) from labeled_treatment_meta_blind
select count(distinct("treatmentId")) from labeled_treatment_meta where "treatmentId" not in (select distinct("treatmentId") from act_featurised_july_2020)

select count(distinct("treatmentId")) from labeled_treatment_meta
select count (*) from act_featurised_blind_del_new_features_21_10_2020
select * from duplicate_pr_clean

select distinct(count) from dupldrop table duplicate_pr_clean

SELECT "actDate","treatmentId", COUNT(*) AS Count
into duplicate_pr_clean
FROM act_featurised_sept_2020_no_treatments_with_0_breaths
GROUP BY "actDate","treatmentId"
ORDER BY Count DESC;

select * from duplicate_pr_clean where count > 1

select count(distinct("treatmentId")) from labeled_treatment_meta where "treatmentId" not in (select distinct("treatmentId") from act_featurised_july_2020)

select count(distinct("treatmentId")) from labeled_treatment_meta
select count (*) from act_featurised_blind_del_new_features_21_10_2020
select * from duplicate_pr_clean

select distinct(count) from duplicate_pr_clean

select count(distinct("treatmentId")) from act_featurised_sept_2020_no_treatments_with_0_breaths
select count(distinct("treatmentId")) from act_labeled_new_blind_br_thresh_8
select count(distinct("treatmentId")) from act_featurised_meta

select count(*) from heart_rate_vals_2020_618
select count(*) from fs_new
select sum(value) from fs_new

select max("actDate") from act_featurised_july_2020
select min("breathCount") from act_featurised_sept_2020_no_treatments_with_0_breaths

select count(*) from act_labeled_new where "treatmentId" = '82a2634f-caf6-4743-8e76-857df348a1ee'

select count(distinct("treatmentId")) from labeled_treatment_meta_blind
select count (*) from act_featurised_post_featurisation_gaming

select min("breathCount") from act_featurised_blind_del

delete from act_featurised_sept_2020_no_treatments_with_0_breaths where "treatmentId" = '82a2634f-caf6-4743-8e76-857df348a1ee'

icate_pr_clean

select count(distinct("treatmentId")) from act_featurised_sept_2020_no_treatments_with_0_breaths
select count(distinct("treatmentId")) from labeled_treatment_meta
select count(distinct("treatmentId")) from act_featurised_meta

select count(*) from heart_rate_vals_2020_618
select count(*) from fs_new
select sum(value) from fs_new

select max("actDate") from act_featurised_july_2020
select min("breathCount") from act_featurised_sept_2020_no_treatments_with_0_breaths

select count(*) from act_labeled_new where "treatmentId" = '82a2634f-caf6-4743-8e76-857df348a1ee'

select count(distinct("treatmentId")) from labeled_treatment_meta_blind
select count (*) from act_featurised_post_featurisation_gaming

select min("minBreathAmplitude") from act_featurised_blind_2020_12_v3

delete from act_featurised_sept_2020_no_treatments_with_0_breaths where "treatmentId" = '82a2634f-caf6-4743-8e76-857df348a1ee'

select "breathCount" from act_featurised_sept_2020_no_treatments_with_0_breaths where "treatmentId" = 'b692405c-2d0c-46d1-8877-58d4d18062e2'

select distinct(patient_id) from act_clean_2020_12

select * from pressure_raw_sessions_meta_act_clean_2020_12 where date <= '2018-10-01' and date >= '2018-9-01'
select * from act_clean_meta_2020_12 where date <= '2018-10-01' and date >= '2018-09-01'

select * from act_featurised_2020_12 