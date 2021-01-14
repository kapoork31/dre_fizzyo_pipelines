SELECT userid, MAX(value) as max_fitbit into fitbit_hr_max
FROM heart_rate_vals_2020_12_clean 
GROUP BY userid;
