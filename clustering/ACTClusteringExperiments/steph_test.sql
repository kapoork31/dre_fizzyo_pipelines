SELECT * FROM breath_test_all_data_02_cleaned WHERE "sessionId" = 'cea33673-d59c-4f9f-840b-557d539d0b3a'

select * from pressure_raw_vals where patient_record_id = '917d53c4-69d7-41c4-8810-85f697782650' order by time LIMIT 500

select "pressureDetrend" from labelled_oct_dec_2018 where "patientId" = '2c5f35f9-68ba-4469-aae3-9fad7e3dd274' and "treatmentId" = '5bfdb744-35d8-42c8-93a2-dffc9001fe01' order by time