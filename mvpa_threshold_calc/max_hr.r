library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(rlang)
# load libraries

link <- xap.read_table('patient_key_id_list')
link$animal <- tolower(link$animal)
polar <- xap.read_table('polar_heartrate')
polar <- na.omit(polar)
polar <- merge(polar, link[, c("animal", "fizzyo_hub_id")], by = "animal") 

# load in link, polar data
# omit na
# add fuzzyo hub id

conn <- xap.conn

maxes_polar <- polar %>%
  group_by(fizzyo_hub_id) %>%
  summarize(max_polar_hr = max(heartrate_polar))

# max polar per person

max_fitbit <- xap.read_table("fitbit_hr_max")
# obtained by code in max_fitbit_hr.sql
# max fitbit hr per person

max_hr <- merge(maxes_polar, max_fitbit, by.x = "fizzyo_hub_id", by.y = "userid")
# merge both

xap.db.writeframe(max_hr, "max_polar_fitbit")
# save data
