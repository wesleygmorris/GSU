library(tidyr)
library(dplyr)

df <- read.csv("dfLong.csv")
dfPartial <- df %>%
  select(c(essay_id, holistic_score_adjudicated, prompt_name,
           gender, grade, ell, race_ethnicity,
           economically_disadvantaged, 
           student_disability_status,task)) %>%
  distinct()

dfCounts <- df[1:4] %>%
  select(-c(X)) %>%
  spread(key = discourse_type, value = count) 

dfWide <- merge(dfCounts, dfPartial, by="essay_id")
write.csv(dfWide, "dfWide.csv")

table(dfWide$grade)
