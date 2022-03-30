library(tidyr)
library(dplyr)

# Load the long dataframe, with zeros filled in for discourse elements with zero counts
df <- read.csv("dfLong.csv")

# Take out dependent variables besides discourse counts and remove duplicates
dfPartial <- df %>%
  select(c(essay_id, holistic_score_adjudicated, prompt_name,
           gender, grade, ell, race_ethnicity,
           economically_disadvantaged, 
           student_disability_status,task)) %>%
  distinct()

# Make the discourse elements into wide form
dfCounts <- df[1:4] %>%
  select(-c(X)) %>%
  spread(key = discourse_type, value = count) 

# Merge the two dataframes
dfWide <- merge(dfCounts, dfPartial, by="essay_id")
write.csv(dfWide, "dfWide.csv")

table(dfWide$grade)
