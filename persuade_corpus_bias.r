# clear memory if necessary
rm(list = ls())
# load required packages
library(lme4)
library(lmerTest)
library(MuMIn)
library(car)
library(tidyverse)
library(ggplot2)
library(sjPlot)
library(sjmisc)
library(contrast)
library(ggeffects)
options(scipen=999)
setwd("~/Dropbox/GSU/Feedback Comp")
mainDf <- read.csv("dfLong.csv")

#HOLISTIC ESSAY SCORES AND DEMOGRAPHIC VARIABLES
#Get a dataframe with only relevant variables
df1 <- mainDf %>%
  select(c(essay_id, holistic_score_adjudicated, gender, grade, ell, 
           race_ethnicity, economically_disadvantaged, prompt_name))
df1 <- unique(df1)

#Get lm model of grade
lmodel <- lm(holistic_score_adjudicated ~ grade, data=df1)
sum = summary(lmodel)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(lmodel)

#LME without American Indian/Alaskan Native
mainDf <- mainDf %>%
  filter(race_ethnicity != 'American Indian/Alaskan Native')

lmemodel <- lmer(holistic_score_adjudicated ~ grade + gender + economically_disadvantaged + ell + race_ethnicity + (1|prompt_name), data = mainDf)
sum = summary(lmemodel)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(lmemodel)


#LME model with discourse types
df <- read.csv('dfWide.csv') %>%
  filter(race_ethnicity != 'American Indian/Alaskan Native')


model1 <- lmer(holistic_score_adjudicated ~ Claim  + Concluding.Statement + 
                 Counterclaim + Evidence + Lead + Position +
                 Rebuttal + Unannotated + (1|prompt_name), data=df)
summary(model1)
sum <- summary(model1)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(model1)


# Model for all variables
bigModel <-  lmer(holistic_score_adjudicated ~ Claim  + ConcludingStatement + 
                    Counterclaim + Evidence + Lead + Position +
                    Rebuttal + Unannotated + grade + gender + race_ethnicity + 
                    ell + economically_disadvantaged + (1|prompt_name), data=dfWide)

summary(bigModel)
sum <- summary(bigModel)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(bigModel)


#discourse effectiveness

dfEffectiveness <- read.csv("persuade_corpus_effectiveness.csv")

dfEffectiveness$race_ethnicity <- relevel(dfEffectiveness$race_ethnicity, ref='Asian/Pacific Islander' )
  
dfEffectiveness <- dfEffectiveness %>%  
  filter(race_ethnicity != 'American Indian/Alaskan Native') %>%
  filter(ell != "Unknown")

table(dfEffectiveness$ell)

model <- lmer(discourse_effectiveness ~ grade + gender + race_ethnicity + 
                                       ell + economically_disadvantaged + student_disability_status + (1|prompt_name), 
                   data=dfEffectiveness[dfEffectiveness$discourse_type == 'Claim',])
sum <- summary(model)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(model)

model <- lmer(discourse_effectiveness ~ grade + gender + race_ethnicity + 
                     ell + economically_disadvantaged + student_disability_status + (1|prompt_name), 
                   data=dfEffectiveness[dfEffectiveness$discourse_type == 'Concluding Statement',])
sum <- summary(model)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(model)

model <- lmer(discourse_effectiveness ~ grade + gender + race_ethnicity + 
                     ell + economically_disadvantaged + student_disability_status + (1|prompt_name), 
                   data=dfEffectiveness[dfEffectiveness$discourse_type == 'Counterclaim',])
sum <- summary(model)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(model)

model <- lmer(discourse_effectiveness ~ grade + gender + race_ethnicity + 
                     ell + economically_disadvantaged + student_disability_status + (1|prompt_name), 
                   data=dfEffectiveness[dfEffectiveness$discourse_type == 'Evidence',])
sum <- summary(model)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(model)

model <- lmer(discourse_effectiveness ~ grade + gender + race_ethnicity + 
                     ell + economically_disadvantaged + student_disability_status + (1|prompt_name), 
                   data=dfEffectiveness[dfEffectiveness$discourse_type == 'Lead',])
sum <- summary(model)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(model)


model <- lmer(discourse_effectiveness ~ grade + gender + race_ethnicity + 
                     ell + economically_disadvantaged + student_disability_status + (1|prompt_name), 
                   data=dfEffectiveness[dfEffectiveness$discourse_type == 'Position',])

sum <- summary(model)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(model)

model <- lmer(discourse_effectiveness ~ grade + gender + race_ethnicity + 
                     ell + economically_disadvantaged + student_disability_status + (1|prompt_name), 
                   data=dfEffectiveness[dfEffectiveness$discourse_type == 'Rebuttal',])
sum <- summary(model)$coefficients
write.csv(sum, "table.csv")
r.squaredGLMM(model)

