## Loading packages

library(tidyverse)
library(tidyr)
library(dplyr)
library(skimr)
library(janitor)
library(lubridate)
library(ggplot2)

## Importing csv files

dailyactivity <- read.csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
sleep <- read.csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

## Summaries

head(dailyactivity)
colnames(dailyactivity)
str(dailyactivity)
head(sleep)
colnames(sleep)
str(sleep)

## Distinct

n_distinct(dailyactivity$Id)
n_distinct(sleep$Id)

## Merge

activity_sleep_merged <- merge(dailyactivity, sleep, by=c("Id"))

n_distinct(activity_sleep_merged$Id)

head(activity_sleep_merged)
colnames(activity_sleep_merged)
str(activity_sleep_merged)

## Final Table

groupave_steps_calories_sedentary_bed_sleep <- 
  activity_sleep_merged %>% 
  group_by(Id) %>% 
  summarise(Steps = mean(TotalSteps), Cal = mean(Calories), Sedentary = mean(SedentaryMinutes), Bedtime = mean(TotalTimeInBed), Sleep =   mean(TotalMinutesAsleep))

head(groupave_steps_calories_sedentary_bed_sleep)
colnames(groupave_steps_calories_sedentary_bed_sleep)
str(groupave_steps_calories_sedentary_bed_sleep)

## Analysis

groupave_steps_calories_sedentary_bed_sleep %>% 
  summarise(mean(Steps), sd(Steps), mean(Cal), sd(Cal), mean(Sedentary), sd(Sedentary), mean(Bedtime), sd(Bedtime), mean(Sleep), sd(Sleep))

# Hypothesis 1

groupave_steps_calories_sedentary_bed_sleep %>% summarise(cor(Steps, Cal))

ggplot(data=groupave_steps_calories_sedentary_bed_sleep, mapping = aes(x=Steps, y=Cal)) + geom_point() + geom_smooth(color="green") + labs(title = "Steps vs. Calories Burnt")

# Hypotheses 2

groupave_steps_calories_sedentary_bed_sleep %>% summarise(cor(Sedentary, Cal))

ggplot(data=groupave_steps_calories_sedentary_bed_sleep, mapping = aes(x=Sedentary, y=Cal)) + geom_point() + geom_smooth(color="red") + labs(title = "Sedentary Minutes vs. Calories Burnt")

# Hypothesis 3

groupave_steps_calories_sedentary_bed_sleep %>% summarise(cor(Bedtime, Sleep))

ggplot(data=groupave_steps_calories_sedentary_bed_sleep, mapping = aes(x=Bedtime, y=Sleep)) + geom_point() + geom_smooth(color="green") + labs(title = "Time in Bed vs. Total Time Asleep")

# Hypothesis 4

groupave_steps_calories_sedentary_bed_sleep %>% summarise(cor(Sleep, Cal))

ggplot(data=groupave_steps_calories_sedentary_bed_sleep, mapping = aes(x=Sleep, y=Cal)) + geom_point() + geom_smooth(color="red") + labs(title = "Total Time Asleep vs. Calories Burnt")


