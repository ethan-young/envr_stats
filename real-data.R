# libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(changepoint)
library(changepoint.np)
library(lubridate)
library(devtools)
source_url("https://raw.githubusercontent.com/ethan-young/envr_stats/master/time-invariant.R")
source_url("https://raw.githubusercontent.com/ethan-young/envr_stats/master/change-points.R")

# Data --------------------------------------------------------------------
data1_raw <- 
  read_excel("data/smith_example.xlsx") %>% 
  rename_with(tolower)

data2_time_agg <- 
  data1_raw %>% 
  mutate(
    date         = ymd(date), 
    per_day      = date %>% floor_date("day"),
    per_week     = date %>% floor_date("week"),
    per_month    = date %>% floor_date("month"),
    per_biannual = date %>% floor_date("halfyear"),
    per_annual   = date %>% floor_date("year"),
    stressor     = potential_stressor
  ) %>% 
  group_by(subject, per_week) %>% 
  summarize(stressors = sum(potential_stressor)) %>% 
  ungroup()

# Add individual stats incrementally --------------------------------------
data3_time_invariants1 <- 
  data2_time_agg %>% 
  nest(subject_data = -subject) %>% 
  mutate(
    stress_range = t_invariant_range(subject_data, stressors),
    stress_iqr   = t_invariant_iqr(subject_data, stressors),
    stress_mean  = t_invariant_mean(subject_data, stressors),
    stress_sd    = t_invariant_sd(subject_data, stressors)
  )

data3_time_invariants1

# Add stats all at once ---------------------------------------------------
data3_time_invariants2 <- 
  data2_time_agg %>% 
  t_invariant_across_stats(
    id_var = subject, 
    variables = stressors, 
    stats = list(mean = mean, sd = sd, range = ~diff(range(.x), iqr = IQR)),
    nest_data = T
  )

## Nice nested dataset
data3_time_invariants2

## unnest() to always retrace your steps
data3_time_invariants2 %>% unnest(data)

# Detect changepoints -----------------------------------------------------
## You can pass additional arguments 
## to the function that cpt.mean would take
data2_time_agg %>% 
  t_variant_cpt_mean(subject, date, stressors)

