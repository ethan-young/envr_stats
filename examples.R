library(tidyverse)
library(lubridate)
library(changepoint)
library(changepoint.np)

source("time-invariant.R")
source("change-points.R")

# Example -----------------------------------------------------------------
stress_df <- 
  map_df(1:10, function(x){
    tibble(
      id              = x,
      time            = c(1:100),
      assault         = c(rnorm(n = 50, mean = sample(-5:5,1), sd = sample(seq(.5,3,.1),1)), rnorm(n = 50, mean = sample(5:10,1), sd = sample(seq(.5,3,.1),1))),
      income          = c(rnorm(n = 50, mean = sample(-5:5,1), sd = sample(seq(.5,3,.1),1)), rnorm(n = 50, mean = sample(5:10,1), sd = sample(seq(.5,3,.1),1))),
      food_insecurity = c(rnorm(n = 50, mean = sample(-5:5,1), sd = sample(seq(.5,3,.1),1)), rnorm(n = 50, mean = sample(5:10,1), sd = sample(seq(.5,3,.1),1)))
    )
  }) 

# Try it out --------------------------------------------------------------
## Time Invariant ----
### Add single columns----
stress_df_nested <- 
  stress_df %>% 
  group_by(id) %>% 
  nest()

stress_df_nested %>% 
  mutate(
    assault_mean        = t_invariant_mean(data, assault, na.rm = T),
    assault_sd          = t_invariant_sd(data, assault, na.rm = T),
    income_iqr          = t_invariant_iqr(data, income, na.rm = T),
    food_insecurity_var = t_invariant_stat(data, food_insecurity, var)
  )

### Add multiple columns ----
stress_df %>% 
  t_invariant_arcoss_sd(id, c(assault, income, food_insecurity), nest_data = T, na.rm = T)

stress_df %>% 
  t_invariant_across_stats(id, assault, list(mean = mean, sd = sd, mad = mad))

stress_df %>% 
  t_invariant_across_stats(id, c(food_insecurity, assault), list(mean = mean, sd = sd, mad = mad))

## Change Points ----
stress_df %>% 
  t_variant_cpt_mean(id, time, assault)

stress_df %>% 
  t_variant_cpt_var(id, time, assault)
