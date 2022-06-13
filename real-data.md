

```r
# libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(changepoint)
library(changepoint.np)
library(lubridate)
library(devtools)
source_url("https://raw.githubusercontent.com/ethan-young/envr_stats/master/time-invariant.R")
```

```
## ℹ SHA-1 hash of file is 4d98787a2d27d995aceb0f80b1b6aaa18b11a12b
```

```r
source_url("https://raw.githubusercontent.com/ethan-young/envr_stats/master/change-points.R")
```

```
## ℹ SHA-1 hash of file is 7e95e9e7cc7dbebffe160bf9a1b25acd80783659
```

```r
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
```

```
## `summarise()` has grouped output by 'subject'. You can override using the `.groups` argument.
```

```r
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
```

```
## # A tibble: 10 × 6
##    subject subject_data      stress_range stress_iqr stress_mean stress_sd
##      <dbl> <list>                   <dbl>      <dbl>       <dbl>     <dbl>
##  1       8 <tibble [3 × 2]>             2          1       1         1    
##  2      15 <tibble [45 × 2]>            4          0       0.178     0.684
##  3      31 <tibble [22 × 2]>            2          1       0.773     0.813
##  4      49 <tibble [31 × 2]>            6          2       1.45      2.11 
##  5      71 <tibble [20 × 2]>            5          1       0.95      1.47 
##  6      75 <tibble [28 × 2]>            5          2       1         1.33 
##  7      84 <tibble [29 × 2]>            4          1       0.655     1.01 
##  8      95 <tibble [40 × 2]>            4          2       0.975     0.974
##  9     160 <tibble [40 × 2]>            3          1       0.675     0.859
## 10     161 <tibble [35 × 2]>            4          1       0.914     1.15
```

```r
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
```

```
## # A tibble: 10 × 5
## # Groups:   subject [10]
##    subject data              stressors_mean stressors_sd stressors_range
##      <dbl> <list>                     <dbl>        <dbl>           <dbl>
##  1       8 <tibble [3 × 2]>           1            1                   2
##  2      15 <tibble [45 × 2]>          0.178        0.684               4
##  3      31 <tibble [22 × 2]>          0.773        0.813               2
##  4      49 <tibble [31 × 2]>          1.45         2.11                6
##  5      71 <tibble [20 × 2]>          0.95         1.47                5
##  6      75 <tibble [28 × 2]>          1            1.33                5
##  7      84 <tibble [29 × 2]>          0.655        1.01                4
##  8      95 <tibble [40 × 2]>          0.975        0.974               4
##  9     160 <tibble [40 × 2]>          0.675        0.859               3
## 10     161 <tibble [35 × 2]>          0.914        1.15                4
```

```r
## unnest() to always retrace your steps
data3_time_invariants2 %>% unnest(data)
```

```
## # A tibble: 293 × 6
## # Groups:   subject [10]
##    subject per_week   stressors stressors_mean stressors_sd stressors_range
##      <dbl> <date>         <dbl>          <dbl>        <dbl>           <dbl>
##  1       8 2008-08-17         1          1            1                   2
##  2       8 2008-08-24         0          1            1                   2
##  3       8 2008-08-31         2          1            1                   2
##  4      15 2007-11-04         2          0.178        0.684               4
##  5      15 2007-11-11         0          0.178        0.684               4
##  6      15 2007-11-18         0          0.178        0.684               4
##  7      15 2007-11-25         0          0.178        0.684               4
##  8      15 2007-12-02         0          0.178        0.684               4
##  9      15 2007-12-09         0          0.178        0.684               4
## 10      15 2007-12-16         0          0.178        0.684               4
## # … with 283 more rows
```

```r
# Detect changepoints -----------------------------------------------------
## You can pass additional arguments 
## to the function that cpt.mean would take
data2_time_agg %>% 
  t_variant_cpt_mean(subject, date, stressors)
```

```
## # A tibble: 10 × 8
## # Groups:   subject [10]
##    subject data              stressors_cpt_mean_data stressors_cpt_mean_model stressors_cpt_mean_n stressors_cpt_mean_incre… stressors_cpt_m… stressors_cpt_m…
##      <dbl> <list>            <list>                  <list>                                  <dbl>                     <dbl>            <dbl>            <dbl>
##  1       8 <tibble [3 × 2]>  <tibble [3 × 6]>        <cpt>                                       1                         0                0             3   
##  2      15 <tibble [45 × 2]> <tibble [45 × 6]>       <cpt>                                       1                         0                0            45   
##  3      31 <tibble [22 × 2]> <tibble [22 × 6]>       <cpt>                                       1                         0                0            22   
##  4      49 <tibble [31 × 2]> <tibble [31 × 6]>       <cpt>                                       4                         2                1             7.75
##  5      71 <tibble [20 × 2]> <tibble [20 × 6]>       <cpt>                                       3                         1                1             9   
##  6      75 <tibble [28 × 2]> <tibble [28 × 6]>       <cpt>                                       1                         0                0            28   
##  7      84 <tibble [29 × 2]> <tibble [29 × 6]>       <cpt>                                       2                         0                1            14.5 
##  8      95 <tibble [40 × 2]> <tibble [40 × 6]>       <cpt>                                       1                         0                0            40   
##  9     160 <tibble [40 × 2]> <tibble [40 × 6]>       <cpt>                                       1                         0                0            40   
## 10     161 <tibble [35 × 2]> <tibble [35 × 6]>       <cpt>                                       1                         0                0            35
```

