# Individual Stats --------------------------------------------------------
# To be used with `mutate()`
# expects a list column produced by tidyr::nest(`name` = `vars`)
# or dplyr::group_by() %>% tidyr::nest()

## Standard Set ----
t_invariant_mean <- function(nest_var, variable, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% mean(...))
}

t_invariant_sd <- function(nest_var, variable, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% sd(...))
}

t_invariant_median <- function(nest_var, variable, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% median(...))
}

t_invariant_mad <- function(nest_var, variable, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% mad(...))
}

t_invariant_min <- function(nest_var, variable, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% min(...))
}

t_invariant_max <- function(nest_var, variable, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% max(na.rm = T))
}

t_invariant_range <- function(nest_var, variable, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% range(...)) %>% diff(...)
}

t_invariant_iqr <- function(nest_var, variable, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% IQR(...))
}

## Generalized Version ----
t_invariant_stat <- function(nest_var, variable, stat, ...){ 
  map_dbl({{nest_var}}, function(x)  x %>% pull({{variable}}) %>% {{stat}}(...))
}

# Multiple columns and/or stats -------------------------------------------
## Standard Set ----
t_invariant_arcoss_mean <- function(data, id_var, variables, nest_data = T, ...){ 
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, mean, .names = "{.col}_mean", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches("_mean$"))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}

t_invariant_arcoss_sd <- function(data, id_var, variables, nest_data = T, ...){ 
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, sd, .names = "{.col}_sd", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches("_sd$"))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}

t_invariant_arcoss_median <- function(data, id_var, variables, nest_data = T, ...){ 
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, median, .names = "{.col}_median", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches("_median$"))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}

t_invariant_arcoss_mad <- function(data, id_var, variables, nest_data = T, ...){ 
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, mad, .names = "{.col}_mad", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches("_mad$"))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}

t_invariant_arcoss_min <- function(data, id_var, variables, nest_data = T, ...){ 
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, min, .names = "{.col}_min", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches("_min$"))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}

t_invariant_arcoss_max <- function(data, id_var, variables, nest_data = T, ...){ 
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, min, .names = "{.col}_max", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches("_max$"))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}

t_invariant_arcoss_range <- function(data, id_var, variables, nest_data = T, ...){ 
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, ~range(.x) %>% diff(), .names = "{.col}_range", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches("_range$"))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}

t_invariant_arcoss_iqr <- function(data, id_var, variables, nest_data = T, ...){ 
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, IQR, .names = "{.col}_iqr", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches("_iqr$"))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}

## Generalized Version ----
t_invariant_across_stats <- function(data, id_var, variables, stats, nest_data = T, ...){ 
  stat_names <- names(stats) %>% paste0("(", ., ")$") %>% paste(collapse = "|")
  data_stats <- 
    data %>%
    group_by({{id_var}}) %>%
    mutate(across({{variables}}, {{stats}}, .names = "{.col}_{.fn}", ...))
  
  if(nest_data){
    data_stats %>% 
      nest(data = c(-{{id_var}}, -matches(stat_names))) %>% 
      select({{id_var}}, data, everything())
  } else{
    data_stats
  }
}
