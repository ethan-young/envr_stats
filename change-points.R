
t_variant_cpt_mean <- function(data, id_var, time_var, variable, ...){ 
  
  change_points <- 
    data %>%
    group_by({{id_var}}) %>%
    nest() %>% 
    mutate(
      cpt_model    = map(data, function(x) cpt.mean(x %>% pull({{variable}}), method = "PELT", ...)),
      cpt_changes  = map(cpt_model, 
                         function(x){
                           x@param.est %>% 
                             as_tibble() %>% 
                             mutate(increase = mean > lag(mean), decrease = mean < lag(mean)) %>% 
                             summarize(
                               cpt_n        = n(), 
                               cpt_decrease = sum(decrease, na.rm=T), 
                               cpt_increase = sum(increase, na.rm=T)
                             )
                         }),
      cpt_mean_data   = map(cpt_model, 
                            function(x){ 
                              x@data.set %>% 
                                as_tibble() %>%
                                mutate(
                                  time   = 1:n(),
                                  "raw" := x
                                ) %>% 
                                left_join(
                                  tibble(
                                    cpt_seg      = 1:length(x@cpts),
                                    cpt_time     = x@cpts,
                                    cpt          = T,
                                    cpt_seg_mean = x@param.est %>% as_vector() %>% unname()
                                  ),
                                  by = c("time"="cpt_time")) %>% 
                                as_tibble() %>% 
                                replace_na(list(cpt = F)) %>% 
                                fill(c(cpt_seg, cpt_seg_mean), .direction = "updown") %>% 
                                group_by(cpt_seg) %>% 
                                mutate(cpt_seglen = n()) %>% 
                                ungroup() %>% 
                                select(-x)
                            }),
      "{{variable}}_cpt_mean_data"        := map(cpt_mean_data, function(x) x %>% rename_with(~paste0(rlang::as_name(quote({{variable}})),"_", .x))),
      "{{variable}}_cpt_mean_model"       := cpt_model,
      "{{variable}}_cpt_mean_n"           := map_dbl(cpt_changes, "cpt_n"),
      "{{variable}}_cpt_mean_increase"    := map_dbl(cpt_changes, "cpt_increase"),
      "{{variable}}_cpt_mean_decrease"    := map_dbl(cpt_changes, "cpt_decrease"),
      "{{variable}}_cpt_mean_avg_seglen"  := map_dbl(cpt_mean_data, function(x){x %>% pull(cpt_seglen) %>% unique() %>% mean()}),
    )
  
  change_points %>% 
    select(-cpt_model, -cpt_changes, -cpt_mean_data)
}

t_variant_cpt_var <- function(data, id_var, time_var, variable, ...){ 
  
  change_points <- 
    data %>%
    group_by({{id_var}}) %>%
    nest() %>% 
    mutate(
      cpt_model    = map(data, function(x) cpt.var(x %>% pull({{variable}}), method = "PELT", ...)),
      cpt_changes  = map(cpt_model, 
                         function(x){
                           x@param.est %>% 
                             as_tibble() %>% 
                             mutate(increase = variance > lag(variance), decrease = variance < lag(variance)) %>% 
                             summarize(
                               cpt_n        = n(), 
                               cpt_decrease = sum(decrease, na.rm=T), 
                               cpt_increase = sum(increase, na.rm=T)
                             )
                         }),
      cpt_var_data   = map(cpt_model, 
                            function(x){ 
                              x@data.set %>% 
                                as_tibble() %>%
                                mutate(
                                  time   = 1:n(),
                                  "raw" := x
                                ) %>% 
                                left_join(
                                  tibble(
                                    cpt_seg      = 1:length(x@cpts),
                                    cpt_time     = x@cpts,
                                    cpt          = T,
                                    cpt_seg_var = x@param.est$variance %>% as_vector() %>% unname()
                                  ),
                                  by = c("time"="cpt_time")) %>% 
                                as_tibble() %>% 
                                replace_na(list(cpt = F)) %>% 
                                fill(c(cpt_seg, cpt_seg_var), .direction = "updown") %>% 
                                group_by(cpt_seg) %>% 
                                mutate(cpt_seglen = n()) %>% 
                                ungroup() %>% 
                                select(-x)
                            }),
      "{{variable}}_cpt_var_data"        := map(cpt_var_data, function(x) x %>% rename_with(~paste0(rlang::as_name(quote({{variable}})),"_", .x))),
      "{{variable}}_cpt_var_model"       := cpt_model,
      "{{variable}}_cpt_var_n"           := map_dbl(cpt_changes, "cpt_n"),
      "{{variable}}_cpt_var_increase"    := map_dbl(cpt_changes, "cpt_increase"),
      "{{variable}}_cpt_var_decrease"    := map_dbl(cpt_changes, "cpt_decrease"),
      "{{variable}}_cpt_var_avg_seglen"  := map_dbl(cpt_var_data, function(x){x %>% pull(cpt_seglen) %>% unique() %>% mean()}),
    )
  
  change_points %>% 
    select(-cpt_model, -cpt_changes, -cpt_var_data)
}

t_variant_cpt_np <- function(data, id_var, time_var, variable, ...){ 
  
  change_points <- 
    data %>%
    group_by({{id_var}}) %>%
    nest() %>% 
    mutate(
      cpt_model    = map(data, function(x) cpt.np(x %>% pull({{variable}}), method = "PELT", ...)),
      cpt_np_data   = map(cpt_model, 
                           function(x){ 
                             x@data.set %>% 
                               as_tibble() %>%
                               mutate(
                                 time   = 1:n(),
                                 "raw" := x
                               ) %>% 
                               left_join(
                                 tibble(
                                   cpt_seg      = 1:length(x@cpts),
                                   cpt_time     = x@cpts,
                                   cpt          = T,
                                   cpt_seg_var = x@param.est$variance %>% as_vector() %>% unname()
                                 ),
                                 by = c("time"="cpt_time")) %>% 
                               as_tibble() %>% 
                               replace_na(list(cpt = F)) %>% 
                               fill(c(cpt_seg, cpt_seg_var), .direction = "updown") %>% 
                               group_by(cpt_seg) %>% 
                               mutate(cpt_seglen = n()) %>% 
                               ungroup() %>% 
                               select(-x)
                           }),
      "{{variable}}_cpt_np_data"        := map(cpt_var_data, function(x) x %>% rename_with(~paste0(rlang::as_name(quote({{variable}})),"_", .x))),
      "{{variable}}_cpt_np_model"       := cpt_model,
      "{{variable}}_cpt_np_n"           := map_dbl(cpt_changes, "cpt_n"),
      "{{variable}}_cpt_np_increase"    := map_dbl(cpt_changes, "cpt_increase"),
      "{{variable}}_cpt_np_decrease"    := map_dbl(cpt_changes, "cpt_decrease"),
      "{{variable}}_cpt_np_avg_seglen"  := map_dbl(cpt_np_data, function(x){x %>% pull(cpt_seglen) %>% unique() %>% mean()}),
    )
  
  change_points %>% 
    select(-cpt_model, -cpt_changes, -cpt_var_data)
}