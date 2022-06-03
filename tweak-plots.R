
ggmap(map_minneapolis) + 
  geom_polygon(data = ethan_circles %>% filter(date > ymd("2013-12-31"), date < ymd("2015-01-01")), aes(x = rlon, y = rlat, group = centers), fill = "skyblue1", color = "skyblue2", inherit.aes = F, alpha = .3) +
  geom_point(data = ethan_crime %>% filter(address_inside == "Outside", date > ymd("2013-12-31"), date < ymd("2015-01-01")), aes(x = long.y, y = lat.y,  fill = address), shape = 21, color = "white", alpha = .2, size = .85) +
  geom_point(data = ethan_crime %>% filter(address_inside != "Outside", date > ymd("2013-12-31"), date < ymd("2015-01-01")), aes(x = long.y, y = lat.y,  fill = address_inside, shape = inside_km), color = "white", size = 1.75) +
  geom_point(data = ethan_crime %>% filter(date > ymd("2013-12-31"), date < ymd("2015-01-01")) %>%  distinct(address, .keep_all = T), aes(x = long.x, y = lat.x, fill = address), size = 3, shape = 23, color = "white") +
  guides(alpha = "none") +
  scale_alpha_manual(values = c(.1,1)) +
  scale_color_manual(values = c("white","black")) +
  scale_shape_manual(values = c(21,23)) +
  scale_fill_viridis_d("Address", option = "C", direction = -1) +
  guides(alpha = "none", shape = "none", color = "none") +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.line  = element_blank()
  )



ethan_crime %>% 
  mutate(
    address = str_remove(address, ",.*"),
    address = fct_reorder(address, date),
    per_month = floor_date(date, "month")
  ) %>% 
  group_by(per_month, address) %>% 
  summarize(n_crimes = n()) %>% 
  group_by(address) %>% 
  mutate(
    move_in   = first(per_month), 
    move_out  = last(per_month),
    event     = case_when(move_in == per_month ~ "Move", T ~ NA_character_)
  ) %>% 
  ggplot(aes(x = per_month, y = n_crimes)) +
  geom_line(aes(x = per_month, y = n_crimes), color = "black", inherit.aes = F) +
  #geom_point(size = 0, shape = 21, fill = "white", stroke = 1) +
  #geom_point(aes(x = per_month, y = n_crimes, fill = address), size = 2, shape = 21, color = "white", stroke = 1, inherit.aes = F) +
  geom_smooth(size = .5, method = "loess", se = T) +
  ggtitle("Ethan's Neighborhood Violent Crime Exposure") +
  scale_x_date("\nTime", date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y", expand = c(0.05,0.05)) +
  scale_y_continuous("Number of Violent Crimes\n") +
  guides(color = guide_legend("Address"), fill = guide_legend("Address")) +
  theme_bw() +
  theme(
    axis.line = element_line(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  )
