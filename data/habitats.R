traits_habitat <- gs_read(ss = MA_sheet, ws=2)

fine_list <- lapply(strsplit(traits_habitat$habitat_fine, ","), data.frame)
names(fine_list) <- traits_habitat$scientific_name

fine_habitat_mat <- fine_list %>%
  map_df(~as.data.frame(.x), .id = "scientific_name") %>% 
  mutate(present = 1) %>% 
  spread(key = X..i.., value = present, fill = NA)
