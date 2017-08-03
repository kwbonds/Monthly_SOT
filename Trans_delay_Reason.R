library(dplyr)
library(readr)
library(xlsx)
library(tidyr)
library(dplyr)
library(ggvis)

Trans_delay_reason <-  SOT_Master %>% select(ShipCancelWeek, SHP_RSN_TYP_DESC, Units) %>% 
  filter(SOT_Master$ShipCancelWeek == EOW) %>% 
  group_by(ShipCancelWeek, SHP_RSN_TYP_DESC) %>% 
  summarize("Delayed Units" = sum(Units, na.rm = TRUE)) %>% 
  arrange(desc(`Delayed Units`))


First_choice_vec <- c()



# Experimental Viz ----

Trans_delay_reason_vis <-  SOT_Master %>%
  filter(ShipCancelWeek == EOW) %>% 
  select(SHP_RSN_TYP_DESC, Units) %>% 
  group_by(SHP_RSN_TYP_DESC) %>%  
  ggvis(~SHP_RSN_TYP_DESC, ~`Units`)  %>% 
  # guide_axis("y", subdivide = 1, values = seq(0, 2000000, by = 500000))  %>% 
  add_axis("x", title = "", properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 9))) %>% 
  add_axis("y", title = "")

Trans_delay_reason_vis

Trans_delay_reason_vis2 <-  SOT_Master %>% 
  filter(SHP_RSN_TYP_DESC != "-") %>% 
  filter(ShipCancelWeek == EOW) %>%
  droplevels() %>% 
  select(SHP_RSN_TYP_DESC, Units) %>% 
  group_by(SHP_RSN_TYP_DESC) %>%  
  ggvis(~SHP_RSN_TYP_DESC, ~`Units`) %>% 
  add_axis("x", title = "Delay Reason", properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 9)),  title_offset = 200) %>% 
  add_axis("y", title = "Units", title_offset = 80)

Trans_delay_reason_vis2

Trans_delay_reason_vis3 <-  SOT_Master %>% 
  filter(ShipCancelWeek == EOW) %>% 
  select(SHP_RSN_TYP_DESC, Units) %>% 
  group_by(SHP_RSN_TYP_DESC) %>%  
  plot(Trans_delay_reason$SHP_RSN_TYP_DESC, Trans_delay_reason$`Units`)

Trans_delay_reason_vis3


Trans_delay_reason_vis4 <-  SOT_Master %>% 
  filter(SHP_RSN_TYP_DESC != "-") %>% 
  filter(ShipCancelWeek == EOW,  Units < 10000) %>%
  droplevels() %>% 
  select(SHP_RSN_TYP_DESC, Units) %>% 
  group_by(SHP_RSN_TYP_DESC) %>%  
  ggvis(~SHP_RSN_TYP_DESC, ~`Units` ) %>%
  layer_boxplots() %>% 
  add_axis("x", title = "Delay Reason", properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 9)),  title_offset = 200) %>% 
  add_axis("y", title = "Units", title_offset = 80)


Trans_delay_reason_vis4 <-  SOT_Master %>% 
  filter(SHP_RSN_TYP_DESC != "-") %>% 
  filter(ShipCancelWeek == EOW,  Units < 10000) %>%
  droplevels() %>% 
  select(SHP_RSN_TYP_DESC, Units) %>% 
  group_by(SHP_RSN_TYP_DESC) %>%  
  ggvis(~SHP_RSN_TYP_DESC, ~`Units` ) %>%
  layer_boxplots() %>% 
  add_axis("x", title = "Delay Reason", properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 9)),  title_offset = 200) %>% 
  add_axis("y", title = "Units", title_offset = 80)

Trans_delay_reason_vis4 <-  SOT_Master_FOB %>% 
  filter(ShipCancelWeek == EOW, `Sub Reason` != "Not Tested") %>%
  droplevels() %>% 
  select(`Sub Reason`, Units) %>% 
  group_by(`Sub Reason`) %>%  
  ggvis(~`Sub Reason`, ~`Units` ) %>%
  layer_boxplots() %>% 
  add_axis("x", title = "Failure Sub-Reason", properties = axis_props(labels = list(angle = 45, align = "left", fontSize = 9)),  title_offset = 70) %>% 
  add_axis("y", title = "Units", title_offset = 80)