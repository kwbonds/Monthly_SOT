library(xlsx)

my_directory <- choose_file_directory()

log_vec <- c("BDC", "FDC", "NDC", "ODC", "PDC", "SCD", "SDC", "TDC", "GUK", "CFC", "OCC", "OFC", "PUC", "WFC")

# Create Monthly - byDC ----
Monthly_by_DC_Log <- OTS_Master %>% 
  filter(OTS_Master$Week <= EOW) %>%
  group_by( Month_Number, DCCampus, DC_NAME) %>% 
  summarise("Raw Units" = floor(sum(Units)),
            "Measured Units" = floor(sum(Units[(Lateness=="OnTime" | Lateness == "Late")])),
            "OnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTS%" = sum(Units[Lateness=="OnTime"])/sum(Units[(Lateness=="OnTime" | Lateness == "Late")]),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])),
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "LateUnits"= floor(sum(Units[Lateness=="Late"]))) %>% 
  select(
          Month_Number,
          "Entity" = DCCampus, 
          DC_NAME,
          `OTS%`,
         `Raw Units`,
         `Measured Units`,
         `OnTimeUnits`, 
         `LateUnits`)
Monthly_by_DC_All <- Monthly_by_DC_Log
Monthly_by_DC_Log <- Monthly_by_DC_Log %>%  right_join(as.data.frame(log_vec), by = c("DC_NAME" = "log_vec")) %>% 
  arrange(desc(Month_Number))

# Create Monthly - byBrand ----
Monthly_by_Brand_Log <- OTS_Master %>% 
  filter(OTS_Master$Week <= EOW) %>%
  group_by( Month_Number, ReportingBrand) %>% 
  summarise("Raw Units" = floor(sum(Units)),
            "Measured Units" = floor(sum(Units[(Lateness=="OnTime" | Lateness == "Late")])),
            "OnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTS%" = sum(Units[Lateness=="OnTime"])/sum(Units[(Lateness=="OnTime" | Lateness == "Late")]),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])),
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "LateUnits"= floor(sum(Units[Lateness=="Late"]))) %>% 
  select( Month_Number,
          "Entity" = ReportingBrand,
          `OTS%`,
          `Raw Units`,
          `Measured Units`,
          `OnTimeUnits`, 
          `LateUnits`)

# Create Monthly - DestCtry ----
Monthly_by_DESTCtry_Log <- OTS_Master %>% 
  filter(OTS_Master$Week <= EOW) %>%
  group_by( Month_Number, DestCtryCD)%>% 
  summarise("Raw Units" = floor(sum(Units)),
            "Measured Units" = floor(sum(Units[(Lateness=="OnTime" | Lateness == "Late")])),
            "OnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTS%" = sum(Units[Lateness=="OnTime"])/sum(Units[(Lateness=="OnTime" | Lateness == "Late")]),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])),
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "LateUnits"= floor(sum(Units[Lateness=="Late"]))) %>% 
  select( Month_Number,
          DestCtryCD,
          `OTS%`,
          `Raw Units`,
          `Measured Units`,
          `OnTimeUnits`, 
          `LateUnits`)

# 
# Monthly_by_Market_Log <- OTS_Master %>% 
#   filter(OTS_Master$Week <= EOW) %>%
#   group_by( Month_Number, ReportingBrand) %>% 
#   summarise("Raw Units" = floor(sum(Units)),
#             "Measured Units" = floor(sum(Units[(Lateness=="OnTime" | Lateness == "Late")])),
#             "OnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
#             "OTS%" = sum(Units[Lateness=="OnTime"])/sum(Units[(Lateness=="OnTime" | Lateness == "Late")]),
#             "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])),
#             "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
#             "LateUnits"= floor(sum(Units[Lateness=="Late"]))) %>% 
#   select(`Raw Units`,
#          `Measured Units`,
#          Month_Number,
#          `ReportingBrand`,
#          `OnTimeUnits`, 
#          `OTS%`, 
#          `OTSLate5daysUnits`,
#          `WTOTSLateUnits`, 
#          `LateUnits`)

write.xlsx(as.data.frame(Monthly_by_DC_Log), file = paste(my_directory, "Logistics_workbook.xlsx", sep = .Platform$file.sep), sheetName = "by DC", showNA = FALSE) 
write.xlsx(as.data.frame(Monthly_by_Brand_Log), file = paste(my_directory, "Logistics_workbook.xlsx", sep = .Platform$file.sep), sheetName = "by Brand", append = TRUE, showNA = FALSE) 
write.xlsx(as.data.frame(Monthly_by_DC_All), file = paste(my_directory, "Logistics_workbook.xlsx", sep = .Platform$file.sep), sheetName = "All DC", append = TRUE, showNA = FALSE) 

Logistic_dash_table <- rbind(Monthly_by_DC_Log, Monthly_by_Brand_Log)
