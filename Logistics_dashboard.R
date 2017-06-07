library(xlsx)
library(data.table)
library(scales)

my_directory <- choose_file_directory()
BMC_table <- fread("https://github.gapinc.com/raw/SRAA/Static_tables/master/BMC.csv", stringsAsFactors = TRUE)

NA_EU_vec <- c("BDC", "FDC", "NDC", "ODC", "PDC", "SCD", "SDC", "TDC", "GUK", "CFC", "OCC", "OFC", "PUC", "WFC")
global_vec <- c("BDC", "FDC", "NDC", "ODC", "PDC", "SCD", "SDC", "TDC", "GUK", "CFC", "OFC", "WFC", "PUC", "SHD", "TFC", "HK DC", "JPD", "EAO", "EFC")
log_vec <- c("BDC", "FDC", "NDC", "ODC", "PDC", "SCD", "SDC", "TDC", "GUK", "CFC", "OCC", "OFC", "PUC", "WFC", "Total DC's (NA+EU)", "")



# Create NA/EU DC ----
log_NA_EU_DC <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number == fis_month) %>%
  group_by( Month_Number, DC_NAME) %>% 
  summarise("OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>% 
  select(
    "Entity" = DC_NAME,
          Month_Number,
          `OTS%`) %>% 
  droplevels() %>% 
  right_join(as.data.table(NA_EU_vec), by = c("Entity" = "NA_EU_vec"))

# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.table(NA_EU_vec), by = c("Entity" = "NA_EU_vec")) %>%
#   arrange(desc(Month_Number))


# Create Total DC ----
log_Total_DC <- OTS_Master %>%
  # filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number == fis_month) %>%
  group_by( Month_Number, DC_NAME) %>% 
  group_by( Month_Number,DC_NAME) %>% 
  summarise("OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>% 
  select(
    "Entity" = DC_NAME,
    Month_Number,
    `OTS%`) %>% 
  droplevels()%>% 
  right_join(as.data.frame(global_vec), by = c("Entity" = "global_vec"))

# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.frame(global_vec), by = c("Entity"  = "global_vec")) %>% 
#   arrange(desc(Month_Number))

# Create NA/EU DC ----
log_NA_EU_DC_YTD <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number <= fis_month) %>%
  group_by(DC_NAME) %>% 
  summarise("YTD OTS%" = sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE)) %>% 
  select(
    "Entity" = DC_NAME,
          `YTD OTS%`) %>%
  mutate("Month_Number" = fis_month) %>% 
  select(
    Entity,
    Month_Number,
    `YTD OTS%`) %>%
  droplevels() %>% 
  right_join(as.data.table(NA_EU_vec), by = c("Entity" = "NA_EU_vec"))

# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.table(NA_EU_vec), by = c("Entity" = "NA_EU_vec")) %>%
#   arrange(desc(Month_Number))



# Create Total DC ----
log_Total_DC_YTD <- OTS_Master %>%
  # filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number <= fis_month) %>%
  group_by(DC_NAME) %>% 
  summarise("YTD OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>%  
  select(
    "Entity" = DC_NAME,
    `YTD OTS%`) %>%
  mutate("Month_Number" = fis_month) %>% 
  select(
    Entity,
    Month_Number,
    `YTD OTS%`) %>%
  droplevels()%>% 
  right_join(as.data.frame(global_vec), by = c("Entity" = "global_vec"))

# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.frame(global_vec), by = c("Entity"  = "global_vec")) %>% 
#   arrange(desc(Month_Number))

NA_EU_join <- right_join(log_NA_EU_DC, log_NA_EU_DC_YTD, by = c("Entity", "Month_Number"))

log_Total_DC_join <- right_join(log_Total_DC, log_Total_DC_YTD, by = c("Entity", "Month_Number"))





# Create Monthly - byBrand ----
Monthly_by_Brand_Log <- OTS_Master %>% 
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Week <= EOW) %>%
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
          `LateUnits`) %>% 
  droplevels()

# Create Monthly - DestCtry ----
Monthly_by_DESTCtry_Log <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Week <= EOW) %>%
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
          `LateUnits`) %>% 
  droplevels()

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
