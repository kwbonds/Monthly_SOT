library(dplyr)
library(xlsx)
library(data.table)
library(scales)
library(magrittr)
library(readr)


my_directory <- choose_file_directory()
BMC_table <- fread("https://github.gapinc.com/raw/SRAA/Static_tables/master/BMC.csv", stringsAsFactors = TRUE)

NA_EU_vec <- c("BDC", "FDC", "NDC", "ODC", "PDC", "SCD", "SDC", "TDC", "GUK", "CFC", "OCC", "OFC", "PUC", "WFC")
global_vec <- c("BDC", "FDC", "NDC", "ODC", "PDC", "SCD", "SDC", "TDC", "GUK", "CFC", "OFC", "WFC", "PUC", "SHD", "TFC", "HK DC", "JPD", "EAO", "EFC")
# log_vec <- c("BDC", "FDC", "NDC", "ODC", "PDC", "SCD", "SDC", "TDC", "GUK", "CFC", "OCC", "OFC", "PUC", "WFC", "Total DC's (NA+EU)", "")

# fis_month = 4

# Create NA/EU DC ----
log_NA_EU_DC <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(!grepl("PIPERLIME", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number == fis_month) %>%
  group_by( Month_Number, DC_NAME) %>% 
  summarise("OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], 
                                         na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], 
                                                           na.rm = TRUE))) %>% 
  select(
    "Entity" = DC_NAME,
          Month_Number,
          `OTS%`) %>% 
  droplevels() %>% 
  right_join(as.data.table(NA_EU_vec), by = c("Entity" = "NA_EU_vec"))

# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.table(NA_EU_vec), by = c("Entity" = "NA_EU_vec")) %>%
#   arrange(desc(Month_Number))



# Create NA/EU DC ----
log_NA_EU_DC_YTD <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>%
  filter(!grepl("PIPERLIME", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number <= fis_month) %>%
  group_by(DC_NAME) %>% 
  summarise("YTD OTS%" = percent(sum(Units[Lateness=="OnTime"], 
                                     na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], 
                                                       na.rm = TRUE))) %>% 
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


# Create NA/EU DC Total----
log_NA_EU_DC_total <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>%
  filter(!grepl("PIPERLIME", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
#  filter(Month_Number != NA) %>% 
  filter(Month_Number == fis_month) %>%
  right_join(as.data.table(NA_EU_vec), by = c("DC_NAME" = "NA_EU_vec")) %>% 
  group_by(Month_Number) %>% 
  summarise("OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>% 
  mutate("Entity"= "") %>%
  select(
    `Entity`,
    Month_Number,
          `OTS%`) %>% 
  droplevels() 

# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.table(NA_EU_vec), by = c("Entity" = "NA_EU_vec")) %>%
#   arrange(desc(Month_Number))



# Create NA/EU DC YTD Total----
log_NA_EU_DC_YTD_total <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>%
  filter(!grepl("PIPERLIME", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number <= fis_month) %>%
  right_join(as.data.table(NA_EU_vec), by = c("DC_NAME" = "NA_EU_vec")) %>% 
  group_by(Data_Pulled) %>% 
  summarise("YTD OTS%" = percent(sum(Units[Lateness=="OnTime"], 
                                     na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], 
                                                       na.rm = TRUE))) %>% 
  select(

          `YTD OTS%`) %>%
  mutate("Entity"= "", "Month_Number" = fis_month) %>%
  select(
    `Entity`,
    Month_Number,
    `YTD OTS%`) %>%
  droplevels()

log_NA_EU_DC_total_bind <- cbind(log_NA_EU_DC_total[1 ,], log_NA_EU_DC_YTD_total[1, 3] )
log_NA_EU_DC_total_bind[1,1] <- "Total DC's (NA+EU)"
# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.table(NA_EU_vec), by = c("Entity" = "NA_EU_vec")) %>%
#   arrange(desc(Month_Number))


# Create Total DC ----
log_Total_DC <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(!grepl("PIPERLIME", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number == fis_month) %>%
  right_join(as.data.frame(global_vec), by = c("DC_NAME" = "global_vec")) %>% 
  group_by( Data_Pulled) %>% 
  summarise("OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>% 
  mutate("Entity"= "", "Month_Number" = fis_month) %>%
  select(
    `Entity`,
    Month_Number,
    `OTS%`) %>% 
  droplevels() 

# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.frame(global_vec), by = c("Entity"  = "global_vec")) %>% 
#   arrange(desc(Month_Number))

# Create Total DC ----
log_Total_DC_YTD <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(!grepl("PIPERLIME", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Month_Number <= fis_month) %>%
  right_join(as.data.frame(global_vec), by = c("DC_NAME" = "global_vec")) %>% 
  #group_by(DC_NAME) %>% 
  group_by(Data_Pulled) %>% 
  summarise("YTD OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>%  
  select(
    `YTD OTS%`) %>%
  mutate("Entity"= "", "Month_Number" = fis_month) %>% 
  select(
    Entity,
    Month_Number,
    `YTD OTS%`) %>%
  droplevels() 

# log_NA_EU_DC <- log_NA_EU_DC %>%  right_join(as.data.frame(global_vec), by = c("Entity"  = "global_vec")) %>% 
#   arrange(desc(Month_Number))



NA_EU_join <- right_join(log_NA_EU_DC, log_NA_EU_DC_YTD, by = c("Entity", "Month_Number"))

#log_Total_DC_join <- right_join(log_Total_DC, log_Total_DC_YTD, by = c("Entity", "Month_Number"))
log_Total_DC_bind <- cbind(log_Total_DC[1, ], log_Total_DC_YTD[1,3])
log_Total_DC_bind[1,1] <- "Global"



# head(Monthly_by_Brand_Log, n = 20)
#### BRAND ----
OTS_Master2 <- OTS_Master
levels(OTS_Master2$ReportingBrand) <- list("Banana Republic" = c("BR NA", "BR INTL"), 
                                            "Gap" = c("GAP INTL", "GAP NA"), 
                                            "Old Navy" = c("ON NA", "ON INTL"), 
                                            "BRFS" = c("BRFS NA"), 
                                            "GFO" = c("GO NA", "GO INTL"), 
                                            "Athleta" = "ATHLETA" )
levels(OTS_Master2$DestCtryCD) <- list("Canada" = "CA")


log_brand_vec <- c("Banana Republic", "Gap", "Old Navy", "BRFS", "GFO")

# Create Monthly - byBrand ----
by_Brand_Log <- OTS_Master2 %>% 
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>%
  filter(Month_Number == fis_month) %>%
  group_by( Month_Number, ReportingBrand) %>% 
  summarise("OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>% 
  select( 
          "Entity" = ReportingBrand,
          Month_Number,
          `OTS%`) %>% 
  right_join(as.data.table(log_brand_vec), by = c("Entity" = "log_brand_vec")) %>% 
  droplevels()

# Create Monthly - byBrand ----
by_Brand_Log_YTD <- OTS_Master2 %>% 
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>%
  filter(Month_Number <= fis_month) %>%
  group_by(ReportingBrand) %>% 
  summarise("YTD OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>%  
  select(
    "Entity" = ReportingBrand,
    `YTD OTS%`) %>%
  mutate("Month_Number" = fis_month) %>% 
  select(
    Entity,
    Month_Number,
    `YTD OTS%`) %>%
  right_join(as.data.table(log_brand_vec), by = c("Entity" = "log_brand_vec")) %>% 
  droplevels()


log_brand_join <- right_join(by_Brand_Log, by_Brand_Log_YTD, by = c("Entity", "Month_Number"))


## Create Market tables ----
market_vec_log <- c("US", "CA", "GB", "JP", "CN", "HK")

OTS_Master3 <- OTS_Master

OTS_Master3 <- OTS_Master3 %>% 
  mutate("custom_country" = case_when(DC_NAME %in% c("FDC", "ODC", "PDC", "SCD", "TDC", "OCC", "OFC", "WFC", "PUC", "NDC", "EAO") ~ "US",
                                    DC_NAME %in% c("BDC", "CFC") ~ "CA",
                                    DC_NAME %in% c("GUK") ~ "GB",
                                    DC_NAME %in% c ("JPD") ~ "JP",
                                    DC_NAME %in% c("SHD") ~ "CN",
                                    DC_NAME %in% c("HK DC") ~ "HK"))

check_customCC <- OTS_Master3 %>% 
  group_by(DC_NAME, custom_country) %>% 
  summarise(n()) %>% 
  arrange(custom_country) %T>% 
  write_csv("check.csv")

# Market ----
by_Market_Log <- OTS_Master3 %>% 
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>%
  filter(Month_Number == fis_month) %>%
  group_by(custom_country) %>%
  summarise("OnTime Units" = sum(Units[Lateness=="OnTime"], na.rm = TRUE),
    "OTS Units" = sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE),   
            "OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>% 
  # summarise("OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>%  
  select(
    "Entity" = custom_country,
    `OTS%`) %>%
  mutate("Month_Number" = fis_month) %>% 
  select(
    Entity,
    Month_Number,
    `OTS%`) %>%
  right_join(as.data.table(market_vec_log), by = c("Entity" = "market_vec_log")) %>% 
  droplevels()

# Market YTD ----
by_Market_Log_YTD <- OTS_Master3 %>% 
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>%
  #filter(Month_Number <= fis_month) %>%
  group_by(custom_country) %>% 
  summarise("YTD Units" = sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE),   
            "YTD OTS%" = scales::percent(sum(Units[Lateness=="OnTime"], na.rm = TRUE)/sum(Units[(Lateness=="OnTime" | Lateness == "Late")], na.rm = TRUE))) %>% 
  select(
    "Entity" = custom_country,
    `YTD OTS%`) %>%
  mutate("Month_Number" = fis_month) %>% 
  select(
    Entity,
    Month_Number,
    `YTD OTS%`) %>%
  right_join(as.data.table(market_vec_log), by = c("Entity" = "market_vec_log")) %>% 
  droplevels()

log_market_join <- right_join(by_Market_Log, by_Market_Log_YTD, by = c("Entity", "Month_Number"))

log_All_bind <- rbind(as.data.frame(NA_EU_join), 
                      as.data.frame(log_NA_EU_DC_total_bind), 
                      as.data.frame(log_brand_join), 
                      as.data.frame(log_market_join), 
                      as.data.frame(log_Total_DC_bind))

write_csv(log_All_bind, "Log_All_bind_July.csv")

##### ADHOC for Market
# Create Monthly - ----
Monthly_by_DC_Log <- OTS_Master %>%
  filter(!grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed=FALSE)) %>% 
  filter(Week <= EOW) %>%
  group_by( Month_Number, DC_NAME)%>% 
  summarise("Raw Units" = floor(sum(Units)),
            "Measured Units" = floor(sum(Units[(Lateness=="OnTime" | Lateness == "Late")])),
            "OnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTS%" = sum(Units[Lateness=="OnTime"])/sum(Units[(Lateness=="OnTime" | Lateness == "Late")]),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])),
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "LateUnits"= floor(sum(Units[Lateness=="Late"]))) %>% 
  select( Month_Number,
          DC_NAME,
          `OTS%`,
          `Raw Units`,
          `Measured Units`,
          `OnTimeUnits`, 
          `LateUnits`) %>% 
  droplevels()


write_csv(Monthly_by_DC_Log, "Monthly_by_DC_log.csv")
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

# write.xlsx(as.data.frame(Monthly_by_DC_Log), file = paste(my_directory, "Logistics_workbook.xlsx", sep = .Platform$file.sep), sheetName = "by DC", showNA = FALSE) 
# write.xlsx(as.data.frame(Monthly_by_Brand_Log), file = paste(my_directory, "Logistics_workbook.xlsx", sep = .Platform$file.sep), sheetName = "by Brand", append = TRUE, showNA = FALSE) 
# write.xlsx(as.data.frame(Monthly_by_DC_All), file = paste(my_directory, "Logistics_workbook.xlsx", sep = .Platform$file.sep), sheetName = "All DC", append = TRUE, showNA = FALSE) 
# 
# Logistic_dash_table <- rbind(Monthly_by_DC_Log, Monthly_by_Brand_Log)
