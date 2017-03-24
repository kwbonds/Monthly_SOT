library(dplyr)
library(readr)
library(xlsx)
library(tidyr)

# SOT_Master <- sqlQuery(my_connect, 
#                        query = "SELECT  * from SRAA_SAND.VIEW_SOT_MASTER_FIS_2016;")
# close(my_connect)
# 
# save(SOT_Master, file = paste(SOT_OTS_directory,  'SOT_Master_object.rtf', sep = .Platform$file.sep))

Ship_Choice_status <- read.csv("Ship_Choice_Status.csv")

# Brand_ship_Choice <- SOT_Master %>%
#   subset(ReportingBrand == "ATHLETA" & Lateness == "Late" & ShipCancelWeek == EOW) %>% 
#   group_by(ReportingBrand, SHIP_MODE_CD, Trade_Lane_Type, SALES_TERMS_CODE, ShipDateChoice) %>% 
#   summarise("Vendor Units" = sum(`Units`)) %>% 
#   arrange(desc(`Vendor Units`))

# ship_Choice_Table<- SOT_Master %>%
#   group_by(SHIP_MODE_CD, Trade_Lane_Type, SALES_TERMS_CODE, ShipDateChoice) %>% 
#   summarise("Vendor Units" = sum(`Units`)) %>% 
#   arrange(desc(`Vendor Units`))



First_Choice <- Ship_Choice_status %>% 
  subset(`Ship_Choice_Status` == 1)
# Ship Choice ----
ship_Choice <- SOT_Master %>%
  # subset(Lateness == "Late") %>%
  group_by(ReportingBrand, Contract_Ship_Cancel, SHIP_MODE_CD, Trade_Lane_Type, SALES_TERMS_CODE, ShipDateChoice, SHP_RSN_TYP_DESC, Lateness) %>% 
  summarise("Vendor Units" = sum(`Units`)) %>% 
  right_join(Ship_Choice_status, by = c("SHIP_MODE_CD" = "SHIP_MODE_CD", "Trade_Lane_Type" = "Trade_Lane_Type", "SALES_TERMS_CODE" = "SALES_TERMS_CODE", "ShipDateChoice"="ShipDateChoice")) %>% 
  mutate("Transportation Delay Reason" = ifelse(SHP_RSN_TYP_DESC != "-", "Delay Reason", "")) %>%
  arrange(desc(`Vendor Units`))

Perf_ship_1st_choice <- ship_Choice %>%
  # subset(ship_Choice$Ship_Choice_Status == 1) %>% 
  subset(Contract_Ship_Cancel >= "2016-01-31" & Contract_Ship_Cancel<= "2017-01-28") %>% 
  group_by(Contract_Ship_Cancel) %>% 
  summarise("OnTimeUnits"= sum(`Vendor Units`[Lateness=="OnTime"]),
            "LateUnits" = sum(`Vendor Units`[Lateness=="Late"]),
            "Unmeasured Units" = sum(`Vendor Units`[Lateness=="Unmeasured"]),
            "SOT % (first Choice)" = (sum(`Vendor Units`[Lateness=="OnTime" & Ship_Choice_Status == 1 ], na.rm = TRUE)/ sum(`Vendor Units`[Lateness != "Unmeasured" & Ship_Choice_Status == 1], na.rm = TRUE))*100,
            "SOT % (all)" = (sum(`Vendor Units`[Lateness=="OnTime"], na.rm = TRUE)/ sum(`Vendor Units`[Lateness != "Unmeasured"], na.rm = TRUE))*100,
            "Impact" = sum(`SOT % (first Choice)`) - sum(`SOT % (all)`),
            "Units"= sum(`Vendor Units`)) %>%
  # group_by(ShipCancelWeek) %>% 
  #  summarise("SOT % to Grand Total" = (sum(`OnTimeUnits`)/ sum(`Vendor Units`))*100) %>% 
  filter(`Units`>= 10000)

rownames(Perf_ship_1st_choice) <- Perf_ship_1st_choice$Contract_Ship_Cancel 
Perf_ship_1st_choice <- Perf_ship_1st_choice %>%  select(`SOT % (first Choice)`, `SOT % (all)`, `Impact`)
Weekly_SOT <- as.xts(Perf_ship_1st_choice)
dygraph(Perf_ship_1st_choice) %>% 
  # dySeries(label = "SOT %", name="SOT Performance") %>% 
  dyRangeSelector()


Perf_ship_choice <- ship_Choice %>%
  # subset(ship_Choice$Ship_Choice_Status == 2) %>% 
  group_by(Contract_Ship_Cancel, Ship_Choice_Status) %>% 
  summarise("OnTimeUnits"= sum(`Vendor Units`[Lateness=="OnTime"]),
            "LateUnits" = sum(`Vendor Units`[Lateness=="Late"]),
            "SOT %" = (sum(`Vendor Units`[Lateness=="OnTime"])/ sum(`Vendor Units`))*100,
            "Units"= sum(`Vendor Units`)) %>%
  # group_by(ShipCancelWeek) %>% 
  #  summarise("SOT % to Grand Total" = (sum(`OnTimeUnits`)/ sum(`Vendor Units`))*100) %>% 
  filter(`Units`>= 10000)



# # For January ----
# ship_Choice <- SOT_Master %>%
#   subset(ShipCancelMonth == 12) %>% 
#   # subset(Lateness == "OnTime") %>%
#   group_by(ReportingBrand, Category, ShipCancelMonth, SHIP_MODE_CD, Trade_Lane_Type, SALES_TERMS_CODE, ShipDateChoice, SHP_RSN_TYP_DESC, Lateness) %>% 
#   summarise("Vendor Units" = sum(`Units`)) %>% 
#   right_join(Ship_Choice_status, by = c("SHIP_MODE_CD" = "SHIP_MODE_CD", "Trade_Lane_Type" = "Trade_Lane_Type", "SALES_TERMS_CODE" = "SALES_TERMS_CODE", "ShipDateChoice"="ShipDateChoice")) %>%
#   mutate("Transportation Delay Reason" = SHP_RSN_TYP_DESC != "-") %>% 
#   arrange(desc(`Vendor Units`))
# 
# Perf_ship_choice <- ship_Choice %>%
#   group_by(ReportingBrand, Category, ShipCancelMonth) %>% 
#   summarise("OnTimeUnits"= sum(`Vendor Units`[Lateness=="OnTime"]),
#             "LateUnits" = sum(`Vendor Units`[Lateness=="Late"]),
#             "SOT %" = (sum(`Vendor Units`[Lateness=="OnTime"])/ sum(`Vendor Units`))*100,
#             "Units"= sum(`Vendor Units`),
#             "Unit late Trans Delay" = sum(`Vendor Units`[`Transportation Delay Reason` == TRUE & Lateness == "Late"], na.rm = TRUE)) %>%
#   # group_by(ShipCancelWeek) %>% 
#   #  summarise("SOT % to Grand Total" = (sum(`OnTimeUnits`)/ sum(`Vendor Units`))*100) %>% 
#   filter(`Units`>= 10000)



write.xlsx(as.data.frame(Perf_ship_choice), file = paste(SOT_OTS_directory, "Ship_Choice2.xlsx", sep = .Platform$file.sep), sheetName = "By brand Category")
# ship_Choice <- SOT_Master %>%
#   subset(Lateness == "Late") %>% 
#   group_by(SHIP_MODE_CD, Trade_Lane_Type, SALES_TERMS_CODE, ShipDateChoice) %>% 
#   summarise("Vendor Units" = sum(`Units`)) %>% 
#   arrange(desc(`Vendor Units`))



Brand_EOW <- SOT_Master %>%
  subset(ReportingBrand == "ATHLETA" & Lateness == "Late" & ShipCancelWeek == EOW)


write.xlsx(as.data.frame(Brand_ship_Choice), file = paste(SOT_OTS_directory, "Athleta Choice.xlsx", sep = .Platform$file.sep), sheetName = "By Ship Choice Matrix")
write.xlsx(as.data.frame(Brand_EOW), file = paste(SOT_OTS_directory, "Athleta Choice.xlsx", sep = .Platform$file.sep), sheetName = "Late DPOs curr week", append = TRUE)
