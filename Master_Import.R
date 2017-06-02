library(dplyr)
library(readr)
library(RODBC)
library(formattable)
library(RJDBC)
library(rChoiceDialogs)
library(lubridate)
library(RCurl)

# setup environment ----
prompt_for_week <- function()
{ 
  n <- readline(prompt="Enter Week number: ")
  return(as.integer(n))
}
choose_file_directory <- function()
{
  v <- jchoose.dir()
  return(v)
}
prompt_for_year <- function()
{ 
  n <- readline(prompt="Enter Fiscal Year as YYYY: ")
  return(as.integer(n))
}
SOT_OTS_directory <- choose_file_directory()
EOW <- prompt_for_week()
fis_yr <- prompt_for_year()

# Create RODBC connection---- 
my_connect <- odbcConnect(dsn= "IP EDWP", uid= my_uid, pwd= my_pwd)
# sqlTables(my_connect, catalog = "EDWP", tableName  = "tables")
sqlQuery(my_connect, query = "SELECT  * from dbc.dbcinfo;")

# Create RJDBC connection - In Dev
#Sys.setenv(JAVA_HOME= "C:\\Users\\Ke2l8b1\\Documents\\Teradata\\JDBC_Driver\\jre-8u101-windows-x64.exe")
# drv2 <- JDBC("com.teradata.jdbc.TeraConnectionPoolDataSource", "C:\\Users\\Ke2l8b1\\Documents\\Teradata\\JDBC_Driver\\terajdbc4.jar;C:\\Users\\Ke2l8b1\\Documents\\Teradata\\JDBC_Driver\\tdgssconfig.jar")
# conn <- dbConnect(drv2, "jdbc:teradata://tdprodcop1.gap.com", my_uid, my_pwd)
# SOT_Master_RJDBC <- dbGetQuery(conn, 
#                       query = "SELECT  * from dbc.dbcinfo;")

# Query EDW ----

my_uid <- read_lines("C:\\Users\\Ke2l8b1\\Documents\\my_uid.txt")
my_pwd <- read_lines("C:\\Users\\Ke2l8b1\\Documents\\my_pwd.txt")

SOT_Master <- sqlQuery(my_connect, 
                       query = "SELECT  * from SRAA_SAND.VIEW_SOT_MASTER;")

OTS_Master <- sqlQuery(my_connect, 
                       query = "SELECT  * from SRAA_SAND.VIEW_OTS_MASTER;")
close(my_connect)
# Alternatively load from Weekly SOT ----
load(paste(choose_file_directory(), "SOT_Master_object.rtf", sep = .Platform$file.sep))
load(paste(choose_file_directory(), "OTS_Master_object.rtf", sep = .Platform$file.sep))

# source("SOT_OTS_Custom_Functions.R")
# Import static files ----
# Preferred_Vendor_new <- read_delim(file = "Preferred Vendor (new).csv", delim = "^")
# Country_description <- read_delim(file= "Country Description.txt", delim = "^")
pref_conn <- getURL("https://raw.githubusercontent.com/GSCAT/Monthly_SOT/master/Static_tables/Preferred%20Vendor%20(new).csv")

#Update Preferred_Vendor_new from Vendor concentration script 
# Preferred_Vendor_new <- read_delim(file = pref_conn, delim = "^")
Preferred_Vendor_new <- read_csv(file = pref_conn)
pref_conn <- getURL("https://raw.githubusercontent.com/GSCAT/Monthly_SOT/Static_tables/master/Country%20Description.txt")
Country_description <- read_delim(file = pref_conn, delim = "^")
# save Master Objects ----
save(SOT_Master, file = paste(SOT_OTS_directory,  'SOT_Master_object.rtf', sep = .Platform$file.sep))
save(OTS_Master, file = paste(SOT_OTS_directory,  'OTS_Master_object.rtf', sep = .Platform$file.sep ))

# load(file = paste(SOT_OTS_directory,  'SOT_Master_object.rtf', sep = .Platform$file.sep))
# load(file = paste(SOT_OTS_directory,  'OTS_Master_object.rtf', sep = .Platform$file.sep))

SOT_Data_Pulled <- SOT_Master$Data_Pulled[1]
OTS_Data_Pulled <- OTS_Master$Data_Pulled[1]
# Check date
SOT_Data_Pulled
OTS_Data_Pulled

# Remove noise from OTS and SOT Master ----
# grep("Liberty Distribution", OTS_Master$Parent_Vendor, ignore.case=TRUE)
OTS_Master <- OTS_Master %>% 
  filter(Week <= EOW,
         !grepl("Liberty Distribution", Parent_Vendor, ignore.case = TRUE),
         !grepl("dummy", Parent_Vendor, ignore.case = TRUE),
         !grepl("JPF", DC_NAME, ignore.case = TRUE)) 

SOT_Master <- SOT_Master %>% 
  filter(ShipCancelWeek <= EOW,
         SOT_Master$FISCAL_YEAR == fis_yr,
         !grepl("Liberty Distribution", Parent_Vendor, ignore.case = TRUE),
         !grepl("dummy", Parent_Vendor, ignore.case = TRUE),
         MetricShipDate <= SOT_Data_Pulled) 


# Create TOP 20 Countries Table ----
Top_20_Countries <- left_join(SOT_Master, Country_description, by= c("CountryOfOrigin"="CTRY_CD" ))
Top_20_Countries <- Top_20_Countries %>% 
  group_by(CountryOfOrigin, CTRY_DESC) %>% 
  summarise("Units by Country" = floor(sum(Units))) %>% 
  arrange(desc(`Units by Country`)) %>% 
  head(20) 
# Create top 50 Vendors ----
Top_50_Vendors <- SOT_Master %>% 
  group_by(Parent_Vendor) %>% 
  summarise("Units by Vendor" = floor(sum(Units))) %>% 
  arrange(desc(`Units by Vendor`)) %>% 
  head(50) 
# Create Monthly SOT Brand and Category Table ----
Monthly_Brand_Category_SOT <- SOT_Master %>%
  filter(SOT_Master$ShipCancelWeek <= EOW) %>%
  group_by(ShipCancelMonth, ReportingBrand, Category) %>% 
  summarise("SOTUnits" = floor(sum(Units)),
            "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
            "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
            "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
            "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
            "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
            "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
  select(ShipCancelMonth, 
         ReportingBrand, 
         Category, 
         SOTUnits, 
         SOTOnTimeUnits, 
         SOTLateUnits, 
         SOTLate5daysUnits, 
         WTSOTLateUnits, 
         PPAUnits,
         PPASOTLateUnits,
         PPASOT5daysLateUnits,
         WTPPASOTLateUnits)
 # View(Monthly_Brand_Category_SOT)

# Create Monthly SOT Brand Table ----
Monthly_Brand_SOT <- SOT_Master %>%
  filter(SOT_Master$ShipCancelWeek <= EOW) %>%
  group_by(ShipCancelMonth, ReportingBrand) %>% 
  summarise("SOTUnits" = floor(sum(Units)),
            "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
            "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
            "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
            "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
            "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
            "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
  select(ShipCancelMonth, 
         ReportingBrand, 
         SOTUnits, 
         SOTOnTimeUnits, 
         SOTLateUnits, 
         SOTLate5daysUnits, 
         WTSOTLateUnits, 
         PPAUnits,
         PPASOTLateUnits,
         PPASOT5daysLateUnits,
         WTPPASOTLateUnits)
# View(Monthly_Brand_SOT)

# Create Monthly SOT Category Table ----
Monthly_Category_SOT <- SOT_Master %>%
  filter(SOT_Master$ShipCancelWeek <= EOW) %>%
  group_by(ShipCancelMonth, Category) %>% 
  summarise("SOTUnits" = floor(sum(Units)),
            "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
            "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
            "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
            "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
            "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
            "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
  select(ShipCancelMonth, 
         Category, 
         SOTUnits, 
         SOTOnTimeUnits, 
         SOTLateUnits, 
         SOTLate5daysUnits, 
         WTSOTLateUnits, 
         PPAUnits,
         PPASOTLateUnits,
         PPASOT5daysLateUnits,
         WTPPASOTLateUnits)
# View(Monthly_Category_SOT) 

# Create Monthly Gap Inc SOT Table ----
Monthly_GapInc_SOT <- SOT_Master %>%
  filter(SOT_Master$ShipCancelWeek <= EOW) %>%
  group_by(ShipCancelMonth) %>% 
  summarise("SOTUnits" = floor(sum(Units)),
            "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
            "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
            "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
            "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
            "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
            "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
  select(ShipCancelMonth, 
         SOTUnits, 
         SOTOnTimeUnits, 
         SOTLateUnits, 
         SOTLate5daysUnits, 
         WTSOTLateUnits, 
         PPAUnits,
         PPASOTLateUnits,
         PPASOT5daysLateUnits,
         WTPPASOTLateUnits)
# View(Monthly_GapInc_SOT) 

# Create Monthly OTS Brand and Category Table ----
Monthly_Brand_Category_OTS <- OTS_Master %>%
  filter(OTS_Master$Week <= EOW) %>%
  group_by(Month_Number, ReportingBrand, Category) %>% 
  summarise("OTSUnits" = floor(sum(Units)),
            "OTSOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTSLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])), 
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "PPAOTSLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]))) %>%  
  select(Month_Number, 
         ReportingBrand, 
         Category, 
         OTSUnits, 
         OTSOnTimeUnits, 
         OTSLateUnits, 
         OTSLate5daysUnits, 
         WTOTSLateUnits, 
         PPAOTSLateUnits)
 # View(Monthly_Brand_Category_OTS)

# Create Monthly OTS Brand Table ----
Monthly_Brand_OTS <- OTS_Master %>%
  filter(OTS_Master$Week <= EOW) %>%
  group_by(Month_Number, ReportingBrand) %>% 
  summarise("OTSUnits" = floor(sum(Units)),
            "OTSOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTSLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])), 
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "PPAOTSLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]))) %>%  
  select(Month_Number, 
         ReportingBrand, 
         OTSUnits, 
         OTSOnTimeUnits, 
         OTSLateUnits, 
         OTSLate5daysUnits, 
         WTOTSLateUnits, 
         PPAOTSLateUnits)
# View(Monthly_Brand_OTS)

# Create Monthly OTS Category Table ----
Monthly_Category_OTS <- OTS_Master %>%
  filter(OTS_Master$Week <= EOW) %>%
  group_by(Month_Number, Category) %>% 
  summarise("OTSUnits" = floor(sum(Units)),
            "OTSOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTSLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])), 
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "PPAOTSLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]))) %>%  
  select(Month_Number, 
         Category, 
         OTSUnits, 
         OTSOnTimeUnits, 
         OTSLateUnits, 
         OTSLate5daysUnits, 
         WTOTSLateUnits, 
         PPAOTSLateUnits)
# View(Monthly_Category_OTS)

# Create Monthly Gap Inc OTS Table ----
Monthly_GapInc_OTS <- OTS_Master %>%
  filter(OTS_Master$Week <= EOW) %>%
  group_by(Month_Number) %>% 
  summarise("OTSUnits" = sum(Units),
            "OTSOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTSLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])), 
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "PPAOTSLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]))) %>%  
  select(Month_Number, 
         OTSUnits, 
         OTSOnTimeUnits, 
         OTSLateUnits, 
         OTSLate5daysUnits, 
         WTOTSLateUnits, 
         PPAOTSLateUnits)
 # View(Monthly_GapInc_OTS)

# Create Monthly - Preferred Vendor - New ----
Monthly_Preferred_Vendor_new <- inner_join(SOT_Master, Preferred_Vendor_new, by = c("Category"= "New Category", "Parent_Vendor"="Vendor Name")) %>% 
  filter(ShipCancelWeek <= EOW) %>%
  group_by(Category, Parent_Vendor, ShipCancelMonth) %>% 
  summarise("SOTUnits" = floor(sum(Units)),
            "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
            "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
            "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
            "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
            "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
            "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
  select(Category,
         Parent_Vendor,
         ShipCancelMonth,
         SOTUnits, 
         SOTOnTimeUnits, 
         SOTLateUnits, 
         SOTLate5daysUnits, 
         WTSOTLateUnits, 
         PPAUnits,
         PPASOTLateUnits,
         PPASOT5daysLateUnits,
         WTPPASOTLateUnits) %>% 
  arrange(ShipCancelMonth, Category)
# Create Monthly Preferred Vendor - New OTS ----
Monthly_Preferred_Vendor_New_OTS <- inner_join(OTS_Master, Preferred_Vendor_new, by = c("Category"= "New Category", "Parent_Vendor"="Vendor Name")) %>% 
  filter(Week <= EOW) %>%
  group_by(Category, Parent_Vendor,  Month_Number ) %>% 
  summarise("OTSUnits" = floor(sum(Units)),
            "OTSOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTSLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])), 
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "PPAOTSLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]))) %>%  
  select(Category, 
         Parent_Vendor,
         Month_Number,
         OTSUnits, 
         OTSOnTimeUnits, 
         OTSLateUnits, 
         OTSLate5daysUnits, 
         WTOTSLateUnits, 
         PPAOTSLateUnits) %>% 
  arrange(Month_Number, Category)
# View(Monthly_Preferred_Vendor_New_OTS)


# Create Monthly Brand and Category Combine Table ----
Monthly_Brand_Category_Combine <- left_join(Monthly_Brand_Category_SOT, Monthly_Brand_Category_OTS, by= c("ShipCancelMonth"="Month_Number", "ReportingBrand"="ReportingBrand", "Category"="Category"))
Monthly_Brand_Category_Combine <- Monthly_Brand_Category_Combine[c(1:8, 13:17,9:10,18,11:12)]

# Create Monthly SOT Brand Combine Table ----
Monthly_Brand_Combine <- left_join(Monthly_Brand_SOT, Monthly_Brand_OTS, by= c("ShipCancelMonth"="Month_Number", "ReportingBrand"="ReportingBrand"))
Monthly_Brand_Combine <- Monthly_Brand_Combine[c(1:7, 12:16, 8:9, 17, 10:11)]
# View(Monthly_Brand_Combine)

# Create Monthly SOT Category Combine Table ----
Monthly_Category_Combine <- left_join(Monthly_Category_SOT, Monthly_Category_OTS, by= c("ShipCancelMonth"="Month_Number", "Category"="Category"))
Monthly_Category_Combine <- Monthly_Category_Combine[c(1:7, 12:16, 8:9, 17, 10:11)]
# View(Monthly_Category_Combine)

# Create Monthly SOT Gap Inc Combine Table ----
Monthly_GapInc_Combine <- left_join(Monthly_GapInc_SOT, Monthly_GapInc_OTS, by= c("ShipCancelMonth"="Month_Number"))
Monthly_GapInc_Combine <- Monthly_GapInc_Combine[c(1:6, 11:15,7:8,16,9:10)]
# View(Monthly_GapInc_Combine)
# Create Preferred Vendor Combine Table ----
Preferred_Vendor_New_Combine <- left_join(Monthly_Preferred_Vendor_new, Monthly_Preferred_Vendor_New_OTS, by = c("Category"="Category", "Parent_Vendor"="Parent_Vendor", "ShipCancelMonth" = "Month_Number"))
Preferred_Vendor_New_Combine <-Preferred_Vendor_New_Combine[c(1:8, 13:17, 9:10, 18, 11:12)]


# Create Monthly - byDC ----
Monthly_by_DC <- OTS_Master %>% 
  filter(OTS_Master$Week <= EOW) %>%
  group_by(Fiscal_Month, Month_Number, DCCampus, DC_NAME, DestCtryCD) %>% 
  summarise("Total Units" = floor(sum(Units)),
            "OnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTS%" = sum(Units[Lateness=="OnTime"])/sum(Units[(Lateness=="OnTime" | Lateness == "Late")]),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])),
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "LateUnits"= floor(sum(Units[Lateness=="Late"]))) %>% 
  select(Fiscal_Month, 
         Month_Number, 
         `OnTimeUnits`, 
         `Total Units`, 
         DCCampus, 
         DC_NAME, 
         DestCtryCD, 
         `OTS%`, 
         `OTSLate5daysUnits`,
         `WTOTSLateUnits`, 
         `LateUnits`)
# Create Top 20 Countries SOT ----
Monthly_Top_20_SOT <- inner_join(SOT_Master, Top_20_Countries, by = c("CountryOfOrigin"= "CountryOfOrigin"))
Monthly_Top_20_SOT <- Monthly_Top_20_SOT %>%
  filter(ShipCancelWeek <= EOW) %>%
  group_by(ShipCancelMonth) %>% 
  summarise("SOTUnits" = floor(sum(Units)),
            "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
            "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
            "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
            "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
            "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
            "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
  select(ShipCancelMonth, 
         SOTUnits, 
         SOTOnTimeUnits, 
         SOTLateUnits, 
         SOTLate5daysUnits, 
         WTSOTLateUnits, 
         PPAUnits,
         PPASOTLateUnits,
         PPASOT5daysLateUnits,
         WTPPASOTLateUnits)
# View(Monthly_Top_20_SOT) 

# Create Monthly Top 20 OTS Table ----
Monthly_TOP_20_OTS <- inner_join(OTS_Master, Top_20_Countries, by = c("ORIGIN_COUNTRY_CODE"="CountryOfOrigin"))
Monthly_TOP_20_OTS <- Monthly_TOP_20_OTS %>%
  filter(Week <= EOW) %>%
  group_by(Month_Number) %>% 
  summarise("OTSUnits" = sum(Units),
            "OTSOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTSLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])), 
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "PPAOTSLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]))) %>%  
  select(Month_Number, 
         OTSUnits, 
         OTSOnTimeUnits, 
         OTSLateUnits, 
         OTSLate5daysUnits, 
         WTOTSLateUnits, 
         PPAOTSLateUnits)
#  View(Monthly_TOP_20_OTS)
# Create Monthly Top 20 Combine table ----
Monthly_Top_20_Combine <- inner_join(Monthly_Top_20_SOT, Monthly_TOP_20_OTS, by= c("ShipCancelMonth"="Month_Number"))
Monthly_Top_20_Combine <- Monthly_Top_20_Combine[c(1:6, 11:15,7:8,16,9:10)]
  
# Create Monthly Top 50 Vendors SOT Table ----
Monthly_Top_50_Vendors_SOT <- inner_join(SOT_Master, Top_50_Vendors, by = c("Parent_Vendor"= "Parent_Vendor"))
Monthly_Top_50_Vendors_SOT <- Monthly_Top_50_Vendors_SOT %>%
  filter(ShipCancelWeek <= EOW) %>%
  group_by(ShipCancelMonth) %>% 
  summarise("SOTUnits" = floor(sum(Units)),
            "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
            "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
            "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
            "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
            "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
            "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
  select(ShipCancelMonth, 
         SOTUnits, 
         SOTOnTimeUnits, 
         SOTLateUnits, 
         SOTLate5daysUnits, 
         WTSOTLateUnits, 
         PPAUnits,
         PPASOTLateUnits,
         PPASOT5daysLateUnits,
         WTPPASOTLateUnits)
 # View(Monthly_Top_50_Vendors_SOT) 
# Create Monthly Top 50 Vendors OTS Table ----
Monthly_Top_50_Vendors_OTS <- inner_join(OTS_Master, Top_50_Vendors, by = c("Parent_Vendor"= "Parent_Vendor"))
Monthly_Top_50_Vendors_OTS <- Monthly_Top_50_Vendors_OTS %>%
  filter(Week <= EOW) %>%
  group_by(Month_Number) %>% 
  summarise("OTSUnits" = sum(Units),
            "OTSOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTSLateUnits"= floor(sum(Units[Lateness=="Late"])),
            "OTSLate5daysUnits" = floor(sum(Units[Lateness=="Late" & Days_Late > 5])), 
            "WTOTSLateUnits" = floor(sum(Units[Lateness=="Late"]*Days_Late[Lateness=="Late" & Days_Late >=1])),
            "PPAOTSLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]))) %>%  
  select(Month_Number, 
         OTSUnits, 
         OTSOnTimeUnits, 
         OTSLateUnits, 
         OTSLate5daysUnits, 
         WTOTSLateUnits, 
         PPAOTSLateUnits)
 # View(Monthly_GapInc_OTS)


# Create Monthly SOT Top 50 Vendors Combine Table ----
Monthly_Top_50_Vendors_Combine <- left_join(Monthly_Top_50_Vendors_SOT, Monthly_Top_50_Vendors_OTS, by= c("ShipCancelMonth"="Month_Number"))
Monthly_Top_50_Vendors_Combine <- Monthly_Top_50_Vendors_Combine[c(1:6, 11:15,7:8,16,9:10)]
# View(Monthly_Top_50__Vendors_Combine)
# Create OTSvsSOT table ----
 OTS_vs <- OTS_Master %>% 
   select(NUMBER_SEQ, Month_Number, Lateness, Units) %>%
   filter(Lateness!= "Undetermined") %>% 
   rename("StockedOnTime" = Lateness) %>%
   group_by(Month_Number, StockedOnTime) %>% 
   droplevels()
 
 SOT_vs <- SOT_Master %>% 
   select(NUMBER_SEQ, ShipCancelMonth, Lateness) %>%
   filter(Lateness!="Unmeasured") %>% 
   rename("ShippedOnTime" = Lateness) %>%
   group_by(ShipCancelMonth, ShippedOnTime) %>% 
   droplevels()
 
 OTSvsSOT <- inner_join(OTS_vs, SOT_vs, by = c("NUMBER_SEQ"= "NUMBER_SEQ")) %>%
   group_by(Month_Number, StockedOnTime, ShippedOnTime) %>% 
   summarise("SumOfUnits" = floor(sum(Units)))
 
# Create Monthly Brand Top 10 Delay Combine ----
Brand_Top_Ten_Delay <-  SOT_Master %>% 
   filter(ShipCancelWeek <= EOW) %>%
   group_by(ReportingBrand, ShipCancelMonth, Parent_Vendor) %>% 
   summarise("SOTUnits" = floor(sum(Units)),
             "AdjustedSOTUnits"= floor(sum(Units[Lateness=="OnTime"]) + sum(Units[Lateness=="Late"])),
             "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
             "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
             "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
             "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
             "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
             "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
             "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
             "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
   select(
          ReportingBrand,
          ShipCancelMonth,
          Parent_Vendor,
          SOTUnits,
          AdjustedSOTUnits,
          SOTOnTimeUnits, 
          SOTLateUnits, 
          SOTLate5daysUnits, 
          WTSOTLateUnits) %>% 
   top_n(10, SOTLateUnits) %>% 
   arrange(ReportingBrand,ShipCancelMonth, desc(SOTLateUnits))
   
# Create Monthly Category Top 10 Delay Combine ----
Category_Top_Ten_Delay <-  SOT_Master %>% 
   filter(ShipCancelWeek <= EOW) %>%
   group_by(Category, ShipCancelMonth, Parent_Vendor) %>% 
   summarise("SOTUnits" = floor(sum(Units)),
             "AdjustedSOTUnits"= floor(sum(Units[Lateness=="OnTime"]) + sum(Units[Lateness=="Late"])),
             "SOTOnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
             "SOTLateUnits"= floor(sum(Units[Lateness=="Late"])),
             "SOTLate5daysUnits" = floor(sum(Units[Lateness=="Late" & DAYS_LATE > 5])), 
             "WTSOTLateUnits" = floor(sum(Units[Lateness=="Late"]*DAYS_LATE[Lateness=="Late" & DAYS_LATE >=1])),
             "PPAUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir"])),
             "PPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"])), 
             "PPASOT5daysLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE>5])),
             "WTPPASOTLateUnits" = floor(sum(Units[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late"]*DAYS_LATE[SHP_MODE_CATG_NM == "PrepaidAir" & Lateness=="Late" & DAYS_LATE >=1]))) %>%  
   select(
          Category,
          ShipCancelMonth,
          Parent_Vendor,
          SOTUnits,
          AdjustedSOTUnits,
          SOTOnTimeUnits, 
          SOTLateUnits, 
          SOTLate5daysUnits, 
          WTSOTLateUnits) %>% 
   top_n(10, SOTLateUnits) %>% 
   arrange(Category, ShipCancelMonth, desc(SOTLateUnits))
   
# Write tables ----
write_csv(Monthly_Brand_Category_Combine, path = paste(SOT_OTS_directory,  paste('Monthly_Brand_Category_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_Brand_Combine, path = paste(SOT_OTS_directory,  paste('Monthly_Brand_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_Category_Combine, path = paste(SOT_OTS_directory,  paste('Monthly_Category_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_GapInc_Combine, path = paste(SOT_OTS_directory,  paste('Monthly_GapInc_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Preferred_Vendor_New_Combine, path = paste(SOT_OTS_directory,  paste('Preferred_Vendor_New_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ), na = "0")
write_csv(Monthly_by_DC, path = paste(SOT_OTS_directory,  paste('Monthly_by_DC_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_Top_20_Combine, path = paste(SOT_OTS_directory,  paste('Monthly_Top_20_Countries_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_Top_50_Vendors_Combine, path = paste(SOT_OTS_directory,  paste('Monthly_Top_50_Vendors_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(OTSvsSOT, path = paste(SOT_OTS_directory,  paste('OTSvsSOT_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Brand_Top_Ten_Delay, path = paste(SOT_OTS_directory,  paste('Brand_Top_10_Delay_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Category_Top_Ten_Delay, path = paste(SOT_OTS_directory,  paste('Category_Top_10_Delay_WE_', EOW, '.csv',sep = ""), sep = '/' ))

# Experimental section ----
On_Time_Stock_table <- OTS_Master %>% 
  # filter(OTS_Master$Week <= 35) %>%
  group_by(Parent_Vendor, Month_Number,Week) %>%
  summarise(OnTime_Units_sum =sum(Units[Lateness=="OnTime"]), 
            Total_Units_sum=sum(Units),
            Measurable_Units_sum=sum(Units[Lateness!= "Unmeasurable"])) 

On_Time_Stock_table <- as.data.frame(On_Time_Stock_table)
# On_Time_Stock_table <- On_Time_Stock_table %>% mutate("OTS_Percent_value"= OTS_Percent_value)

On_Time_Stock_table <- OTS_Master %>% 
  #filter(OTS_Master$Week <= 30) %>%
  group_by(Parent_Vendor, Month_Number,Week) %>%
  summarise(OnTime_Units_sum =sum(Units[Lateness=="OnTime"]),
            Late_Units_sum =sum(Units[Lateness=="Late"]),
            Total_Units_sum=sum(Units),
            Measurable_Units_sum=sum(Units[Lateness!= "Unmeasurable"])) %>% 
  #mapply(OTS_percent, On_Time_Stock_table$OnTime_Units_sum, On_Time_Stock_table$Measurable_Units_sum) %>% 
mutate("OTS_Percent_value"= as.data.frame(mapply(OTS_percent, On_Time_Stock_table$OnTime_Units_sum, On_Time_Stock_table$Measurable_Units_sum)))



close(my_connect)
