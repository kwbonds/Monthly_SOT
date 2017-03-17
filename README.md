SOT/OTS Monthly Notebook
================

This notebook documents the process for running the SOT/OTS monthly scripts in R. Preceding these scripts is a SQL procedure and the end output is a collection of .csv files. The following document will demonstrate the process for creating the Master tables and also the output of a single .csv file.

First we need to load our libraries.

``` r
library(dplyr)
```

    FALSE 
    FALSE Attaching package: 'dplyr'

    FALSE The following objects are masked from 'package:stats':
    FALSE 
    FALSE     filter, lag

    FALSE The following objects are masked from 'package:base':
    FALSE 
    FALSE     intersect, setdiff, setequal, union

``` r
library(readr)
library(RODBC)
library(formattable)
library(RJDBC)
```

    FALSE Loading required package: DBI

    FALSE Loading required package: rJava

``` r
library(rChoiceDialogs)
```

    FALSE This is rChoiceDialogs 1.0.6 2014-09-05

``` r
library(RCurl)
```

    FALSE Loading required package: bitops

    FALSE 
    FALSE Attaching package: 'RCurl'

    FALSE The following object is masked from 'package:rJava':
    FALSE 
    FALSE     clone

Then set up our Environment with a few functions:

``` r
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
```

The following code will open a system-independent, file chooser using Java and the function created above. It will allow you to choose the directory in which to save all files. This will sometimes fail the first time it is run. If it fails just rerun it (usually works the second time). Note: Eval is set to off to disable dynamic functionality for this notebook. This notebook will use the current working directory instead.

``` r
SOT_OTS_directory <- choose_file_directory()
```

``` r
EOW <- prompt_for_week()
```

Next we need to create a connection to EDWP. Once you have stored your username and password as my\_uid and my\_pwd; and created a DSN, connect with a string similar to this (You may need to change the below if you gave your DSN an different name). Then verify that we have successfully connected by performing a query on the dbcinfo table.

``` r
# Create RODBC connection---- 
my_connect <- odbcConnect(dsn= "IP EDWP", uid= my_uid, pwd= my_pwd)
sqlQuery(my_connect, query = "SELECT  * from dbc.dbcinfo;")
```

    ##                 InfoKey    InfoData
    ## 1               VERSION 14.10.07.10
    ## 2               RELEASE 14.10.07.09
    ## 3 LANGUAGE SUPPORT MODE    Standard

We can see that the server has returned our table! We have connected!

Then we create our master tables via query and store them as R objects.

``` r
# Query EDW ----
SOT_Master <- sqlQuery(my_connect, 
                       query = "SELECT  * from SRAA_SAND.VIEW_SOT_MASTER;")

OTS_Master <- sqlQuery(my_connect, 
                       query = "SELECT  * from SRAA_SAND.VIEW_OTS_MASTER;")
close(my_connect)
```

For the purpose of this notebook, however, we will not query the database. Instead, we will load objects already created and stored on the corporate FTP server.

Now let's download a few static files from Github and save the master objects we just created. We will need these for static mapping.

``` r
# Import static files ----
pref_conn <- getURL("https://raw.githubusercontent.com/GSCAT/Monthly_SOT/master/Preferred%20Vendor%20(new).csv")
Preferred_Vendor_new <- read_delim(file = pref_conn, delim = "^")
pref_conn <- getURL("https://raw.githubusercontent.com/GSCAT/Monthly_SOT/master/Country%20Description.txt")
Country_description <- read_delim(file = pref_conn, delim = "^")
```

``` r
# save Master Objects ----
save(SOT_Master, file = paste(SOT_OTS_directory,  'SOT_Master_object.rtf', sep = .Platform$file.sep))
save(OTS_Master, file = paste(SOT_OTS_directory,  'OTS_Master_object.rtf', sep = .Platform$file.sep ))

# load(file = paste(SOT_OTS_directory,  'SOT_Master_object.rtf', sep = .Platform$file.sep))
# load(file = paste(SOT_OTS_directory,  'OTS_Master_object.rtf', sep = .Platform$file.sep))
```

``` r
SOT_Data_Pulled <- SOT_Master$Data_Pulled[1]
OTS_Data_Pulled <- OTS_Master$Data_Pulled[1]
# Check date
SOT_Data_Pulled
```

    ## [1] "2017-01-04"

``` r
OTS_Data_Pulled
```

    ## [1] "2017-01-04"

And create a few tables by joining and summarising.

``` r
# Create TOP 20 Countries Table ----
Top_20_Countries <- left_join(SOT_Master, Country_description, by= c("CountryOfOrigin"="CTRY_CD" ))
```

    ## Warning in left_join_impl(x, y, by$x, by$y, suffix$x, suffix$y): joining
    ## character vector and factor, coercing into character vector

``` r
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
```

Check the first 6 rows of SOT Master to test that our load() was successful:

``` r
head(SOT_Master)
```

    ##   NUMBER_SEQ DEST_PO_ID ReportingBrand       Category
    ## 1     516001    WR9XNDA         GAP NA       Sweaters
    ## 2     914924    XK9LQAA         GAP NA          Knits
    ## 3     876763    XJ1SS2A        ATHLETA Category Other
    ## 4     121836    1332571   ON FRANCHISE    Accessories
    ## 5     664187    WZ6VJ7A          ON NA         Wovens
    ## 6    1531645    YW8WEAA         GAP NA       Sweaters
    ##                        Parent_Vendor Contract_Ship_Cancel SHIP_CANCEL_DATE
    ## 1 Hive & Honey - Binhai Sales Co LTD           2016-02-07       2016-02-07
    ## 2   WUXI JINMAO FOREIGN TRADE CO LTD           2016-08-11       2016-08-11
    ## 3             The Timberland Company           2016-09-01       2016-09-01
    ## 4              AIM INTERNATIONAL INC           2016-12-19       2016-12-19
    ## 5              COLUMBIA GARMENTS LTD           2016-05-14       2016-05-14
    ## 6               THATS IT SWEATER LTD           2016-11-26       2016-11-26
    ##   Units MetricShipDate StockedDate CountryOfOrigin Lateness
    ## 1    11     2016-02-07  2016-02-10              CN   OnTime
    ## 2    55     2016-08-12  2016-09-10              CN   OnTime
    ## 3   450     2016-10-19  2016-10-21              US     Late
    ## 4    14     2016-12-20  2016-12-30              CN   OnTime
    ## 5  4118     2016-05-10  2016-06-22              BD   OnTime
    ## 6   702     2016-11-21        <NA>              BD   OnTime
    ##   ShipCancelMonth ShipCancelWeek DAYS_LATE Vendor_Rank
    ## 1               1              2         0          96
    ## 2               7             28         1          58
    ## 3               8             31        48         378
    ## 4              11             47         1         232
    ## 5               4             15        -4          86
    ## 6              10             43        -5         220
    ##                    Fiscal_Month                     Quarter FISCAL_YEAR
    ## 1   FEB (RLN): 2/6/16 - 2/27/16  Q1 (RLN): 2/6/16 - 4/30/16        2016
    ## 2   AUG (RLN): 8/6/16 - 8/27/16 Q3 (RLN): 8/6/16 - 10/29/16        2016
    ## 3   SEP (RLN): 9/3/16 - 10/1/16 Q3 (RLN): 8/6/16 - 10/29/16        2016
    ## 4 DEC (RLN): 12/3/16 - 12/31/16 Q4 (RLN): 11/5/16 - 1/28/17        2016
    ## 5   MAY (RLN): 5/7/16 - 5/28/16  Q2 (RLN): 5/7/16 - 7/30/16        2016
    ## 6 NOV (RLN): 11/5/16 - 11/26/16 Q4 (RLN): 11/5/16 - 1/28/17        2016
    ##   DC_GEO_LOC MasterVendorID AGT_DEPT_ID    AGENT_DEPT OPR_BRD_STY_ID
    ## 1       <NA>        2038333       23316 S_SWTER_GP_WN         240796
    ## 2       Ohio        2010075       14240 K_SLEEP_GP_WN         321122
    ## 3       Ohio        2019085          NA          <NA>         403934
    ## 4  Hong Kong        1001704          NA          <NA>         495847
    ## 5   Gallatin        1006159       84413 W_BABYP_ON_TB         206193
    ## 6       Ohio        2044715       13319 S_SWTER_GP_IB         627206
    ##   Category_Source SALES_TERMS_CODE SHIP_MODE_CD ShipDateChoice
    ## 1              XF              FOB            A             XF
    ## 2              LP              FOB            O             LP
    ## 3            InDC              DDP            M           InDC
    ## 4              LP              FOB            O             LP
    ## 5              OC              FCA            O             OC
    ## 6              OC              FCA            O             OC
    ##   Trade_Lane_Type ProgramType BUYING_AGENT_GROUP XFR_PT_COUNTRY_CODE
    ## 1   International  TnR                       GIS                  CN
    ## 2   International  MANUAL                    GIS                  CN
    ## 3        Domestic  MANUAL                Non-GIS                  US
    ## 4   International  MANUAL                    GIS                  CN
    ## 5   International  RR                        GIS                  BD
    ## 6   International  MANUAL                    GIS                  BD
    ##   LOC_ABBR_NM SHP_MODE_CATG_NM
    ## 1        <NA>       Parcel    
    ## 2         OFC       Ocean     
    ## 3         OFC       Motor     
    ## 4       HK DC       Ocean     
    ## 5         SDC       Ocean     
    ## 6         OFC       Ocean     
    ##                                      SHP_RSN_TYP_DESC Data_Pulled
    ## 1                                                   -  2017-01-04
    ## 2                                                   -  2017-01-04
    ## 3                                                   -  2017-01-04
    ## 4 Transportation Delay-Port/Infrastructure Constraint  2017-01-04
    ## 5                                                   -  2017-01-04
    ## 6          Transportation Delay-Bad Weather Condition  2017-01-04

Once we have our master tables as R objects we need to clean them up a bit:

``` r
# Remove noise from OTS and SOT Master
OTS_Master <- OTS_Master %>% 
  filter(Week <= EOW,
         !grepl("Liberty Distribution", Parent_Vendor, ignore.case = TRUE),
         !grepl("dummy", Parent_Vendor, ignore.case = TRUE),
         !grepl("JPF", DC_NAME, ignore.case = TRUE)) 

SOT_Master <- SOT_Master %>% 
  filter(ShipCancelWeek <= EOW,
         !grepl("Liberty Distribution", Parent_Vendor, ignore.case = TRUE),
         !grepl("dummy", Parent_Vendor, ignore.case = TRUE),
         MetricShipDate <= SOT_Data_Pulled) 
```

In the actual script *Master\_Import.R* there are many tables that are generated from the above Master tables. The following code is an example of one output that will illustrate the process for all. The remaining code will be reserved for the appendix of this document.

``` r
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
```

Taking a look at our new table:

    ##    ShipCancelMonth ReportingBrand                Category  SOTUnits
    ## 1                7   BR FRANCHISE                  Wovens    23,449
    ## 2               10   ON FRANCHISE          Category Other     1,698
    ## 3                6       GAP INTL                   Knits 4,480,815
    ## 4                9          BR NA Denim and Woven Bottoms   562,753
    ## 5               10          BR NA             Accessories   251,696
    ## 6                1        ON INTL             Accessories    69,606
    ## 7                6        ON INTL                   Knits   620,042
    ## 8                9         GAP NA                  Wovens 1,086,271
    ## 9                8        GO INTL             Accessories       855
    ## 10               5          ON NA                Sweaters 1,750,728
    ##    SOTOnTimeUnits SOTLateUnits SOTLate5daysUnits WTSOTLateUnits PPAUnits
    ## 1          22,412        1,037               382          7,299       60
    ## 2           1,698            0                 0              0       96
    ## 3       3,818,057      662,758           376,733      8,305,811  341,633
    ## 4         361,259      201,494           183,446      4,586,942  192,026
    ## 5         229,654       22,042             1,425         81,519      726
    ## 6          66,895        2,711               691         41,301      260
    ## 7         476,706      143,336             7,675        515,227   19,508
    ## 8         626,307      459,964           407,882     11,393,964  331,649
    ## 9             681          174                 0            522        0
    ## 10      1,732,839       17,889             6,003        129,962    3,813
    ##    PPASOTLateUnits PPASOT5daysLateUnits WTPPASOTLateUnits
    ## 1               48                   48               432
    ## 2                0                    0                 0
    ## 3          311,095              302,689         6,683,888
    ## 4          158,376              155,011         4,118,468
    ## 5              726                  726            13,068
    ## 6                0                    0                 0
    ## 7            8,469                7,675           110,626
    ## 8          321,952              317,833         9,634,244
    ## 9                0                    0                 0
    ## 10           3,813                3,813            68,634

Lastly we need to output this file as a csv. The file will be output to the directory you set up using the Java file chooser earlier (deactivated for this notebook).

``` r
write_csv(Monthly_Brand_Category_Combine, 
          path = paste(SOT_OTS_directory,  
                       paste('Monthly_Brand_Category_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
```

That is it! The basic process is complete. The remaining tables can be created in much the same way.

Appendix
--------

``` r
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

# Create Monthly - byDC
Monthly_by_DC <- OTS_Master %>% 
  filter(OTS_Master$Week <= EOW) %>%
  group_by(Fiscal_Month, Month_Number, DCCampus, DC_NAME, DestCtryCD) %>% 
  summarise("Total Units" = floor(sum(Units)),
            "OnTimeUnits" = floor(sum(Units[Lateness=="OnTime"])),
            "OTS%" = sum(Units[Lateness=="OnTime"])/sum(Units),
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
write_csv(Monthly_Brand_Category_Combine, 
          path = paste(SOT_OTS_directory,  
                       paste('Monthly_Brand_Category_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_Brand_Combine, 
          path = paste(SOT_OTS_directory,  
                       paste('Monthly_Brand_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_Category_Combine, 
          path = paste(SOT_OTS_directory,  
                       paste('Monthly_Category_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_GapInc_Combine, 
          path = paste(SOT_OTS_directory,  
                       paste('Monthly_GapInc_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Preferred_Vendor_New_Combine, 
          path = paste(SOT_OTS_directory,  
                       paste('Preferred_Vendor_New_Combine_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_by_DC, 
          path = paste(SOT_OTS_directory,  
                       paste('Monthly_by_DC_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_Top_20_Combine, 
          path = paste(SOT_OTS_directory,  
                       paste('Monthly_Top_20_Countries_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Monthly_Top_50_Vendors_Combine, 
          path = paste(SOT_OTS_directory,  
                       paste('Monthly_Top_50_Vendors_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(OTSvsSOT, 
          path = paste(SOT_OTS_directory,  
                       paste('OTSvsSOT_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Brand_Top_Ten_Delay, 
          path = paste(SOT_OTS_directory,  
                       paste('Brand_Top_10_Delay_WE_', EOW, '.csv',sep = ""), sep = '/' ))
write_csv(Category_Top_Ten_Delay, 
          path = paste(SOT_OTS_directory,  
                       paste('Category_Top_10_Delay_WE_', EOW, '.csv',sep = ""), sep = '/' ))
```
