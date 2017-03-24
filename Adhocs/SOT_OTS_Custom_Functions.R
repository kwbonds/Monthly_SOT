

OTS_percent <- function(OTUnits, TotalUnits){
  OTS <- OTUnits/TotalUnits
  round(OTS*100, digits = 1)
}
