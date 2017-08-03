
# by Vendor ----
dir.create((file.path(SOT_OTS_directory, "Impact_files")))

vendor_df <- SOT_Master_FOB %>% 
  filter(FISCAL_YEAR == fis_yr,  !grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed= FALSE)) %>% 
  select(Category, Parent_Vendor, Units) %>% 
  group_by(Category, Parent_Vendor) %>% 
  summarise("Total Units" = sum(Units)) %>% 
  top_n(15, `Total Units`) %>% 
  arrange(Category, desc(`Total Units`))

Trans_output_Vendor <- SOT_Master_FOB %>%
  filter(ShipCancelMonth == fis_month,  !grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed= FALSE)) %>% 
  group_by(Parent_Vendor, Category) %>% 
  summarise(
    "Blank" = '',
    "SOT %" = (sum(subset(Units, Lateness == "OnTime"), na.rm = TRUE))/sum(subset(Units, Lateness != "Unmeasured")),
    "Transport_Impact" = (sum(subset(Units, `Probable Failure` == "Transportation")))/sum(subset(Units, Lateness != "Unmeasured")),
    "Air_Vendor_Impact" = (sum(subset(Units, `Probable Failure` == "Vendor" & SHIP_MODE_CD == "A" )))/sum(subset(Units, Lateness != "Unmeasured")),
    "Vendor_non_Air_Impact" = (sum(subset(Units, `Probable Failure` == "Vendor" & SHIP_MODE_CD != "A" )))/sum(subset(Units, Lateness != "Unmeasured")),
    "Unmeasured_Impact" = 1 - sum(`Transport_Impact` + `Air_Vendor_Impact` + `Vendor_non_Air_Impact` + `SOT %`),
    "Total_Impact" = sum(`Transport_Impact` + `Air_Vendor_Impact` + `Vendor_non_Air_Impact` + `Unmeasured_Impact` + `SOT %`),
    "Units" = sum(Units)) %>% 
  right_join(vendor_df, by = c("Parent_Vendor" = "Parent_Vendor", "Category" = "Category")) %>% 
  mutate("SOT Variance from Target" = `SOT %` -.95) %>% 
  group_by(Category) %>%
  # top_n(15, Units) %>% 
  # arrange(Category, desc(Units)) %>% 
  right_join(as.data.frame(cat_vec), by = c("Category" = "cat_vec"))%>% 
  select(Parent_Vendor, `Blank`, Units, `SOT %`, `SOT Variance from Target`, `Transport_Impact`, `Air_Vendor_Impact`, `Vendor_non_Air_Impact`, `Unmeasured_Impact`,  `Total_Impact`)

Trans_output_Vendor_YTD <- SOT_Master_FOB %>%
  filter(FISCAL_YEAR == fis_yr,  !grepl("FRANCHISE", ReportingBrand, ignore.case = TRUE, fixed= FALSE)) %>% 
  group_by(Parent_Vendor, Category) %>% 
  summarise(
    "Blank" = '',
    "SOT %" = (sum(subset(Units, Lateness == "OnTime"), na.rm = TRUE))/sum(subset(Units, Lateness != "Unmeasured")),
    "Transport_Impact" = (sum(subset(Units, `Probable Failure` == "Transportation")))/sum(subset(Units, Lateness != "Unmeasured")),
    "Air_Vendor_Impact" = (sum(subset(Units, `Probable Failure` == "Vendor" & SHIP_MODE_CD == "A" )))/sum(subset(Units, Lateness != "Unmeasured")),
    "Vendor_non_Air_Impact" = (sum(subset(Units, `Probable Failure` == "Vendor" & SHIP_MODE_CD != "A" )))/sum(subset(Units, Lateness != "Unmeasured")),
    "Unmeasured_Impact" = 1 - sum(`Transport_Impact` + `Air_Vendor_Impact` + `Vendor_non_Air_Impact` + `SOT %`),
    "Total_Impact" = sum(`Transport_Impact` + `Air_Vendor_Impact` + `Vendor_non_Air_Impact` + `Unmeasured_Impact` + `SOT %`),
    "Units" = sum(Units)) %>% 
  right_join(vendor_df, by = c("Parent_Vendor" = "Parent_Vendor", "Category" = "Category")) %>%
  mutate("SOT Variance from Target" = `SOT %` -.95) %>% 
  group_by(Category) %>% 
  # top_n(15, Units) %>% 
  # arrange(Category, desc(Units)) %>% 
  right_join(as.data.frame(cat_vec), by = c("Category" = "cat_vec"))%>% 
  select(Parent_Vendor, `Blank`, Units, `SOT %`, `SOT Variance from Target`, `Transport_Impact`, `Air_Vendor_Impact`, `Vendor_non_Air_Impact`, `Unmeasured_Impact`,  `Total_Impact`)
# Replace NA's with "-" ----
Trans_output_Vendor[is.na(Trans_output_Vendor)] <- "-"
Trans_output_Vendor_YTD[is.na(Trans_output_Vendor_YTD)] <- "-"

write_csv(Trans_output_Vendor, paste(SOT_OTS_directory, "Impact_files", "Trans_output_Vendor.csv", sep = .Platform$file.sep))
write_csv(Trans_output_Vendor_YTD, paste(SOT_OTS_directory,"Impact_files", "Trans_output_Vendor_YTD.csv", sep = .Platform$file.sep))
