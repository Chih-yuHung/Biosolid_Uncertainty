library(readxl)
library(tidyverse)

# To read data I need for Biosolid N2O emissions

BiosolidN <- read_xlsx("Inputs/N2OBIO_1a_Activity_Data_Full.xlsx") %>% 
  filter(Inv_Year_ID %in% c(1990, 1995, 2000, 2005, 2010, 2015))

#This is used to know the N applied is soil that Chang actually do in the MS. 
#This is being used to calculate the proportion between Chang's method and NIR method by province. 
#The proportion is then applied to the ecodistrict level to calculate uncertainty
BiosolidN_Chang <- read_csv("Inputs/Biosolid Emissions_Chang_organized.csv") %>%
   mutate(TotalN = TotalN*10^3) # t N to kg N


#To check if this is the data used in our manuscript
BiosolidN_year <- BiosolidN %>%
  group_by(Inv_Year_ID, Province_ID) %>%
  summarise(TotalN = sum(BION_Applied)) %>%
  mutate(Region = case_when(
    Province_ID %in% c("NB","NL","NS","PE") ~ "MT",
    TRUE ~ Province_ID
  )) %>%
  group_by(Inv_Year_ID, Region) %>%
  summarise(TotalN = sum(TotalN)) %>%
  ungroup()


#Join Chang's results 
N_proportion <- BiosolidN_year %>%
  left_join(BiosolidN_Chang, by = c("Inv_Year_ID", "Region")) %>%
  mutate(Proportion = TotalN.y/TotalN.x) %>% # So I know 1 proportion of NIR N to Chang's N
  select(Inv_Year_ID, Region, Proportion)

N_proportion <- N_proportion %>%
  bind_rows(
    N_proportion %>%
      filter(Region == "MT") %>%
      select(-Region) %>%
      crossing(Region = c("NB", "NL", "NS", "PE"))
  ) %>%
  arrange(Inv_Year_ID, Region)

#Join the proportion back to Ecodistrict level
BiosolidN_corrected <- BiosolidN %>%
  left_join(N_proportion, by = c("Inv_Year_ID", "Province_ID" = "Region"))  %>%
  mutate(BION_corrected = BION_Applied * Proportion) %>%
  select(Inv_Year_ID, Province_ID, Ecodistrict_ID, Crop_System_ID, BION_corrected) # Keep columns I need

#QC 1
summary(BiosolidN_corrected) #BION_corrected 593.9, 737884.4

#QC 2, with Chang's MS number
test <- BiosolidN_corrected %>%
  group_by(Inv_Year_ID) %>% summarise(BioN = sum (BION_corrected))

if(abs(test$BioN[1]- 9483277) < 0.5 & abs( test$BioN[6]- 16035596) <0.5) {
  saveRDS(BiosolidN_corrected, "Inputs/BiosolidN_corrected.rds")
} else(
  cat("Please check biosolid N")
)

