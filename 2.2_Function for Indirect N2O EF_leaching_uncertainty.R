#title: "Function for Indirect N2O_Leaching"
# author: "Dr. Chih-Yu Hung"

library(tidyverse)



## Indirect N2O emissions

N2OEF_leaching <- function(SiteData, ProvinceCol = "ProvinceID", FertCol = "Fertilizer_Applied",
                           PCol = "P", PECol = "PE", CropCol = "CropID", YearCol = "Year" ,
                           RegionCol = "RegionID") {
  
  SiteData <- SiteData %>%
    rename(ProvinceID = !!sym(ProvinceCol), Fertilizer_Applied = !!sym(FertCol),
           P = !!sym(PCol), PE = !!sym(PECol), Year = !!sym(YearCol), RegionID = !!sym(RegionCol)
           ) %>%  # Dynamically rename the chosen column
    drop_na()
  
  N2OEF_leaching <- SiteData %>%
    mutate(Frac_leach = ifelse(P >= PE, Frac_max, ifelse(P/PE <= PPE_min, Frac_min, 
                                                    slope*(P/PE)-intercept))) %>%
    mutate(Leach_factor = Frac_leach *EF5) %>% #It's 0.011 in the 2019 Refinement
mutate(N2O = Leach_factor*Fertilizer_Applied)

#Calculate the EF and total emissions based on CropID and ProvinceID, regardless of year
Result_Prov_Crop <- N2OEF_leaching %>%
  group_by(ProvinceID,CropID) %>%
  summarise(#Avg.N2O = mean(N2O),
            #Tot.N2O = sum(N2O),
            #Tot.Fert = sum(Fertilizer_Applied),
            N2O.IEF = ifelse(sum(Fertilizer_Applied) >0, sum(N2O)/sum(Fertilizer_Applied),NA))

}
