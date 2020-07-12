## ON THE FRINGE -- LDRD PROJECT -- SOM MINERALIZATION
## Kaizad F. Patel
## 12-Jul-2020

source("code/0-packages.R")


# load files --------------------------------------------------------------

flux_data = read.csv("data/respiration.csv")

## clean the data
flux_clean = 
  flux_data %>% 
  select(Temperature, Clay, Moisture, date, `time.hour.`, time, `Sample..`, `CO2_1_ppm`, `CO2_2_ppm`, skip) %>% 
  mutate(skip = if_else(skip=="skip", "skip", NA_character_)) %>% 
  filter(is.na(skip)) %>% 
  rename(hour = `time.hour.`,
         core = `Sample..`) %>% 
  mutate(datetime = mdy_hm(paste(date, hour)),
         date = mdy(date))

# calculate flux ----------------------------------------------------------
headspace = 12.713075
headspace2 = headspace/1000
R = 82.05 #cm3-atm/g mole- K
soil_wt = 42 #g

flux = 
  flux_clean %>% 
  mutate(
    #PV=nRT
    mmol_air = ((1*headspace)/(R*(Temperature+273)))*1000,
    ug_CO2_C = mmol_air*CO2_1_ppm*12.011/1000,
    mg_CO2_C = ug_CO2_C/1000) 



#




# output ------------------------------------------------------------------

write.csv(flux_clean, "data/processed/flux.csv", row.names = F)

