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
## fixed parameters for calculation ----
headspace = 12.713075
headspace2 = headspace/1000
R = 82.05 #cm3-atm/g mole- K
soil_wt = 42 #g


## calculate elapsed time for flux ----
  ## elapsed time = time since the last flush
  ## the calc is different for t0-t6 and for t7-t8
  ## for t0-t6: cores were capped at a single time after flushing:

flushed = 
  tribble(
    ~time, ~date, ~hour,
    "t0", "2016-09-09", "12:00",
    "t1", "2016-09-09", "14:37",
    "t2", "2016-09-09", "22:44",
    "t3", "2016-09-10", "10:30",
    "t4", "2016-09-11", "11:00",
    "t5", "2016-09-12", "07:42",
    "t6", "2016-09-13", "11:55"
  ) %>% 
  mutate(datetime = ymd_hm(paste(date, hour)),
         date = ymd(date),
         # set core=0 for cleanliness
         core=0
         )

  ## for t7 and t8, each core was closed at a different time, immediately after the previous measurement
  ## so for t7 core 1, time_flux began at the measurement time of 76 core 1, etc.

  ## use the lag() function to calculate this 
  ## we will calculate this for the entire set for convenience, and then filter only the t7-t8 values

flux_times = 
  flux_clean %>% 
  # add the flushed file
  bind_rows(flushed) %>% # use bind_rows() because of unequal columns
  arrange((datetime)) %>% 
  # for t7-t8 calculations
  group_by(core) %>% 
  mutate(Diff = (difftime(datetime, lag(datetime), units = "mins"))) %>% 
  # for t0-t6 calculations
  group_by(time) %>% 
  mutate(flushedtime = difftime(datetime, min(datetime), units = "mins")) %>% 
  ungroup () %>% 
  # now pull appropriate values for each set
  mutate(flux_mins = case_when(time %in% c("t0", "t1", "t2", "t3", "t4", "t5") ~ flushedtime,
                               ## APS said add 1 minute for t6
                               time %in% "t6" ~ flushedtime+1,
                               time %in% c("t7", "t8") ~ Diff),
         flux_hr = as.double(flux_mins/60)) %>% 
  select(-Diff, -flushedtime) %>% 
  # now calculate elapsed time for the incubation
  ungroup() %>% 
  mutate(incub_days = as.double(difftime(date, min(date), units = "days")))

## calculate fluxes ----
flux = 
  flux_times %>% 
  filter(!core==0) %>% 
  # remove 32 and 33 for all except t0
  mutate(remove_3132 = (core==31&time=="t0")|(core==32&time=="t0")|(core<31),
    #PV=nRT
    mmol_air = ((1*headspace)/(R*(Temperature+273)))*1000,
    CO2C_ug = mmol_air*CO2_1_ppm*12.011/1000,
    CO2C_mg = CO2C_ug/1000,
    CO2C_ug_g_hr = (CO2C_ug/soil_wt) * 1/flux_hr) %>% 
  filter(remove_3132)  %>% 
  select(-remove_3132)
  

# recode variables --------------------------------------------------------

flux2 = 
  flux %>% 
  mutate(Moisture = recode(Moisture,
                           "f.c." = "50% WFPS",
                           "sat." = "100% WFPS"),
         Clay = if_else(Clay=="clay","illite-amended", "non-amended")
         ) %>% 
  select(Temperature, Clay, Moisture, core, datetime, date, time, incub_days,
         CO2_1_ppm, CO2_2_ppm, CO2C_ug_g_hr)


# output ------------------------------------------------------------------

write.csv(flux2, "data/processed/flux.csv", row.names = F)




# cumulative  -------------------------------------------------------------

cumulative_evolution <- function(time, flux, interpolate = TRUE) {
  

  if(interpolate) {
    flux = approx(time, flux, xout = time, rule = 2)[['y']]
  }
  
  delta_time <- time[-1] - head(time, -1)
  
  intermediate_fluxes <- rep(NA_real_, length(delta_time))
  ivals <- head(seq_along(flux), -1)
  for(i in ivals) {
    intermediate_fluxes[i] <- mean(c(flux[i], flux[i+1]))
  }
  evolved <- intermediate_fluxes * delta_time
  c(0, cumsum(evolved))  # cumulative
}

flux2 %>% 
  select(-time) %>% 
  mutate(time=as.numeric(datetime),
         flux=CO2C_ug_g_hr) %>% 
  group_by(core) %>% 
  cumulative_evolution(time, flux)

cumulative_evolution(as.numeric(flux2$datetime), flux2$CO2C_ug_g_hr)

