## ON THE FRINGE -- LDRD PROJECT -- SOM MINERALIZATION
## Kaizad F. Patel
## 12-Jul-2020

source("code/0-packages.R")


# load files --------------------------------------------------------------

flux_data = read.csv("data/respiration.csv")
flux_conc_ambient = read.csv("data/Blanks_Amb_PPM.csv") %>% 
  filter(temperature=="amb") %>% 
  rename(amb_ppm = `CO2.ppm`) %>% 
  mutate(date = mdy(date)) %>% 
  select(date, amb_ppm)

## clean the data
flux_clean = 
  flux_data %>% 
  select(Temperature, Clay, Moisture, date, `time.hour.`, time, `Sample..`, `CO2_1_ppm`, `CO2_2_ppm`, skip) %>% 
  mutate(skip = if_else(skip=="skip", "skip", NA_character_)) %>% 
  filter(is.na(skip)) %>% 
  rename(hour = `time.hour.`,
         core = `Sample..`) %>% 
  mutate(datetime = mdy_hm(paste(date, hour)),
         date = mdy(date)) %>% 
  ## we need to correct the CO2_ppm with ambient_ppm
  left_join(flux_conc_ambient, by = "date") %>% 
  ungroup() %>% 
  mutate(CO21_ppm_corr = CO2_1_ppm - min(CO2_1_ppm))

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
  mutate(incub_days = as.double(difftime(date, min(date), units = "days"))) %>% 
         ## but now, we have two `0` days per sample
         ## recode them to `0` and `0.5`
  group_by(core, incub_days) %>% 
  dplyr::mutate(incub_days = if_else(incub_days==0 & datetime == max(datetime), 0.5, incub_days))

## calculate fluxes ----
flux = 
  flux_times %>% 
  filter(!core==0) %>% 
  mutate(
    # remove 32 and 33 for all except t0
    # create a TRUE/FALSE column where TRUE = rows we want to keep, and FALSE = rows to exclude
    remove_3132 = (core==31&time=="t0")|(core==32&time=="t0")|(core<31),
    # calculate the moles of air in the headspace
    # PV=nRT
    mmol_air = ((1*headspace)/(R*(Temperature+273)))*1000,
    # calculate the absolute mass of CO2C in the headspace
    # from CO2 and moles of air
    CO2C_ug = mmol_air*CO2_1_ppm*12.011/1000,
    CO2C_ug_corr = mmol_air*CO21_ppm_corr*12.011/1000,
    #CO2C_mg = CO2C_ug/1000
    ) %>% 
  filter(remove_3132)  %>% 
  select(-remove_3132)
  

# check for missing values ----------------------------------------------------------
# CO2 data are missing for some time points
# this will f- up our cumulative flux calculations
# so we (a) determine which time points have missing values, and (b) interpolate to fill the gaps

flux_missing = 
  flux %>% 
  # select only the required columns
  dplyr::select(Temperature, Clay, Moisture, core, incub_days, CO2C_ug_corr) %>% 
  # we want to determine which time points are missing, so spread and then melt
  spread(incub_days, CO2C_ug_corr) %>% 
  pivot_longer(-c(Temperature, Clay, Moisture, core), names_to = "incub_days", values_to = "CO2C_ug_corr") %>% 
  # interpolate to fill in missing values, for each group
  group_by(core) %>% 
  mutate(CO2C_ug_interp = zoo::na.approx(CO2C_ug_corr, incub_days),
         incub_days = as.double(incub_days)) %>% 
  dplyr::select(-CO2C_ug_corr) %>% 
  # this includes cores 31-32, because we have timezero values for 31-32.
  # remove those
  filter(core<31)

# now, merge this file into the `flux` file
flux_missing2 = 
  flux %>% 
  full_join(flux_missing, by = c("Temperature", "Clay", "Moisture", "core", "incub_days")) %>% 
  # combine the `CO2C_ug_interp` into `CO2C_ug_corr`
  ungroup() %>% 
  mutate(CO2C_ug_corr = if_else(is.na(CO2C_ug_corr), CO2C_ug_interp, CO2C_ug_corr)) %>% 
  dplyr::select(-CO2C_ug_interp)

# recode variables --------------------------------------------------------

flux2 = 
  flux_missing2 %>% 
  mutate(Moisture = recode(Moisture,
                           "f.c." = "50% WFPS",
                           "sat." = "100% WFPS"),
         Clay = if_else(Clay=="clay","illite-amended", "non-amended")
         ) %>% 
  mutate(CO2C_ug_g_hr = (CO2C_ug_corr/soil_wt) * 1/flux_hr) %>% 
  select(Temperature, Clay, Moisture, core, datetime, date, time, incub_days,
         CO2_1_ppm, CO2_2_ppm, CO2C_ug_g_hr, CO2C_ug, CO2C_ug_corr) %>% 
  ## calculate cumulative evolution
  group_by(core) %>% 
  arrange(incub_days) %>% 
  mutate(CO2C_ug_cum = cumsum(CO2C_ug_corr)) %>% 
  select(Temperature, Clay, Moisture, core, date, time, incub_days, 
         CO2_1_ppm, CO2C_ug_g_hr, CO2C_ug, CO2C_ug_corr, CO2C_ug_cum)



# output ------------------------------------------------------------------

write.csv(flux2, "data/processed/flux.csv", row.names = F)




    ##  # cumulative  -------------------------------------------------------------
    ##  
    ##  flux_cum = 
    ##    flux2 %>% 
    ##    group_by(core) %>% 
    ##    arrange(datetime) %>% 
    ##    mutate(CO2C_ug_cum = cumsum(CO2C_ug_corr))
    ##  
    ##  flux_cum %>% 
    ##    ggplot(aes(x = incub_days, y = cumulative, color = as.character(core)))+
    ##    geom_point()+geom_path()+
    ##    facet_grid(Moisture ~ Clay+Temperature)
    ##    
    ##  
    ##  
    ##  
    ##  
    ##  
    ##  
    ##  
    ##  
    ##  cumulative_evolution <- function(time, flux, interpolate = TRUE) {
    ##    
    ##  
    ##    if(interpolate) {
    ##      flux = approx(time, flux, xout = time, rule = 2)[['y']]
    ##    }
    ##    
    ##    delta_time <- time[-1] - head(time, -1)
    ##    
    ##    intermediate_fluxes <- rep(NA_real_, length(delta_time))
    ##    ivals <- head(seq_along(flux), -1)
    ##    for(i in ivals) {
    ##      intermediate_fluxes[i] <- mean(c(flux[i], flux[i+1]))
    ##    }
    ##    evolved <- intermediate_fluxes * delta_time
    ##    c(0, cumsum(evolved))  # cumulative
    ##  }
    ##  
    ##  flux2 %>% 
    ##    select(-time) %>% 
    ##    mutate(time=as.numeric(datetime),
    ##           flux=CO2C_ug_g_hr) %>% 
    ##    group_by(core) %>% 
    ##    cumulative_evolution(time, flux)
    ##  
    ##  cumulative_evolution(as.numeric(flux2$datetime), flux2$CO2C_ug_g_hr)

