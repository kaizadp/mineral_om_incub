## ON THE FRINGE -- LDRD PROJECT -- SOM MINERALIZATION
## Kaizad F. Patel
## 29-Jun-2020

## FTICR analysis -- (a) cleaning and processing

source("code/0-packages.R")

# 1. load report ----------------------------------------------------------
corekey = read.csv("data/corekey.csv")
report = read.csv("data/Bioavailability_Incubation_LDRD_ICR_Report.csv")

report2 = 
  report %>% 
  filter(Mass > 200 & Mass < 900) %>% 
  # remove isotopes
  filter(C13==0) %>% 
  filter(C>0) %>% 
  dplyr::select(-C13)

#
# 2. create fticr meta file -----------------------------------------------

fticr_meta = 
  report2 %>% 
  dplyr::select(Mass:Class) %>% 
  # create columns for indices
  dplyr::mutate(AImod = round((1+C-(0.5*O)-S-(0.5*(N+P+H)))/(C-(0.5*O)-S-N-P),4),
                NOSC =  round(4-(((4*C)+H-(3*N)-(2*O)-(2*S))/C),4),
                HC = round(H/C,2),
                OC = round(O/C,2)) %>% 
  # create column/s for formula
  # first, create columns for individual elements
  # then, combine
  dplyr::mutate(formula_c = if_else(C>0,paste0("C",C),as.character(NA)),
                formula_h = if_else(H>0,paste0("H",H),as.character(NA)),
                formula_o = if_else(O>0,paste0("O",O),as.character(NA)),
                formula_n = if_else(N>0,paste0("N",N),as.character(NA)),
                formula_s = if_else(S>0,paste0("S",S),as.character(NA)),
                formula_p = if_else(P>0,paste0("P",P),as.character(NA)),
                formula = paste0(formula_c,formula_h, formula_o, formula_n, formula_s, formula_p),
                formula = str_replace_all(formula,"NA","")) %>% 
  dplyr::select(Mass, formula, El_comp, Class, HC, OC, AImod, NOSC, C:P)


# subset of meta for HC/OC only, for Van Krevelen diagrams
meta_hcoc = 
  fticr_meta %>% 
  dplyr::select(Mass, formula, HC, OC)

#
# 3. create data file -------------------------------------------------------------

data_long =
  report2 %>% 
  dplyr::select(Mass, starts_with("x_")) %>% 
  pivot_longer(-Mass,
               names_to = "Sample_ID",
               values_to = "intensity") %>% 
  mutate(presence = case_when(intensity > 0 ~ 1,
                              intensity==0 ~ 0)) %>% 
  dplyr::select(-intensity) %>% 
  left_join(dplyr::select(meta, Mass, formula), by = "Mass") %>% 
  dplyr::select(Mass, formula, Sample_ID, presence) %>% 
  group_by(formula, Sample_ID) %>% 
  dplyr::summarise(presence = mean(presence),
                   n = n()) %>% 
  ungroup %>% 
  mutate(presence = if_else(presence>0,1,0)) %>% 
  filter(presence==1)
  
data_long2  =
  data_long %>% 
  dplyr::select(-n) %>% 
  left_join(dplyr::select(corekey, Pre_post, Sample_ID, Temperature, Moisture, Clay), by = "Sample_ID")

#
# 4. OUTPUT -----------------------------------------------------------------

write.csv(fticr_meta, "processed/fticr_meta.csv", row.names = F)
write.csv(data_long2, "processed/fticr_data_long.csv", row.names = F)
write.csv(meta_hcoc, "processed/fticr_meta_hcoc.csv", row.names = F)






