## ON THE FRINGE -- LDRD PROJECT -- SOM MINERALIZATION
## Kaizad F. Patel
## 12-Jul-2020

# DISSOLVED ORGANIC CARBON

source("code/0-packages.R")


# load files --------------------------------------------------------------

doc_data = read.csv("data/doc.csv")
doc_key = read.csv("data/doc_key.csv")


## clean

doc_cleaned = 
  doc_data %>% 
  rename(doc_ppm = `DOC..ppm.`) %>% 
  group_by(Identifier) %>% 
  summarise(doc_ppm = mean(doc_ppm)) %>% 
  left_join(doc_key, by = "Identifier") %>% 
  mutate(Time = recode(Time, " Initial" = "Initial"))




# outputs -----------------------------------------------------------------

write.csv(doc_cleaned, "data/processed/doc.csv", row.names = F)


