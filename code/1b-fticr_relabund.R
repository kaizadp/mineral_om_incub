## ON THE FRINGE -- LDRD PROJECT -- SOM MINERALIZATION
## Kaizad F. Patel
## 29-Jun-2020

## FTICR analysis -- (b) relative abundance

source("code/0-packages.R")

# load files --------------------------------------------------------------

data_long = read.csv("processed/fticr_data_long.csv", stringsAsFactors = F)
meta_hcoc = read.csv("processed/fticr_meta_hcoc.csv", stringsAsFactors = F)
fticr_meta = read.csv("processed/fticr_meta.csv", stringsAsFactors = F)

#