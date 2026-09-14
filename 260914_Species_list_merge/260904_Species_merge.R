#Looking at species list across what's available in FIA vs 

library(dplyr)

REF_SPECIES <- read.csv("Documents/Data/FIADB_REFERENCE/REF_SPECIES.csv") |> select(SCIENTIFIC_NAME)
write.csv(REF_SPECIES, "~/maia1/260914_Species_list_merge/fia_spp_ls.csv")
