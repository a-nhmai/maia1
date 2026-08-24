#FIA exploration
#Simplifying at MN_TREE.csv from FIA website https://research.fs.usda.gov/products/dataandtools/fia-datamart
#Anh Mai
#07 August 2026

library(dplyr)
#install.packages("janitor")
library(janitor)

#Data
fia_rd <- read.csv("~/maia1/260807_FIA_exploration/MN_TREE.csv") #rd = raw data
                      #NOTE: MN_TREE file is too big to exist on git server
head(fia_rd)

fia_rd <- clean_names(fia_rd)

colnames(fia_rd)

select_cols <- c("tree",
                 "spcd",
                 "spgrpcd",
                 "plot",
                 "subp",
                 "condid",
                 "unitcd",
                 "ht",
                 "htcd",
                 "actualht",
                 "totage",
                 "dia",
                 "bhage",
                 "countycd")

fia_rd_2025_sub <- fia_rd |> #sub = subset
                        filter(invyr == 2025) |>
                        select(select_cols) 

write.csv(fia_rd_2025_sub, "~/maia1/260807_FIA_exploration/fia_rd_2025_sub.csv")


