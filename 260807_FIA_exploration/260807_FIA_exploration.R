#FIA exploration
#Exploring a simplified version of the 
#MN_TREE.csv from FIA website https://research.fs.usda.gov/products/dataandtools/fia-datamart
#Anh Mai
#07 August 2026


#Data overview
      #from https://research.fs.usda.gov/products/dataandtools/fia-datamart
      #Filtered for year 2025 and subset of columns

#Column names
#select_cols <- c("tree",       Tree number
#                 "spcd",       Species code
#                 "spgrpcd",    Species group code
#                 "plot",       Plot number
#                 "subp",       Subplot number
#                 "condid",     Condition class number
#                 "unitcd",     Survey unit code
#                 "ht",         Total height
#                 "htcd",       Height method code
#                 "actualht",   Actual height
#                 "totage",     Total age: age of a live tree either by tree rings or d.r.c
#                 "dia",        Current diameter
#                 "bhage",      Breast height age
#                 "countycd")   County code


#Libraries
lib_ls <- c("dplyr",
            "ggplot2",
            "janitor",
            "forcats")

sapply(lib_ls, library, character.only = TRUE)

#Reading in data
fia_rd <- read.csv("~/maia1/260807_FIA_exploration/fia_rd_2025_sub.csv")
fia_rd <- fia_rd[,-1] #eliminating row numbers
spp_ref <- read.csv("~/maia1/260807_FIA_exploration/REF_SPECIES.csv")

#Cleaning data
spp_ref <- clean_names(spp_ref)
spp_ref_sm <- spp_ref[,c(1,2,19)]

#Joining df
fia_spp <-  fia_rd |> 
                left_join(spp_ref_sm, by = join_by(spcd))

#Overview of data
head(fia_rd)
tail(fia_rd)
colnames(fia_rd)
glimpse(fia_rd)
summary(fia_rd)

glimpse(fia_spp)

#Restructuring data
fia_rd <- fia_rd |>
            mutate(across(tree:countycd, as.factor)) |>
            mutate(across(c(ht, actualht, totage, dia, bhage), as.numeric))
unique(fia_rd$plot)

fia_spp <- fia_spp |>
  mutate(across(c(tree:countycd, common_name), as.factor)) |>
  mutate(across(c(ht, actualht, totage, dia, bhage), as.numeric))

#exploratory
fia_rd |>
  filter(countycd == 15 | countycd == 93) |>
  ggplot(aes(x = ht, color = countycd, fill = countycd)) +
    geom_density(size = 1, show.legend = F, alpha = 0.1)+
    #scale_color_viridis_d()+
    #scale_fill_viridis_d()+
    facet_wrap(.~countycd) #county 15 and 93 are interesting...

fia_rd |>
  group_by(plot) |> 
  summarise(n_dist = n_distinct(tree)) |>
  ggplot(aes(x = n_dist, y = 1)) +
      geom_boxplot(color="black", width = 0.4)+
      geom_violin(alpha = 0.3) #most plots have around 20 species, a few with 50+

fia_rd |>
  group_by(plot) |> 
  summarise(n_dist = n_distinct(tree)) |>
  arrange(desc(n_dist)) |>
  slice_max(n = 5, order_by = n_dist)

fia_rd |>
  group_by(plot) |> 
  summarise(n_dist = n_distinct(tree)) |>
  arrange(desc(n_dist)) |>
  slice_min(n = 5, order_by = n_dist) #8 plots have only 1 species

fia_spp |>
  filter(sftwd_hrdwd == "H") |> #about 20 are softwoods
  count(common_name) |>
  arrange(desc(n))  #really interesting composition... quaking aspen is dominating BY FAR.

fia_spp |>
  filter(sftwd_hrdwd == "H") |> #about 20 are softwoods
  count(common_name) |> 
  mutate(common_name = as.factor(common_name),
         common_name = fct_rev(fct_infreq(common_name))) |>
  ggplot(aes(x = n, y = common_name)) +
      geom_col() +
      labs(x = "Log(Count)")


fia_spp |>
  filter(sftwd_hrdwd == "H") |> 
  ggplot(aes(x = fct_rev(fct_infreq(common_name)))) +
    geom_bar() +
    coord_flip() +
    labs(x = "Species common name")



