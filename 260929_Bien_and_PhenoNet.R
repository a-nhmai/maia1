#Vulnerability proj: Data Synthesis
#Cleaning data from 
#Anh Mai
#28 September 2026

#Libraries--------------------------------
#install.packages("BIEN")
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot",
            "tidyr",
            "BIEN")

lapply(lib_ls, library, character.only = TRUE)

#Data: load--------------------------------
spp_highlighted <- read.csv("Data/Cleaning/spp_highlighted.csv") |> select(1) |> unlist(use.names = FALSE)
col_order <- read.csv("Data/Cleaning/col_order.csv") |> unlist(use.names = FALSE)



#Data: cleaning--------------------------------

#Botanical Information and Ecology Network Database (BIEN)
bien_df_traits <- BIEN_trait_list() 
bien_df_traits <- bien_df_traits[c(1, 5, 7, 11, 15, 16, 16, 18, 19, 25, 36, 37, 41), ] |> unlist(use.names = FALSE)
bien_df_r <- BIEN_trait_traitbyspecies(species = spp_highlighted,
                                      trait = bien_df_traits,
                                      source.citation = TRUE)
bien_df_r |>
  filter(scrubbed_species_binomial %in% spp_highlighted) |> #129/146 species represented here
  group_by(scrubbed_species_binomial) |> 
  summarize(n = n()) |>
  arrange(desc(n))

bien_df_c <- bien_df_r |> 
              rename(spp_name = scrubbed_species_binomial,
                     value = trait_value,
                     kind = method) |>
              mutate(comment = str_c("project PI = ", project_pi, 
                                     " | access = ", access,
                                     " | id = ", id,
                                     " | source citation = " , source_citation),
                     source = "BIEN") |>
              relocate(col_order)

#phenophase data from Phenology Network
#only for MI, WI, MN, IA, IL for 2010-2020

#probably end up writing a function for this
pheno_net <- read.csv("~/Documents/05_Data/pheno_network_datasheet_1790643272661/individual_phenometrics_data.csv") |> 
  filter(Growth_Habit == "Tree") |>
  mutate(spp_name = str_c(genus, " ", Species),
         across(First_Yes_Year:Last_Yes_DOY, as.character)) |>
  filter(spp_name %in% spp_highlighted) |>
  select(State, Individual_ID, Phenophase_Description, 
         First_Yes_Year, Last_Yes_Year, First_Yes_DOY, 
         Last_Yes_DOY, Site_Name,
         spp_name) |>
  pivot_longer(cols = First_Yes_DOY:Last_Yes_DOY,
               names_to = "trait_name",
               values_to = "value") |>
  mutate(comment = str_c("Year = ", First_Yes_Year, ", ", Last_Yes_Year, 
                         " | Site Name = ", Site_Name, 
                         " | Individual ID = ", Individual_ID),
         trait_name = str_c(trait_name, " ", Phenophase_Description),
         kind = NA,
         unit = "DOY",
         source = "Phenology Network") |>
  select(unlist(col_order, use.names = FALSE))




#Data: writing csv--------------------------------

#write.csv(bien_df_r, "~/maia1/Data/Trait/r_BIEN.csv", row.names = FALSE) stored locally due to size
write.csv(bien_df_c, "~/Documents/05_Data/BIEN/c_BIEN.csv", row.names = FALSE) #stored locally due to size

write.csv(pheno_net, "~/maia1/Data/Trait/c_pheno_network.csv", row.names = FALSE)



