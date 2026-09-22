#Vulnerability proj: Trait Network Setup
#Data synthesis from multiple literature data sources
#Anh Mai
#18 September 2026

#Libraries--------------------------------
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot",
            "tidyr")

lapply(lib_ls, library, character.only = TRUE)

install.packages("MultiTraits")
library(MultiTraits)

#Data:loading--------------------------------
data_dir <- "~/maia1/Data/Trait/"

REF_SPECIES <- read.csv("~/maia1/Data/Cleaning/spp_highlighted.csv") |> select(1) #FIA + U.S.

col_order <- read.csv("~/maia1/Data/Cleaning/col_order.csv") 
spp_highlight <- read.csv("~/maia1/Data/Cleaning/spp_highlighted.csv") 

diaz <- read.csv(paste0(data_dir, "c_diaz_et_al_2022.csv")) |> select(-1) |> mutate(value = as.character(value))
guo <- read.csv(paste0(data_dir, "c_Guo_et_al_2022.csv")) |> relocate(col_order$x) |> mutate(value = as.character(value))
jin <- read.csv(paste0(data_dir, "c_jin_et_al_2026.csv")) |> select(-1) |> mutate(value = as.character(value))
lopez_martin <- read.csv(paste0(data_dir, "c_lopez_martinez_2023.csv")) |> select(-1) |> mutate(value = as.character(value))
try <- read.csv(paste0(data_dir, "c_TRY_angio_trait_v2.csv")) |> mutate(value = as.character(value)) |> select(-6)

df_main <- bind_rows(diaz, guo, jin, lopez_martin, try)


#Data: overview--------------------------------

#What is the data coverage for species traits for my reference list?......................................
    #findings:
          #looking to see where the reference species list overlaps with the trait data coverage I have
          #146 reference species
          #143 species are already represented in the dataset so far
          #3 species not represented in the dataset: Quercus graciliformis, Quercus tardifolia, Quercus robusta

    #how many species in consideration?
        REF_SPECIES #reference list: FIA, U.S. List, MN natural resource mentions, n = 146
        
    #how many species overlap with the REF_SPECIES and the ones from the main trait df?
        n_distinct(df_main$spp_name) #n = 44263 species in main trait df
        
        ref_species_ls <- REF_SPECIES |> unlist(use.names = FALSE)
        
        df_main |>
          filter(spp_name %in% ref_species_ls) |>
          nrow() #8020 observations
        
        REF_SPECIES |> inner_join(as.data.frame(unique(df_main$spp_name)), 
                                  by = join_by(scientific_name == `unique(df_main$spp_name)`)) |>
          nrow() #143 out of 146 species represented
        
        REF_SPECIES |> anti_join(as.data.frame(unique(df_main$spp_name)), 
                                  by = join_by(scientific_name == `unique(df_main$spp_name)`)) #the 3 species not represented
        
#What does the dataset look like when I filter for only the 148ish species?......................................
        df_filt <- df_main |>
                      filter(spp_name %in% ref_species_ls) 
        
        
#What is the data coverage for traits?......................................
    #findings: 
        # 33 unique traits 
        # least NAs: wood density, seed dry mass, plant height max, phosph/nitro concentration, leaf traits...
        # most NAs: floral, roots, vessel diameter
        # selection/filter for traits with less than 50% NAs
        
                
      
        
        unique(df_filt$trait_name) #33 unique traits
        
        trait_ct <- df_filt |> #df where rows = species and columns = traits and cells = number of observations
                    group_by(spp_name, trait_name) |>
                    summarise(n_trait_ct = n()) |>
                    arrange(desc(n_trait_ct)) |>
                    pivot_wider(names_from = trait_name,
                                values_from = n_trait_ct) 
        
        summary(trait_ct) #validating NAs look right
        
        NA_row_num <- nrow(trait_ct) #number of species 
        
        NA_prop <- colSums(is.na(trait_ct[-1])) |> #proportion of NAs per trait in descending order at species level
                        as.data.frame() |>
                        rename(propNA = `colSums(is.na(trait_ct[-1]))`) |>
                        mutate(propNA = round(propNA/NA_row_num, 2)) |>
                        arrange(desc(propNA))
          
        NA_prop |> #less than 50% missing
          filter(propNA < 0.5)
        
        NA_prop |> #more than 50% missing
          filter(propNA > 0.5)
        
              trait_ls_NAsub50 <- NA_prop |> 
                filter(propNA < 0.5) |>
                rownames() |>
                as.data.frame() |>
                unlist(use.names = FALSE) 
                
                
#Analysis: PTMN--------------------------------

#Setup......................................
    #Dataframe: species in rows and traits in columns, one point value in each cell
          ptmn_df1 <- df_filt |>
                        filter(trait_name %in% trait_ls_NAsub50) |>
                        filter(trait_name != "leaf type") |> #no categorical at this point
                        filter(trait_name != "seed dispersal category") |> #no categorical at this point
                        select(spp_name, trait_name, value) |>
                        mutate(value = as.numeric(value)) |>
                        group_by(spp_name, trait_name) |>
                        summarise(avg_trait_value = mean(value, na.rm = TRUE)) |>
                        pivot_wider(names_from = trait_name,
                                    values_from = avg_trait_value) |>
                        clean_names()
              
          ptmn_df1 <- ptmn_df1[-1] #must remove species, only cells should be traits
          
    #Layers list: species in rows and traits in columns, one point value in each cell
          ptmn_df1 |> colnames()
          
          layers1 <- list(
            growth = c("leaf_dry_matter_content", 
                       "leaf_area", 
                       "plant_height_maximum", 
                       "leaf_nitrogen_concentration",
                       "leaf_phosphorous_concentration",
                       "plant_height"),
            reproduction = c("seed_dry_mass"),
            stress = c("sla", 
                       "wood_density")
          )


#PTMN......................................
ptmn_edges1 <- PTMN(ptmn_df1, layers_list = layers1, method = "pearson")
PTMN_metrics(ptmn_edges1)
PTMN_plot(ptmn_edges1, 
          style = 1, 
          vertex.size = 10, #aes
          vertex.label.cex = 0.8, #aes
          edge.width = 2, #aes
          show.legend = TRUE) #aes





































#Really messy.. Anh wanted to start over but still really helpful exploration

#Data:overview -------------------------------- MESSY...

#filtering for species highlighted
    #there are 480 species highlighted, and theres a fair amount of representation
    #471 are represented in this initial df

#top 10
df |>
  filter(spp_name %in% spp_highlight$x) |>
  group_by(spp_name) |>
  summarize(n = n()) |>
  arrange(desc(n)) |> 
  slice_max(n, n = 10) 

#trait representation
df |>
  filter(spp_name %in% spp_highlight$x) |>
  group_by(trait_name) |>
  summarize(n = n()) |>
  arrange(desc(n)) |> 
  print(n = 10) 

#Data: rearranging for trait by species--------------------------------

#Data: for the top 10 represented species-------------------------------

#carrying this out for the top 10 represented species based on overlap between FIA and US list; still need to figure out the living collections
top10_spp <- df |>
  filter(spp_name %in% spp_highlight$x) |>
  group_by(spp_name) |>
  summarize(n = n()) |>
  arrange(desc(n)) |> 
  slice_max(n, n = 10) |>
  select(spp_name) |> c() |> unlist()

#diagnostics
      #number of traits for each species
            df |> 
              filter(spp_name %in% top10_spp) |>
              group_by(spp_name) |>
              summarise(n_trait = n_distinct(trait_name)) |>
              arrange(desc(n_trait))
      
      #number of observations of traits 
            nObs <- df |> 
                    filter(spp_name %in% top10_spp) |>
                    group_by(spp_name, trait_name) |>
                    summarise(n_trait_ct = n()) |>
                    arrange(desc(n_trait_ct)) |>
                    pivot_wider(names_from = trait_name,
                                values_from = n_trait_ct) |>
                    clean_names()
            nObs
      #percent NAs      
            colSums(is.na(nObs[,-1])) |> 
              as.data.frame() |>
              rename(propMissing = `colSums(is.na(nObs[, -1]))`) |>
              arrange(desc(propMissing)) |>
              mutate(propMissing = propMissing/10)

#setup
        #dataframe that is by species
        spp_df <- df |>
                    filter(spp_name %in% top10_spp) |>
                    filter(trait_name != "leaf type") |> 
                    filter(trait_name != "seed dispersal category") |>
                    select(spp_name, trait_name, value) |>
                    mutate(value = as.numeric(value)) |>
                    group_by(spp_name, trait_name) |>
                    summarise(avg_trait_value = mean(value, na.rm = TRUE)) |>
                    pivot_wider(names_from = trait_name,
                                values_from = avg_trait_value) |>
                    clean_names()
        
        #8 of the traits have no NAs... starting with these
          #many have gaps that are able to be filled in to a certain extent!
        trait_noNA <- colSums(is.na(spp_df[,-1])) |> 
                          as.data.frame() |> 
                          rename("NumNA" = "colSums(is.na(spp_df[, -1]))") |>
                          arrange(NumNA) |>
                          filter(NumNA == "0") |>
                          row.names()


#PTMN
ptn_traits <- spp_df[,-1] |> select(trait_noNA) 

layers <- list(
  growth = c("leaf_dry_matter_content", 
  "leaf_area", 
  "plant_height_maximum", 
  "leaf_nitrogen_concentration",
  "leaf_phosphorous_concentration"),
  reproduction = c("seed_dry_mass"),
  stress = c("sla", "wood_density")
)

ptmn_edges <- PTMN(ptn_traits, layers_list = layers, method = "pearson")
PTMN_metrics(ptmn_edges)
PTMN_plot(ptmn_edges, style = 1, vertex.size = 8,
          vertex.label.cex = 0.5, edge.width = 2,
          show.legend = FALSE)


#PTMN: larger trait list, top10 --------------------------------

#diagnostics
      #nspecies
            n_spp2 <- n_distinct(df$spp_name)

      #number of traits for each species
            df |> 
              group_by(spp_name) |>
              summarise(n_trait = n_distinct(trait_name)) |>
              arrange(desc(n_trait))
      
      #number of observations of traits 
            nObs2 <- df |> 
                    group_by(spp_name, trait_name) |>
                    summarise(n_trait_ct = n()) |>
                    arrange(desc(n_trait_ct)) |>
                    pivot_wider(names_from = trait_name,
                                values_from = n_trait_ct) |>
                    clean_names()
            nObs2
      #percent NAs      
            colSums(is.na(nObs2[,-1])) |> 
              as.data.frame() |>
              rename(propMissing = `colSums(is.na(nObs2[, -1]))`) |>
              arrange(desc(propMissing)) |>
              mutate(propMissing = round(propMissing/n_spp2, 4)) 
            
            

trait_noNA2 <- colSums(is.na(spp_df[,-1])) |> 
  as.data.frame() |> 
  rename("NumNA" = "colSums(is.na(spp_df[, -1]))") |>
  arrange(NumNA) |>
  filter(NumNA < "3") |>
  row.names()

trait_noNA2 <- trait_noNA2[c(-12, -15)]

ptn_traits2 <- spp_df[,-1] |> select(trait_noNA2) 

layers2 <- list(
  growth = c("leaf_dry_matter_content", 
             "leaf_area", 
             "plant_height_maximum", 
             "leaf_nitrogen_concentration",
             "leaf_phosphorous_concentration",
             "plant_height"),
  reproduction = c("seed_dry_mass", 
                   "diaspore_mass"),
  stress = c("sla", 
             "lma", 
             "wood_density", 
             "stem_conduit_diameter_vessels_tracheids",
             "ssd_combined")
)

ptmn_edges2 <- PTMN(ptn_traits2, layers_list = layers2, method = "pearson")
PTMN_metrics(ptmn_edges2)
PTMN_plot(ptmn_edges2, style = 1, vertex.size = 8,
          vertex.label.cex = 0.5, edge.width = 2,
          show.legend = FALSE)

#more redundancy in floral/reproductive traits than I thought
#some redundancy in leaf traits though not necessarily in relation to nutrients
#seed dry mass, leaf dry content, and wood density have an interesting interlater clustering


#PTMN: larger trait list, all spp--------------------------------

#big dataframe, NOT filtered for top 10 highest trait covered spp
spp_df_big <- df |>
                select(spp_name, trait_name, value) |>
                filter(trait_name != "leaf type") |> 
                filter(trait_name != "seed dispersal category") |>
                mutate(value = as.numeric(value)) |>
                group_by(spp_name, trait_name) |>
                summarise(avg_trait_value = mean(value, na.rm = TRUE)) |>
                pivot_wider(names_from = trait_name,
                            values_from = avg_trait_value) |>
                clean_names() |>
                select(-stem_conduit_length, -plant_lifespan_longevity, -nmass, -ssd_observed, -leaf_area_2) #had 0 or 1 trait value or irrelevant summary stat, irrelevant, respectively

#diagnostics
      #number of species
      n_distinct(spp_df_big$spp_name) # N = 12386
      
      #number of traits for each species
      df |> 
        filter(spp_name %in% top10_spp) |>
        group_by(spp_name) |>
        summarise(n_trait = n_distinct(trait_name)) |>
        arrange(desc(n_trait))
      
      #number of observations of traits 
      nObs <-  df |> 
        filter(spp_name %in% top10_spp) |>
        group_by(spp_name, trait_name) |>
        summarise(n_trait_ct = n()) |>
        arrange(desc(n_trait_ct)) |>
        pivot_wider(names_from = trait_name,
                    values_from = n_trait_ct) |>
        clean_names()
      nObs
      #percent NAs 95%     
      perc95_NA <- colSums(is.na(spp_df_big[,-1])) |> 
        as.data.frame() |> 
        rename("NumNA" = "colSums(is.na(spp_df_big[, -1]))") |>
        arrange(NumNA) |>
        mutate(PropNA = NumNA/n_distinct(spp_df_big$spp_name),
               PropNA = round(PropNA, 3)) |>
        filter(PropNA < 0.95) |> #filtering for less than 90% missing
        row.names()





ptn_traits3 <- spp_df_big[,-1] |> select(perc95_NA)
layers3 <- list(
  growth = c("leaf_dry_matter_content", 
             "leaf_area", 
             "plant_height_maximum", 
             "leaf_nitrogen_concentration",
             "leaf_phosphorous_concentration",
             "plant_height"),
  reproduction = c("seed_dry_mass", 
                   "diaspore_mass",
                   "sex_of_flowers",
                   "ovary_position",
                   "anther_dehiscence",
                   "fusion_of_ovaries",
                   "anther_attachment",
                   "no_ovules",
                   "anther_orientation"),
  stress = c("sla", 
             "lma", 
             "wood_density", 
             "ssd_combined"
             )
)

ptmn_edges3 <- PTMN(ptn_traits3, layers_list = layers3, method = "pearson")
PTMN_metrics(ptmn_edges3)
PTMN_plot(ptmn_edges3, style = 1, vertex.size = 8,
          vertex.label.cex = 0.5, edge.width = 2,
          show.legend = FALSE)

#can handle some NAs (like up to 9171) but not all; can still handle up to 95% missing 


#for 90% missing values
perc90_NA <- colSums(is.na(spp_df_big[,-1])) |> 
                   as.data.frame() |> 
                   rename("NumNA" = "colSums(is.na(spp_df_big[, -1]))") |>
                   arrange(NumNA) |>
                   mutate(PropNA = NumNA/n_distinct(spp_df_big$spp_name),
                          PropNA = round(PropNA, 3)) |>
                   filter(PropNA < 0.90) |> #filtering for less than 90% missing
                   row.names()

ptn_traits4 <- spp_df_big[,-1] |> select(perc90_NA)

layers4 <- list(
  growth = c("leaf_dry_matter_content", 
              "leaf_area", 
              "plant_height_maximum", 
              "leaf_nitrogen_concentration",
              "leaf_phosphorous_concentration",
              "plant_height"),
  reproduction = c("seed_dry_mass", 
                    "diaspore_mass"),
  stress = c("sla", 
              "lma", 
              "wood_density", 
              "ssd_combined"
              )
)
ptmn_edges4 <- PTMN(ptn_traits4, layers_list = layers4, method = "pearson")
PTMN_metrics(ptmn_edges4)
PTMN_plot(ptmn_edges4, 
          style = 1, 
          vertex.size = 8, #aesthe
          vertex.label.cex = 0.5, 
          edge.width = 2, 
          show.legend = FALSE)
