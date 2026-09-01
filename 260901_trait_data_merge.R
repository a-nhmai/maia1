#Merging Trait Data
#Data from Knighton et al. (2025), TRY, and Guo et al. (2022)
#1 September 2026
#Anh Mai

#Formating shortcut--------------------------------
dashes <- "--------------------------------"

#Libraries--------------------------------
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot")

lapply(lib_ls, library, character.only = TRUE)


#Loading: Data--------------------------------
dir <- "~/maia1/Data/Trait/"

knighton <- read.csv(paste0(dir, "c_Knighton_et_al_2025.csv"))
try <- read.csv(paste0(dir, "c_try_angio_trait_v2.csv")) 
guo <- read.csv(paste0(dir, "c_Guo_et_al_2022"))



#Binding Data--------------------------------

col_ls <- try |> colnames() |> c() #getting the correct order for columns
  col_ls <- col_ls[-6]

  guo <- guo |> relocate(col_ls)
  knighton <- knighton |> relocate(col_ls) 
  
summary(knighton)  
  
trait_df <- try |>
                bind_rows(guo) |>
                bind_rows(knighton) 

#Visualizing Data--------------------------------
dir_save <- "~/maia1/260901_deliverables" #place to save deliverables to present to JPL

top30genera <- trait_df |>
                mutate(genus = str_extract(trait_df$spp_name, "[A-z]*")) |>
                group_by(genus) |>
                summarize(n = n()) |>
                mutate(genus = as.factor(genus),
                       genus = fct_reorder(genus, n)) |>
                slice_max(n, n = 30) |>
                ggplot(aes(x = genus, y = n)) +
                    geom_col(fill = "grey") +
                    coord_flip() +
                    scale_y_continuous(expand = c(0,0), limits = c(0, 4500)) +
                    geom_text(aes(label = n), color = "black", nudge_y = 120, size = 3) +
                theme_classic() +
                labs(x = "", y = "",
                     title = "Top 30 Genera in Preliminary Data",
                     subtitle = "Data from Guo et al. (2022), Knighton et al (2025), and TRY database") 
ggsave(filename = "t_top30genera.png", plot = top30genera, path = dir_save)

#top 30 species
top30genera_ls <- trait_df |>
                        mutate(genus = str_extract(trait_df$spp_name, "[A-z]*")) |>
                        group_by(genus) |>
                        summarize(n = n()) |>
                        mutate(genus = as.factor(genus),
                               genus = fct_reorder(genus, n)) |>
                        slice_max(n, n = 30) |>
                        select(genus) |>
                        unlist(use.names = FALSE)
write.table(top30genera_ls, "~/maia1/Data/Trait/c_top30genera_ls.txt", 
            row.names = FALSE,
            col.names = "Genus")

top30spp_ls <- trait_df |>
                  group_by(spp_name) |>
                  summarize(n = n()) |>
                  mutate(spp_name = as.factor(spp_name),
                         spp_name = fct_reorder(spp_name, n),
                         genus = str_extract(spp_name, "[A-z]*")) |>
                  slice_max(n, n = 30) 

top30generaT <- top30spp_ls |> #list of species that are represented in the top 30 genera
                  mutate(genusT = ifelse(genus %in% top30genera_ls,
                                         "T",
                                         "F")) |>
                  select(spp_name, genusT)

top30spp <- trait_df |>
                group_by(spp_name) |>
                summarize(n = n()) |>
                mutate(spp_name = as.factor(spp_name),
                       spp_name = fct_reorder(spp_name, n)) |>
                slice_max(n, n = 30) |>
                mutate(genus = str_extract(spp_name, "[A-z]*")) |> 
                mutate(genusT = ifelse(genus %in% top30genera_ls, #list of species that are represented in the top 30 genera
                                       "T",
                                       "F"),
                       genusT = fct_recode(genusT,
                                           "In Top 30 Genera" = "T",
                                           "Not in Top 30 Genera" = "F")) |>
                ggplot(aes(x = spp_name, y = n)) +
                geom_col(aes(fill = genusT)) +
                coord_flip() +
                scale_y_continuous(expand = c(0,0), limits = c(0, 2000)) +
                geom_text(aes(label = n), color = "black", nudge_y = 50, size = 3) +
                theme_classic() + 
                theme(legend.title=element_blank()) +
                scale_fill_manual(values = c("darkgreen", "#73be73")) +
                labs(x = "", y = "",
                     title = "Top 30 Spp. in Preliminary Data",
                     subtitle = "Data from Guo et al. (2022), Knighton et al (2025), and TRY database")
ggsave(filename = "t_top30spp.png", plot = top30spp, path = dir_save)

#traits represented
syndrome_ls <- c("S", "G", "G", "G", "G", "G", "G","S", "S", "S", "G", "G", "G", "G",
                 "G", "S", "S", "S", "G", "G", "G")

traits_ranked <- trait_df |>
                      group_by(trait_name) |>
                      mutate(trait_name = fct_collapse(trait_name,
                                                       "Height*" = c("Crown (canopy) height (base to top)",
                                                                     "Plant height (maximum)",
                                                                     "height"),
                                                       "LeafN*" = c("LeafN",
                                                                    "Leaf nitrogen concentration"),
                                                       "Max Rooting Depth*" = c("Root rooting depth",
                                                                               "Rooting Depth maximum")),
                             trait_name = fct_recode(trait_name,
                                                     "LeafP" = "Leaf phosphorous concentration")) |>
                      summarize(n = n()) |> 
                      mutate(trait_name = fct_reorder(trait_name, n)) |>
                      bind_cols(syndrome_ls) |>
                      rename("Syndrome" = "...3") |> 
                      ggplot(aes(x = n, y = trait_name)) +
                        geom_col(aes(fill = Syndrome)) +
                        geom_text(aes(label = n, color = Syndrome), nudge_x = 1300) +
                      scale_x_continuous(expand = c(0,0), limits = c(0, 28000)) +
                      scale_fill_manual(values = c("darkgreen", "#73be73")) +
                      scale_color_manual(values = c("darkgreen", "#73be73")) +
                      theme_classic() +
                      labs(x = "", y = "",
                           title = "Trait Counts",
                           subtitle = "Data from Guo et al. (2022), Knighton et al (2025), and TRY database",
                           caption = "*Indicates lumping together of observations from different databases, requires validation")

ggsave(filename = "t_traits_ranked.png", plot = traits_ranked, path = dir_save)

  
  
  
  
