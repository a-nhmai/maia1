#Informational Interviews Visualizations
#Construction of visualizations to show JPL
#Anh Mai
#01 September 2026

#Libraries--------------------------------
lib_ls <- c("dplyr",
            "ggplot2",
            "janitor",
            "stringr",
            "readxl")

sapply(lib_ls, library, character.only = TRUE)

#Data: Loading--------------------------------
dir <- "~/maia1/260824_info_interview_analysis/"
dir_save <- "~/maia1/260901_deliverables" #place to save deliverables to present to JPL

concepts <- read.delim(paste0(dir, "concept_tab.txt"), sep = " ")
spp <- read.delim(paste0(dir, "spp_tab.txt"), sep = " ")
top30genera <- read.delim("~/maia1/Data/Trait/c_top30genera_ls.txt") |> unlist(use.names = FALSE)

#Data: visualization--------------------------------

#Concept viz
concept_ranking <- concepts |> 
                        mutate(term_concept = fct_reorder(term_concept, count),
                               term_concept = fct_recode(term_concept,
                                 "Forest regeneration" = "Forest regeneration (young forests survival and old forest maintenance)")) |>
                        ggplot(aes(x = count, y = term_concept)) +
                          geom_col() +
                          geom_text(aes(label = count), nudge_x = 0.2) +
                          labs(x = "", y = "",
                               title = "Concept Analysis of Interviews",
                               subtitle = "Interviews from private, non-profit, public, and federal agencies (N = 8)") +
                        theme_classic() +
                        scale_x_continuous(expand = c(0,0), limits = c(0, 10))

ggsave(filename = "i_concept_ranking.png", path = dir_save, plot = concept_ranking)

#Genus viz
genus_mentions <- spp |> 
                      mutate(top30T = ifelse(spp$genus %in% top30genera, "T", "F"),
                             genus = fct_reorder(genus, n),
                             top30T = fct_rev(top30T)) |>
                      ggplot(aes(x = n, y = genus)) +
                      geom_col(aes(fill = top30T)) +
                      geom_text(aes(label = n), nudge_x = 0.2) +
                      labs(x = "", y = "",
                           title = "Genus Mentions in Interviews",
                           subtitle = "Interviews from private, non-profit, public, and federal agencies (N = 8)") +
                      theme_classic() +
                      scale_fill_manual(values = c("darkgreen", "#73be73")) +
                      scale_x_continuous(expand = c(0,0), limits = c(0, 13))

ggsave(filename = "i_genus_mentions.png", path = dir_save, plot = genus_mentions)  



