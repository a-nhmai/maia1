#Looking at species list across what's available in FIA vs 

library(dplyr)

#Libraries--------------------------------------------------------------------------------
lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "readxl")

sapply(lib_ls, library, character.only = TRUE)

#Data--------------------------------------------------------------------------------
data_dir <- "~/maia1/Data/"

#FIA
fia_spp <- read.csv(paste0(data_dir, "FIA/c_REF_SPECIES.csv")) |> select(SCIENTIFIC_NAME)

#Morton
morton_check_ls <- read.csv(paste0(data_dir, "Morton/The Checklist of U.S. Trees - Checklist of U.S. Trees.csv")) |> select(Species.Name)
    #https://docs.google.com/spreadsheets/d/1n-wofRuswvfJaghAzcN-rd7l3uCM3t1UjF5y6E-_cbA/edit?gid=846368255#gid=846368255

#Interview
interview_ls <- read.delim("~/maia1/260824_info_interview_analysis/spp_tab.txt", sep = " ")

#Inner-join of FIA and Morton--------------------------------------------------------------------------------
fia_morton <- fia_spp |>
                  inner_join(morton_check_ls, by = join_by("SCIENTIFIC_NAME" == "Species.Name")) |>
                  clean_names() |>
                  mutate(genus = str_extract(scientific_name, "[A-z]*"),
                         InterviewMN_Y= ifelse(genus %in% interview_ls$genus == "TRUE", "Y", "N"))


#Overview of FIA and Morton--------------------------------------------------------------------------------

#number of species that is identified by morton that is in FIA
n_fia_coverage <- dim(fia_morton)[1] 
n_fia_coverage

#percent coverage of morton by FIA
percent_coverage <- n_fia_coverage/dim(morton_check_ls)[1] 
percent_coverage

genera_ct <- fia_morton |>
                group_by(genus) |>
                count(InterviewMN_Y) |>
                filter(InterviewMN_Y == "Y" | n > 9) |> #genera associated with the interview OR having more than 10 species represented in the priority list
                arrange(desc(n)) |>
                mutate(Mention = ifelse(InterviewMN_Y == "Y", "*", " ")) |>
                select(-InterviewMN_Y) |>
                relocate(genus, n) |>
                rename(n_spp = n) 
    #this however has some gymnosperm species so filtered below

genera_ct[c(-2,-5,-10),2] |> sum() #still 142 angiosperm species with the 10 focal angiosperm genera


#Species and genera of priority
genera_highlighted <- genera_ct[c(-2,-5,-10),1] |> unlist(use.names = FALSE) #10 focal genera
spp_highlighted <- fia_morton |>                                            #and the associated species
                    filter(genus %in% genera_highlighted) #146 species in consdieration

write.csv(spp_highlighted, "~/maia1/Data/Cleaning/spp_highlighted.csv", row.names = F)
write.csv(genera_highlighted, "~/maia1/Data/Cleaning/genera_highlighted.csv", row.names = F)



