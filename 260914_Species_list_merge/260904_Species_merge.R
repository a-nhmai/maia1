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

fia_morton |>
  group_by(genus) |>
  count(InterviewMN_Y) |>
  filter(InterviewMN_Y == "Y" | n > 9) |>
  arrange(desc(n)) |>
  mutate(Mention = ifelse(InterviewMN_Y == "Y", "*", " ")) |>
  select(-InterviewMN_Y) |>
  relocate(genus, n) |>
  rename(n_spp = n) |>
  knitr::kable()
