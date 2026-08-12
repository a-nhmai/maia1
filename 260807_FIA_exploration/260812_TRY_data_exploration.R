#TRY Data Acquisition and Cleaning 
#EDA of try_angio.csv
#Anh Mai
#12 August 2026

#Loading in libraries
    #install.packages("cowplot")

lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "cowplot")

sapply(lib_ls, library, character.only = TRUE)


#Loading in df
trait_df_raw <- read.csv("~/maia1/260807_FIA_exploration/try_angio_clean.csv")
trait_df <- trait_df_raw |>
              select(DatasetID, 
                     AccSpeciesName,
                     ObservationID,
                     TraitID,
                     TraitName,
                     DataID,
                     OriglName,
                     OrigValueStr,
                     OrigUnitStr,
                     StdValue,
                     Comment) |>
              rename(value = StdValue,
                     spp_name = AccSpeciesName) |>
              clean_names() |> 
              mutate(across(c(dataset_id:data_id),
                            as.factor)) |>
              mutate(across(c(orig_value_str, value),
                            as.numeric))

#Removed data and justification
trait_df <- trait_df |>
              filter(trait_name != "Leaf area index (LAI) of a single plant") |> #only has 1 value
              filter(trait_name != "Plant lifespan (longevity)") #mostly NAs and 1 value, might have to double check this!!!


#EDA

    #how many species total are there?
        unique(trait_df$spp_name)
        #739 species


    #how many oak species are there
    trait_df |> 
      count(spp_name) |>
      arrange(desc(n)) |>
      filter(str_detect(spp_name, "Quercus")) 
      #40 Quercus species


    #how many observations per trait are there?
        trait_df |>
          count(trait_name)
        #Leaf area per... undefined is highly represented.
        #Crown/canopy height has a decent amount of observations but nowhere near LMA
        #Bark thickness and leaf lifespan/longevity have around 300 observations
        
        #Data cleaning note: LAI of a single plant = 1, was removed above

    #what does the distribution of the traits look like across all species?
        trait_df |>
          filter(trait_name == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded") |> 
          ggplot(aes(x = value)) +
            geom_histogram()
        #very right skewed, logarithmic scale looks better and very normal
        trait_df |>
          filter(trait_name == "Root length per root dry mass (specific root length, SRL)") |> 
          ggplot(aes(x = value)) +
          geom_histogram()

        
        
        #setup for forloop
        traits <- unique(trait_df$trait_name) |> as.vector()
        plot_vector <- vector("list", length(traits))
        
        #for loop
        plot_function <- function(data, value_list){
          
          list_ct <- length(value_list)
          
          for(i in 1:list_ct){
            p <- data |>
              filter(trait_name == traits[i]) |>
              ggplot(aes(x = value)) +
              geom_histogram()
            
            plot_vector[[i]] <- p
          }
          
          plot_vector
          
        }

        distrib_plots <- plot_function(trait_df, traits)
      
        plot_grid(distrib_plots[[1]],
                  distrib_plots[[2]],
                  distrib_plots[[3]],
                  distrib_plots[[4]],
                  distrib_plots[[5]],
                  distrib_plots[[6]],
                  labels = traits,
                  label_size = 8,
                  hjust = -0.3)
            
              #all traits right skewed, likely will need to log-transform most of them
              #rooting length per dry mass/specific root length data seems a little sparse, so plot doesn't look great. possibly discrete values?
        