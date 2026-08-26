#FIA exploration
#Simplifying at MN_TREE.csv from FIA website https://research.fs.usda.gov/products/dataandtools/fia-datamart
#Anh Mai
#07 August 2026


lib_ls <- c("dplyr",
            "ggplot2",
            "forcats",
            "janitor",
            "stringr",
            "readxl",
            "ggthemes")

sapply(lib_ls, library, character.only = TRUE)

######
#DATA#
######

  ####################
  #Load Many Function#
  ####################
    
  #setup for function
    
  #inputs into function
  dir <- "~/Documents/Data/FIA"
  fia_metrics <- readxl::read_excel("260807_FIA_exploration/selected_FIA.xlsx")
  rs_ref <- read.csv("~/Documents/Data/FIADB_REFERENCE/REF_RESEARCH_STATION.csv") |> dplyr::select(STATECD, RS)
    
  fia_many <- function(dir, fia_metrics, rs_ref){
        
        ################################
        #Setting up directory and files#
        ################################
        directory <- paste0(dir) 
        file_names <- list.files(directory)
        
        ###############
        #Forloop setup#
        ###############
        
        ls <- list()
        metric_ls <- fia_metrics[,1] |> unlist(use.names = FALSE) #setting up filter for columns
        
        #########
        #Forloop#
        #########
        
        for(i in length(file_names)){
        csv <- read.csv(paste0(directory, "/" , file_names[i]))
        state_name <- stringr::str_extract(file_names[i], "[A-Z]{2}")
        
        fia_filt <- csv |>
                    dplyr::select(all_of(metric_ls)) |> #filter the columns
                    dplyr::mutate(state = state_name) |> #state acronym
                    dplyr::left_join(rs_ref, dplyr::join_by(STATECD))  #research station associated
        
        ls[[i]] <- fia_filt
        }
        
        fia_df <- data.table::rbindlist(ls) |> janitor::clean_names() 
        
        write.csv(fia_df, paste0(directory, "/", "FIA_combined.csv"))
  }
        
  fia_single <- function(path, fia_metrics, rs_ref){
    
    csv <- read.csv(paste0(path))
    
    metric_ls <- fia_metrics[,1] |> unlist(use.names = FALSE) #setting up filter for columns
    
    state_name <- stringr::str_extract(path, "[A-Z]{2}_") 
    state_name <- stringr::str_remove(state_name, "_")
    
    fia_filt <- csv |>
      dplyr::select(all_of(metric_ls)) |> #filter the columns
      dplyr::mutate(state = state_name) |> #state acronym
      dplyr::left_join(rs_ref, dplyr::join_by(STATECD)) |> #research station associated
      janitor::clean_names()
    
    fia_filt
  }     

  
  ########################
  #Proportion NA Function#
  ########################
  
  propNA <- function(fia_df){
    
    propNA <-  fia_df |>
      is.na() |>
      colSums()/nrow(fia_df) 
    
    propNA <- round(propNA, 2)
    
    matNA <- as.data.frame(matrix(propNA, ncol = 1, byrow = TRUE, dimnames = list(names(propNA))))
    
    matNA <- cbind(rownames(matNA), matNA)
    
    colnames(matNA) <- c("Metric", "PropNA")
    
    rownames(matNA) <- NULL
    
    p <- matNA |>
      dplyr::mutate(quantile = cut(PropNA, 4, label = FALSE),
                    Metric = forcats::fct_rev(forcats::fct_reorder(Metric, PropNA))) |>
      ggplot2::ggplot(ggplot2::aes(x = Metric, y = PropNA)) +
      ggplot2::geom_bar(stat = "identity") + 
      ggplot2::theme_classic(base_size = 10) + 
      ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
      ggplot2::coord_flip() 
    
    NA_df <- matNA |> dplyr::arrange(desc(PropNA)) |> as.data.frame()
    
    list <- c(p, NA_df)
    
  }
  


