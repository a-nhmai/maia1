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