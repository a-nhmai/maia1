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