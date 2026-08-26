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