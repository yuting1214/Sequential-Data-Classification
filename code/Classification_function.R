##### Importing packages
library(tidyverse)
library(dtwclust) # dtw_lb

### Main function
classify_series <- function(query, database, NN = 1, Filter = T, window_size = 30L) {
  # Similarity
  similarity <- dtw_lb(database, query, window.size = window_size,nn.margin = 2L)
  similarity_sorted_index <- order(similarity)
  # Nearest neighbor
  Nearest_index <-similarity_sorted_index[1:NN]
  index <- as.character(fold_index)
  Nearest_Nei_label <- index[Nearest_index]
  # Find the Nearest element
  Nearest_label <- sort(table(Nearest_Nei_label), decreasing = T)[1]
  niche <- ceiling(NN/2)
  Check <- Nearest_label >= niche
  if(Check){
    final_label <- names(Nearest_label)
  }else{
    final_label <- "pending"
  }
  
  if(Filter){
    return(final_label)
  }else{
    return( list(Nearest_Neighbor_set = Nearest_Nei_label,  Nearest_element = final_label) )
  }
  
}
