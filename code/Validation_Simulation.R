##### Importing packages
library(tidyverse)
library(dtwclust) # dtw_lb
library(caret) # createFolds
library(parallel) # Parlapply 
library(nnet) # which.is.max
##### Model function
classify_series <- function(query, database, NN = 1, Filter = T, window_size = 10L) {
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
# Argument Setting (Hyperparameter )
NN <- 1; window_size <- 100L
formals(classify_series)[c("NN", "window_size")] <- list(NN = NN, window_size = window_size)

##### Validation function
### (1) Leave one out validation function
LOU <- function(data = training_data_list_new, func = classify_series){
  test_result <- rep(NA, length(data))
  for( i in seq_len(length(data)) ){
    result <- do.call( func, args = list(query = data[[i]],database =  data[-i]))
    test_result[i] <- result
    cat("The", i, "th iteration is finished. \n")
  }
  return(test_result)
}

### (2) Leave all variables in the same file out .
LSO <- function(data = training_data_list_new, func = classify_series){
  test_result <- rep(NA, length(data))
  for( i in seq_len(length(data)) ){ 
    file_title <- training_data %>% filter(X == i) %>% pull(file_names)
    precluded_index <- training_data %>% filter(file_names == file_title) %>% pull(X)
    result <- do.call( func, args = list(query = data[[i]],database =  data[-precluded_index]) )
    test_result[i] <- result
    cat("The", i, "th iteration is finished. \n")
  }
  return(test_result)
}

### (3) Cross-Validate by file, and evaluate all result at once.
LFO <- function(data = training_data_list_new, func = classify_series, K_fold = 10){
  #File fold
  file_pool <- training_data %>% select(file_names) %>%unique() %>% pull(file_names) %>% as.character()
  file_fold_size <- length(file_pool)
  random_file_index <- sample(file_pool, file_fold_size)
  fold_size <- floor(file_fold_size/K_fold)
  cross_validate_index <- split(random_file_index, ceiling((1:file_fold_size)/fold_size))
  # Result Repository
  RR <- rep(NA,length(data))
  for( i in seq_len(K_fold)){
    file_title <- cross_validate_index[[i]]
    precluded_index <- training_data %>% filter(file_names %in% file_title) %>% pull(X)
    for( j in file_title){
      test_file_variable_set_index <- training_data %>% filter(file_names == j) %>% pull(X)
      for(k in test_file_variable_set_index){
        result <- do.call( func, args = list(query = data[[k]],database =  data[-precluded_index]) )
        RR[k] <- result
      }
    }
    cat("The", i, "th fold is finished. \n")
  }
  return(RR)
}

##### (4) Cross-Validate by file, and evaluate all result at once. (Balance fold category)
LBFO <- function(data = training_data_list_new, func = classify_series, K_fold = 10){
  #File fold
  td_sub <- training_data %>% select(fold,file_names) %>% distinct(file_names, .keep_all = T)
  folds <- createFolds(td_sub$fold, k = K_fold)
  RR <- rep(NA,length(data))
  for( i in seq_len(K_fold)){
    # Find precluded data index
    file_title <- as.character(td_sub[folds[[i]], "file_names"])
    label <- unlist(lapply(strsplit(file_title, "-"), function(x) x[1]))
    label_num <- training_data %>% filter(file_names %in% file_title) %>% count("fold")
    precluded_index <- training_data %>% filter(file_names %in% file_title) %>% pull(X)
    for( j in file_title){
      # Find index of  testing file's variable 
      test_file_variable_set_index <- training_data %>% filter(file_names == j) %>% pull(X)
      for(k in test_file_variable_set_index){
        result <- do.call( func, args = list(query = data[[k]],database =  data[-precluded_index]) )
        RR[k] <- result
      }
    }
    cat("The", i, "th fold is finished. \n")
  }
  return(RR)
}

##### (5) Cross-Validate by file, and evaluate each fold separately.
LFO_sep <- function( data = training_data_list_new, func = classify_series, K_fold = 5){
  #File fold
  file_pool <- training_data %>% select(file_names) %>%unique() %>% pull(file_names) %>% as.character()
  file_fold_size <- length(file_pool)
  random_file_index <- sample(file_pool, file_fold_size)
  fold_size <- floor(file_fold_size/K_fold)
  cross_validate_index <- split(random_file_index, ceiling((1:file_fold_size)/fold_size))
  #Cross-Validation function
  CV <- function(cross_validate_index){
    True_label_set <- c()
    Predicted_label_set <- c()
    file_title <- cross_validate_index
    precluded_index <- training_data %>% filter(file_names %in% file_title) %>% pull(X)
    # File stage
    for( i in file_title){
      #Find the index of variable in the file 
      test_file_variable_set_index <- training_data %>% filter(file_names == i) %>% pull(X)
      True_label <- training_data %>% filter(X %in% test_file_variable_set_index) %>% pull(fold) %>% as.character()
      True_label_set <- c(True_label_set, True_label)
      # Variable stage
      for(j in test_file_variable_set_index){
        result <- do.call(func, args = list(query = data[[j]],database =  data[-precluded_index]) )
        Predicted_label_set <- c(Predicted_label_set, result)
      }
    }
    return(list(Predicted_label = Predicted_label_set, True_label = True_label_set))
  }
  # Execution 
  prediction <- lapply(cross_validate_index, CV)
  accuracy_set <- sapply(prediction,function(x){num <- length(x[[1]]);sum(x[[1]]==x[[2]])/num} )
  mean_accuracy <- mean(accuracy_set)
  return(list(accuracy_set = accuracy_set, mean_accuracy = mean_accuracy))
}
### Parallel version
LFO_sep_para <- function( data = training_data_list_new, func = classify_series, K_fold = 10){
  #File fold
  file_pool <- training_data %>% select(file_names) %>%unique() %>% pull(file_names) %>% as.character()
  file_fold_size <- length(file_pool)
  random_file_index <- sample(file_pool, file_fold_size)
  fold_size <- floor(file_fold_size/K_fold)
  cross_validate_index <- split(random_file_index, ceiling((1:file_fold_size)/fold_size))
  inner <- environment()
  #Cross-Validation function
  CV <- function(cross_validate_index){
    True_label_set <- c()
    Predicted_label_set <- c()
    file_title <- cross_validate_index
    precluded_index <- training_data %>% filter(file_names %in% file_title) %>% pull(X)
    # File stage
    for( i in file_title){
      #Find the index of variable in the file 
      test_file_variable_set_index <- training_data %>% filter(file_names == i) %>% pull(X)
      True_label <- training_data %>% filter(X %in% test_file_variable_set_index) %>% pull(fold) %>% as.character()
      True_label_set <- c(True_label_set, True_label)
      # Variable stage
      for(j in test_file_variable_set_index){
        result <- do.call(func, args = list(query = data[[j]],database =  data[-precluded_index]) )
        Predicted_label_set <- c(Predicted_label_set, result)
      }
    }
    return(list(Predicted_label = Predicted_label_set, True_label = True_label_set))
  }
  # Execution (Parallel)
  n.cores <- detectCores(logical = FALSE)
  clust <- makeCluster(n.cores)
  clusterEvalQ(clust, {library(tidyverse);library(dtwclust)})
  clusterExport(clust, c("training_data", "fold_index"))
  clusterExport(clust , c("data", "func"), envir = inner)
  prediction <- parLapply(clust, cross_validate_index, CV)
  stopCluster(clust)
  accuracy_set <- sapply(prediction,function(x){num <- length(x[[1]]);sum(x[[1]]==x[[2]])/num} )
  mean_accuracy <- mean(accuracy_set)
  return(list(accuracy_set = accuracy_set, mean_accuracy = mean_accuracy))
}
##### (6) Cross-Validate by file, and evaluate  each fold separately. (Balance fold category)
LBFO_sep <- function(data = training_data_list_new, func = classify_series, K_fold = 10){
  #File fold
  td_sub <- training_data %>% select(fold,file_names) %>% distinct(file_names, .keep_all = T)
  folds <- createFolds(td_sub$fold, k = K_fold)
  RR <- rep(NA,length(data))
  for( i in seq_len(K_fold)){
    # Find precluded data index
    file_title <- as.character(td_sub[folds[[i]], "file_names"])
    label <- unlist(lapply(strsplit(file_title, "-"), function(x) x[1]))
    label_num <- training_data %>% filter(file_names %in% file_title) %>% count("fold")
    precluded_index <- training_data %>% filter(file_names %in% file_title) %>% pull(X)
    for( j in file_title){
      # Find index of  testing file's variable 
      test_file_variable_set_index <- training_data %>% filter(file_names == j) %>% pull(X)
      for(k in test_file_variable_set_index){
        result <- do.call( func, args = list(query = data[[k]],database =  data[-precluded_index]) )
        RR[k] <- result
      }
    }
    cat("The", i, "th fold is finished. \n")
  }
  return(RR)
}
### Parallel code
LBFO_sep_para <- function(data = training_data_list_new, func = classify_series, K_fold = 10){
  #File fold
  td_sub <- training_data %>% select(fold,file_names) %>% distinct(file_names, .keep_all = T)
  td_sub_file_names <- as.character(td_sub$file_names)
  folds <- createFolds(td_sub$fold, k = K_fold)
  cross_validate_index <- lapply(folds, function(x) td_sub_file_names[x])
  inner <- environment()
  #Cross-Validation function
  CV <- function(cross_validate_index){
    True_label_set <- c()
    Predicted_label_set <- c()
    file_title <- cross_validate_index
    precluded_index <- training_data %>% filter(file_names %in% file_title) %>% pull(X)
    # File stage
    for( i in file_title){
      #Find the index of variable in the file 
      test_file_variable_set_index <- training_data %>% filter(file_names == i) %>% pull(X)
      True_label <- training_data %>% filter(X %in% test_file_variable_set_index) %>% pull(fold) %>% as.character()
      True_label_set <- c(True_label_set, True_label)
      # Variable stage
      for(j in test_file_variable_set_index){
        result <- do.call(func, args = list(query = data[[j]],database =  data[-precluded_index]) )
        Predicted_label_set <- c(Predicted_label_set, result)
      }
    }
    return(list(Predicted_label = Predicted_label_set, True_label = True_label_set))
  }
  # Execution (Parallel)
  n.cores <- detectCores(logical = FALSE)
  clust <- makeCluster(n.cores)
  clusterEvalQ(clust, {library(tidyverse);library(dtwclust)})
  clusterExport(clust, c("training_data", "fold_index"))
  clusterExport(clust , c("data", "func"), envir = inner)
  prediction <- parLapply(clust, cross_validate_index, CV)
  stopCluster(clust)
  
  accuracy_set <- sapply(prediction,function(x){num <- length(x[[1]]);sum(x[[1]]==x[[2]])/num} )
  mean_accuracy <- mean(accuracy_set)
  return(list(accuracy_set = accuracy_set, mean_accuracy = mean_accuracy))
  
}

##### (7) Cross-Validate by file,  evaluate each fold separately, and label is decide by the majority of varables 
LFO_file_para <- function( data = training_data_list_new, func = classify_series, K_fold = 10){
  #File fold
  file_pool <- training_data %>% select(file_names) %>%unique() %>% pull(file_names) %>% as.character()
  file_fold_size <- length(file_pool)
  random_file_index <- sample(file_pool, file_fold_size)
  fold_size <- floor(file_fold_size/K_fold)
  cross_validate_index <- split(random_file_index, ceiling((1:file_fold_size)/fold_size))
  inner <- environment()
  #Cross-Validation function
  CV <- function(cross_validate_index){
    True_label_set <- c()
    Predicted_file_label <- c()
    file_title <- cross_validate_index
    precluded_index <- training_data %>% filter(file_names %in% file_title) %>% pull(X)
    # Label determinal function
    Assign <- function(x){
      table <- table(x)
      fold_cate <- names(table)
      fold_cate[which.is.max(table)]
    }
    # File stage
    for( i in file_title){
      #Find the index of variable in the file 
      test_file_variable_set_index <- training_data %>% filter(file_names == i) %>% pull(X)
      True_label <- training_data %>% filter(X %in% test_file_variable_set_index) %>% pull(fold) %>% as.character() %>% unique()
      True_label_set <- c(True_label_set, True_label)
      Predicted_label_set <- c()
      # Variable stage
      for(j in test_file_variable_set_index){
        result <- do.call(func, args = list(query = data[[j]],database =  data[-precluded_index]) )
        Predicted_label_set <- c(Predicted_label_set, result)
      }
      Predicted_file_label <- c(Predicted_file_label, Assign(Predicted_label_set))
    }
    return(list(Predicted_label = Predicted_file_label, True_label = True_label_set))
  }
  # Execution (Parallel)
  n.cores <- detectCores(logical = FALSE)
  clust <- makeCluster(n.cores)
  clusterEvalQ(clust, {library(tidyverse);library(dtwclust);library(nnet)})
  clusterExport(clust, c("training_data", "fold_index"))
  clusterExport(clust , c("data", "func"), envir = inner)
  prediction <- parLapply(clust, cross_validate_index, CV)
  stopCluster(clust)
  accuracy_set <- sapply(prediction,function(x){num <- length(x[[1]]);sum(x[[1]]==x[[2]])/num} )
  mean_accuracy <- mean(accuracy_set)
  return(list(accuracy_set = accuracy_set, mean_accuracy = mean_accuracy))
}
##### (8) Cross-Validate by file,  evaluate each fold separately, and label is decide by the majority of varables (Balance fold category) 
LBFO_file_para <- function(data = training_data_list_new, func = classify_series, K_fold = 10){
  #File fold
  td_sub <- training_data %>% select(fold,file_names) %>% distinct(file_names, .keep_all = T)
  td_sub_file_names <- as.character(td_sub$file_names)
  folds <- createFolds(td_sub$fold, k = K_fold)
  cross_validate_index <- lapply(folds, function(x) td_sub_file_names[x])
  inner <- environment()
  #Cross-Validation function
  CV <- function(cross_validate_index){
    True_label_set <- c()
    Predicted_file_label <- c()
    file_title <- cross_validate_index
    precluded_index <- training_data %>% filter(file_names %in% file_title) %>% pull(X)
    # Label determinal function
    Assign <- function(x){
      table <- table(x)
      fold_cate <- names(table)
      fold_cate[which.is.max(table)]
    }
    # File stage
    for( i in file_title){
      #Find the index of variable in the file 
      test_file_variable_set_index <- training_data %>% filter(file_names == i) %>% pull(X)
      True_label <- training_data %>% filter(X %in% test_file_variable_set_index) %>% pull(fold) %>% as.character() %>% unique()
      True_label_set <- c(True_label_set, True_label)
      Predicted_label_set <- c()
      # Variable stage
      for(j in test_file_variable_set_index){
        result <- do.call(func, args = list(query = data[[j]],database =  data[-precluded_index]) )
        Predicted_label_set <- c(Predicted_label_set, result)
      }
      Predicted_file_label <- c(Predicted_file_label, Assign(Predicted_label_set))
    }
    return(list(Predicted_label = Predicted_file_label, True_label = True_label_set))
  }
  # Execution (Parallel)
  n.cores <- detectCores(logical = FALSE)
  clust <- makeCluster(n.cores)
  clusterEvalQ(clust, {library(tidyverse);library(dtwclust);library(nnet)})
  clusterExport(clust, c("training_data", "fold_index"))
  clusterExport(clust , c("data", "func"), envir = inner)
  prediction <- parLapply(clust, cross_validate_index, CV)
  stopCluster(clust)
  accuracy_set <- sapply(prediction,function(x){num <- length(x[[1]]);sum(x[[1]]==x[[2]])/num} )
  mean_accuracy <- mean(accuracy_set)
  return(list(accuracy_set = accuracy_set, mean_accuracy = mean_accuracy))
}

# Example 
#LOU
# K = 1, window_size = 10
LOU1 <- LOU(data = training_data_list_new,  func = classify_series)
### (4-1)Confusion matrix
table(LOU1, True_label)
### (4-2) Accuracy
sum(LOU1 == True_label)/length(True_label)

#LSO
# K = 1, window_size = 10
LSO1 <- LSO(data = training_data_list_new, func = classify_series)
### (4-1)Confusion matrix
table(LSO1, True_label)
### (4-2) Accuracy
sum(LSO1 == True_label)/length(True_label)

#LFO
# K = 1, window_size = 10 , K_fold 5 
LFO1 <- LFO(data = training_data_list_new, func = classify_series, K_fold = 5)
### (4-1)Confusion matrix
table(LFO1, True_label)
### (4-2) Accuracy
sum(LFO1 == True_label)/length(True_label)

#LBFO
# K = 1, window_size = 10, K_fold 5 
LBFO1 <- LBFO(data = training_data_list_new, func = classify_series, K_fold = 20)
### (4-1)Confusion matrix
table(LBFO1, True_label)
### (4-2) Accuracy
sum(LBFO1 == True_label)/length(True_label)

# LFO_sep_para
LFO_sep_para1 <- LFO_sep_para(data = training_data_list_new, func = classify_series, K_fold = 10)
LFO_sep_para1
# LBFO_sep_para1\
LBFO_sep_para1 <- LBFO_sep_para(data = training_data_list_new, func = classify_series, K_fold = 10)
LBFO_sep_para1
