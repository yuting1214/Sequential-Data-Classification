##### Importing packages
library(tidyverse)
library(dtwclust) # dtw_lb
library(caret) # createFolds
library(parallel) # Parlapply 
##### (2) Loading data
### (2-1)Training data
# Retrieve Training data label
setwd("C:/Users/Public/Industry_data/") # Change
training_data <- read.csv("Merged_data.csv")
fold_index <- training_data[,"fold"]
True_label <- as.character(fold_index)
# Format Training data into list
list_training_data <- function(){
  #Data directory
  root <-"C:/Users/Public/Industry_data/comp_data_csv/" #Change
  setwd(root)
  fold_name <- list.files(pattern="G*")
  working_directory <- paste(root,fold_name,"/", sep ="")
  # Retrive datasets into list
  data_list <- list()
  for( i in seq_len(length(working_directory))){
    wdsep <- working_directory[i]
    setwd(wdsep)
    fold_name <- list.files(pattern="G*")
    #file
    for ( j in seq_len(length(fold_name))){
      file <- fold_name[j]
      file_read <-  read.csv(file)
      data_list[[file]] <- file_read
      cat("The", j, "th iteration is finished. \n")
    }
  }
  # Treatment 
  settle <- function(df){
    df <- df[-1]
    names(df) <- NULL
    data_list_tidy <- as.list(df)
    return(data_list_tidy)
  }
  data_list_tidy <- sapply(data_list, settle)
  # Organize
  training_data_list <- list()
  for( i in seq_len(length(data_list_tidy))){
    item_num <- length(data_list_tidy[[i]])
    for( j in seq_len(item_num)){
      training_data_list <- c(training_data_list, data_list_tidy[[i]][j])
    }
  }
  return(training_data_list)
}
training_data_list <- list_training_data()
# Reinterpolate training data list  to same length
training_data_list_new <- reinterpolate(training_data_list, new.length = max(lengths(training_data_list)))

### (2-2) Testing data
# Format Testing data into list
list_testing_data <- function(){
  #Data directory
  root <-"C:/Users/Public/Industry_data/testing_csv/" #Change
  setwd(root)
  file_name <- list.files(pattern=".csv")
  file_dire <- paste(root,file_name, sep ="")
  name_list <- unlist(strsplit(file_name, ".csv"))
  # Retrive datasets into list
  data_list <- list()
  for( i in seq_len(length(file_dire))){
    file <- file_dire[i]
    file_read <-  read.csv(file)
    data_list[[name_list[i]]] <- file_read
    cat("The", i, "th iteration is finished. \n")
  }
  # Treatment 
  settle <- function(df){
    df <- df[-1]
    names(df) <- NULL
    data_list_tidy <- as.list(df)
    return(data_list_tidy)
  }
  data_list_tidy <- sapply(data_list, settle)
  # Organize
  testing_data_list <- list()
  for( i in seq_len(length(data_list_tidy))){
    item_num <- length(data_list_tidy[[i]])
    for( j in seq_len(item_num)){
      testing_data_list <- c(testing_data_list, data_list_tidy[[i]][j])
    }
    name_index <- sapply(data_list, function(x) (dim(x)[2]-1))
    file_name <- rep(name_list, name_index)
  }
  return(list(testing_data_list = testing_data_list, file_name = file_name) )
}
testing_data <-  list_testing_data()
testing_data_list <- testing_data$testing_data_list
testing_data_file_name <- testing_data$file_name

### Reinterpolate testig data to same length(equal to training data length)
testing_data_list_new <- reinterpolate(testing_data_list, new.length = max(lengths(training_data_list)))

##### (3) Main function
### (3-1) KNN 
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
### Example
classify_series(query = training_data_list_new[[100]], database =  training_data_list_new[-100])
fold_index[100]

###(3-2) Leave one out validation function
LOU <- function(NN, data = training_data_list_new){
  test_result <- rep(NA, length(data))
  for( i in seq_len(length(data)) ){
    result <- classify_series(query = data[[i]],database =  data[-i],
                              NN = NN, Filter = T, window_size = 10L)
    test_result[i] <- result
    cat("The", i, "th iteration is finished. \n")
  }
  return(test_result)
}

###(3-3) Leave all variables in the same file out .
LSO <- function(NN, data = training_data_list_new){
  test_result <- rep(NA, length(data))
  for( i in seq_len(length(data)) ){ 
    file_title <- training_data %>% filter(X == i) %>% pull(file_names)
    precluded_index <- training_data %>% filter(file_names == file_title) %>% pull(X)
    result <- classify_series(query = data[[i]],database =  data[-precluded_index],
                              NN = NN, Filter = T, window_size = 10L)
    test_result[i] <- result
    cat("The", i, "th iteration is finished. \n")
  }
  return(test_result)
}

##### (4) Evaluation
#LOU
# K = 1, window_size = 10
T1 <- LOU(NN = 1,  data = training_data_list_new)
### (4-1)Confusion matrix
table(T1, True_label)
### (4-2) Accuracy
sum(T1 == True_label)/length(True_label)

#LSO
# K = 1, window_size = 10
S1 <- LSO(NN = 1,  data = training_data_list_new)
### (4-1)Confusion matrix
table(S1, True_label)
### (4-2) Accuracy
sum(S1 == True_label)/length(True_label)

##### (5) Prediction
# (1) window_size = 10
Result1 <- rep(NA, length(testing_data_list_new))
for( i in seq_len(length(Result1))){
  Result1[i] <- classify_series(query = testing_data_list_new[[i]], database =  training_data_list_new, NN=1, Filter = T)
  cat("The", i, "th iteration is finished. \n")
}
Final_Result1 <- data.frame(file = as.numeric(testing_data_file_name), Prediction = Result1)
View(Final_Result1)
# (1-1)
View(Final_Result1 %>% arrange(file))
# (1-2)
View(Final_Result1 %>% group_by(file) %>% unique() %>% arrange(file))
# (1-3)
View(Final_Result1 %>% group_by(file) %>% summarise(count = n_distinct(Prediction))%>% arrange(desc(count)) )

# (2) Win size = 18
Result2 <- rep(NA, length(testing_data_list_new))
for( i in seq_len(length(Result2))){
  Result2[i] <- classify_series(query = testing_data_list_new[[i]],
                                database =  training_data_list_new, NN=1, Filter = T,
                                window_size = 18L )
  cat("The", i, "th iteration is finished. \n")
}
Final_Result2 <- data.frame(file = as.numeric(testing_data_file_name), Prediction = Result2)
View(Final_Result2)
# (1-1) Final result
View(Final_Result2 %>% arrange(file))
# (1-2) Different Category predicted in each csv file
View(Final_Result2 %>% group_by(file) %>% unique() %>% arrange(file))
# (1-3)  Different Category amount predicted in each csv file
View(Final_Result2 %>% group_by(file) %>% summarise(count = n_distinct(Prediction))%>% arrange(desc(count)) )

##### Final decision
Split_df <- split(Final_Result1, Final_Result1$file)
#Decision function
Assign <- function(x){
  table <- table(x)
  fold_cate <- colnames(table)
  fold_cate[which.max(table)]
}
Prediction_result <- sapply(Split_df, Assign)
Prediction_result
# Prediction distribution
table(Prediction_result)

##### Export result
file_name <- paste(1:36, ".txt",sep = "")
result_data <- data.frame(file_name = file_name, Prediction = Prediction_result)
write.csv(result_data, "Submit.csv")
