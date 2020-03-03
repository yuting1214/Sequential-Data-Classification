### Package
library(stringr) # For str_trim 
### Working directory
root <-"C:/Users/Public/Industry_data/Inform_manu_comp/comp_data/" #Change
setwd(root)
fold_name <- list.files(pattern="G*")
working_directory <- paste(root,fold_name,"/", sep ="")

### Iteration
#Repository
repo <- "C:/Users/陳昱廷/Desktop/comp_data_csv/" #Change
repo_dr <- paste(repo,fold_name,"/", sep ="")
#fold
for( i in seq_len(length(working_directory))){
  wdsep <- working_directory[i]
  setwd(wdsep)
  file_name <- list.files(pattern="*.txt")
  file_path <- paste(wdsep,file_name,sep = "")
  title <- unlist(strsplit(file_name, "txt"))
  file_csv <- paste(title,"csv",sep = "")
  repo_current <- repo_dr[i]
  #file
  for ( j in seq_len(length(file_path))){
    file <- file_path[j]
    file_read <-  readLines(file)
    # Treatment
    split_file <- strsplit(file_read, split="\t")
    split_file[[1]] <- NULL #space 
    split_file[[2]] <- NULL # Deg.F
    element_num <- length(split_file[[2]])
    variable_name <- split_file[[1]]
    temp <- lapply(2:length(split_file), function(i) split_file[[i]][-element_num])
    file_final <- as.data.frame(do.call(rbind, temp), stringsAsFactors=FALSE)
    colnames(file_final) <- variable_name
    #Repository
    setwd(repo_current)
    #Save CSV
    write.csv(file_final,file_csv[j])
    cat("The", j, "th iteration is finished. \n")
  }
}

