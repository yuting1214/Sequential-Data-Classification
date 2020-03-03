### Package
library(ggplot2)
library(stringr) # For str_trim 
library(reshape2)
### Data location
root <-"C:/Users/陳昱廷/Desktop/Inform_manu_comp/comp_data_csv/" #Change
setwd(root)
fold_name <- list.files(pattern="G*")
working_directory <- paste(root,fold_name,"/", sep ="")

### Iteration
#Repository
repo <- "C:/Users/陳昱廷/Desktop/Inform_manu_comp/plot/" #Change
repo_dr <- paste(repo,fold_name,"/", sep ="")
#fold
for( i in seq_len(length(working_directory))){
  wdsep <- working_directory[i]
  setwd(wdsep)
  file_name <- list.files(pattern=".csv")
  title_list <- unlist(strsplit(file_name, "csv"))
  repo_current <- repo_dr[i]
  #file
  for ( j in seq_len(length(file_name))){
    file_sep <-file_name[j]
    file_final <- paste(wdsep, file_sep, sep = "")
    data <- read.csv(file_final)
    #Treatment
    data_melted <- melt(data, id = "X")
    plot <- ggplot(data = data_melted, aes(x = X, y = value, color = variable)) +
      geom_line()
    #Save CSV
    file_title <- title_list[j]
    title <- paste(file_title,"png",sep ="")
    ggsave(title,plot)
    cat("The", j, "th iteration is finished. \n")
  }
}
