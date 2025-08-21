### Convert elec data to sample data or reverse
### Fall 2023
### adam.milton.morgan@gmail.com


### Formats for input/output:
# elec.data: list of dataframes, where each dataframe is a trial (rox) X time sample (column) matrix for a single elec
# sample.data: list of dataframes, where each dataframe is a trial (rox) X elec (column) matrix for a single time sample



reverse.data.frame.list.hierarchy <- function(data.list) {
  
  library('abind')
  
  # Output the same class as input
  input.class <- class(data.list[[1]])
  
  # 3-dimensional array
  data.list <- abind(data.list, along = 3)
  
  # Reorganize as list
  output <- list()
  for(col.loop in dimnames(data.list)[[2]]){
    output[[col.loop]] <- data.list[,col.loop,]
  }
  
  # If input class was data.frame, convert
  if("data.frame" %in% input.class){
    output <- lapply(output, data.frame)
  }
  
  # Output
  return(output)
}

