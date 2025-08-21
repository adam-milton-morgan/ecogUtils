### Moving window average smoothing function
### Winter 2021
### adam.milton.morgan@gmail.com

matrix.smoothing <- function(matrix,
                             row.labels=NA,
                             col.labels=NA,
                             n.samples.pre=30, 
                             n.samples.post=n.samples.pre, 
                             na.pad=FALSE,
                             output.class=class(matrix)[1]){
  
  # Set up labels
  if(is.na(row.labels)){
    row.labels = rownames(matrix)
  }
  if(is.na(col.labels)){
    col.labels = colnames(matrix)
  }
  
  # Convert
  matrix <- data.matrix(matrix)
  
  # Create blank matrix to store results in:
  output <- matrix(nrow=nrow(matrix),
                  ncol=ncol(matrix))
  
  # Loop thru rows and columns
  for(row in (n.samples.pre+1):(nrow(matrix)-n.samples.post)){
    for(col in (n.samples.pre+1):(ncol(matrix)-n.samples.post)){
      output[row,col] <- 
        mean(matrix[(row - n.samples.pre):(row + n.samples.post),
                    (col - n.samples.pre):(col + n.samples.post)],
                              na.rm=TRUE)
    }
  }
  
  # Convert to data.frame if desired
  if(output.class == "data.frame"){
    output <- data.frame(output)
  }
  
  # Add labels
  colnames(output) <- as.character(col.labels)
  rownames(output) <- as.character(row.labels)
  
  # Get rid of NA rows/columns if not na.pad
  if(! na.pad){
    output = output[(n.samples.pre+1):(nrow(matrix)-n.samples.post),
                    (n.samples.pre+1):(ncol(matrix)-n.samples.post)]
  }
  
  # End
  return(output)
}