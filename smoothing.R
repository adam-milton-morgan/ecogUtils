### Moving window average smoothing function
### Winter 2021
### adam.milton.morgan@gmail.com

smoothing <- function(time.series,
                      sample.labels=NA,
                      n.samples.pre=30, 
                      n.samples.post=n.samples.pre, 
                      na.pad=TRUE,
                      edge.handling=c('full.only','use.partial')){
  
  # Set edge.handling
  edge.handling <- match.arg(edge.handling)
  
  # Set up labels
  if(all(is.na(sample.labels))){
    labels = names(time.series)
  }else{
    labels = sample.labels
  }
  
  # Remove any NAs at the edges
  non.na.indices <- which(! is.na(time.series))
  # Trailing NAs (BEFORE LEADING ONES! Otherwise indices will change)
  exist.trailing.nas <- (! length(time.series) %in% non.na.indices)
  if(exist.trailing.nas){
    final.na.indices <- (tail(non.na.indices,1) + 1):length(time.series)
    n.final.nas <- length(final.na.indices)
    time.series <- time.series[-final.na.indices]
  }
  # Leading NAs (AFTER FINAL NAs!)
  exist.leading.nas <- (! 1 %in% non.na.indices)
  if(exist.leading.nas){
    initial.na.indices <- 1:(non.na.indices[1] - 1)
    n.initial.nas <- length(initial.na.indices)
    time.series <- time.series[-initial.na.indices]
  }
  
  # Convert
  time.series <- as.numeric(time.series)
  output <- c()
  for(i in (n.samples.pre+1):(length(time.series)-n.samples.post)){
    output = c(output, mean(time.series[(i - n.samples.pre):(i + n.samples.post)],
                            na.rm=TRUE))
  }
  
  # Add back any leading and trailing NAs
  if(exist.trailing.nas){output <- c(output, rep(NA, n.final.nas))}
  if(exist.leading.nas){output <- c(rep(NA, n.initial.nas), output)}
  
  # Re-label and NA pad
  if(na.pad){
    output = c(rep(NA, n.samples.pre), output, rep(NA, n.samples.post))
    # Add labels
    names(output) <- labels
  }else{
    names(output) <- labels[(n.samples.pre+1):(length(time.series)-n.samples.post)]
  }
  
  # End
  return(output)
}