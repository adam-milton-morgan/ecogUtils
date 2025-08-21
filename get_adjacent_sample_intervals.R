### Get start and end labels of windows of adjacent significant samples
### Fall 2021
### adam.milton.morgan@gmail.com

get.adjacent.sample.intervals <- 
  function(
    values,
    labels = 1:length(values),
    label.type = c("index", "time", "sample", "sample.label")[1]
  ){
    
    # Interval labels
    start.label <- paste0("start.", label.type)
    end.label <- paste0("end.", label.type)
    
    # Create list of groups of adjacent significant samples
    windows <- list()
    window.number <- 0
    # Manually code first entry if it starts on first row
    if(values[1] == 1){
      window.number <- 1
      windows[['w1']] <- c(labels[1], NA, NA)
    }
    # Loop thru rows and add list entries for each new window of adjacent sig samples
    for(sample.loop in 2:length(values)){ # row.loop = 2
      if(values[sample.loop] == 1){
        if(values[sample.loop - 1] == 0){
          # Initialize entry
          window.number <- window.number + 1
          windows[[paste0('w',window.number)]] <- 
            c(labels[sample.loop], NA, NA)
        }
      }else{ # i.e., if current row is not significant
        if(values[sample.loop - 1] == 1){
          windows[[paste0('w',window.number)]][2] <- 
            labels[sample.loop]
          windows[[paste0('w',window.number)]][3] <-
            windows[[paste0('w',window.number)]][2] - windows[[paste0('w',window.number)]][1]
          names(windows[[paste0('w',window.number)]]) <- c(start.label,end.label,'difference')
        }
      }
      # If the last row is still in a sig window, add last time
      if((sample.loop == length(values)) & 
         values[sample.loop] == 1 &
         values[sample.loop - 1] == 1){
        windows[[paste0('w',window.number)]][2] <- 
          labels[sample.loop]
        windows[[paste0('w',window.number)]][3] <-
          windows[[paste0('w',window.number)]][2] - windows[[paste0('w',window.number)]][1]
        names(windows[[paste0('w',window.number)]]) <- c(start.label,end.label,'difference')
      }
      # If just the last sample is its own significant window, just don't save that window I guess?
      if((sample.loop == length(values)) & 
         values[sample.loop] == 1 &
         values[sample.loop - 1] == 0){
        windows[[paste0('w',window.number)]] <- NULL
      }
    }
       
  # End
  return(windows)
}