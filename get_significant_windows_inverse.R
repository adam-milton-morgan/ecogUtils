### Given significant windows and time labels, return string of 1s and 0s for significance
### Summer 2024
### adam.milton.morgan@gmail.com

get.significant.windows.inverse <- 
  function(sig.windows,
           .times = NULL,
           .samples = NULL,
           .sample.labels = NULL,
           .alternative.labels = NULL,
           output.class = c('vector','data.frame')[1],
           .output.units = c('sample.labels','times','samples','alternative.labels')[1],
           .sampling.rate = 512){
    
    library('dplyr')
    # Function to convert between samples, sample labels, and times
    source(paste0(path,'/analysis/R/functions/time_convert.R'))
    
    # Convert input to dataframe if it isn't already
    if('list' %in% class(sig.windows)){
      if(length(sig.windows) == 0){
        sig.windows <- data.frame('start.time' = NA, 'end.time' = NA, 'duration' = NA)[0,]
      }else{
        sig.windows <- data.frame(bind_rows(sig.windows))
      }
    }
    
    # Get sig samples
    sig.samples <- c()
    sig.sample.labels <- c()
    if(nrow(sig.windows) > 0){
      sig.windows$start.sample <- time.convert(sig.windows$start.time, 'times', 'samples', .sampling.rate)
      sig.windows$end.sample <- time.convert(sig.windows$end.time, 'times', 'samples', .sampling.rate)
      for(window.loop in 1:nrow(sig.windows)){
        sig.samples <- c(sig.samples,
                         sig.windows[window.loop, 'start.sample']:sig.windows[window.loop, 'end.sample'])
      }
      sig.samples <- unique(sig.samples)
      sig.sample.labels <- time.convert(sig.samples, 'samples', 'sample.labels', .sampling.rate)
    }
    
    # Create ordinal indices if not given
    if(sum(as.numeric(is.null(.times)),
           as.numeric(is.null(.samples)),
           as.numeric(is.null(.alternative.labels)),
           as.numeric(is.null(.sample.labels))) != 3){
      stop('ERROR: One (and only one) of these must be given: .times, .samples, .sample.labels, .alternative.labels.')
    }
    if(!is.null(.times)){
      .samples <- time.convert(.times, "times", "samples", .sampling.rate)
      .sample.labels <- time.convert(.samples, "samples", "sample.labels", .sampling.rate)
    }
    if(!is.null(.samples)){
      .sample.labels <- time.convert(.samples, "samples", "sample.labels", .sampling.rate)
      .times <- time.convert(.samples, "samples", "times", .sampling.rate)
    }
    if(!is.null(.sample.labels)){
      .samples <- time.convert(.sample.labels, "sample.labels", "samples", .sampling.rate)
    }
    if(!is.null(.alternative.labels)){
      .samples <- 1:length(.alternative.labels)
      .sample.labels <- time.convert(.samples, "samples", "sample.labels", .sampling.rate)
    }

    # Initialize sig vals
    sig.vals <- setNames(rep(0, times = length(.sample.labels)), .sample.labels)
    
    # Set significant windows to 1
    sig.vals[sig.sample.labels] <- 1
    
    # Set output units
    if(.output.units == "times"){
      names(sig.vals) <- time.convert(.sample.labels, "sample.labels", "times", .sampling.rate)
    }
    if(.output.units == "samples"){
      names(sig.vals) <- .samples
    }
    if(.output.units == "alternative.labels"){
      names(sig.vals) <- .alternative.labels
    }
    
    # Convert to dataframe
    if(output.class == "data.frame"){
      sig.vals <- data.frame('significant' = sig.vals,
                             'time' = time.convert(.sample.labels, "sample.labels", "times", .sampling.rate),
                             'sample' = .samples,
                             'sample.label' = .sample.labels)
      rownames(sig.vals) <- .sample.labels
    }
    
    # End
    return(sig.vals)
  }