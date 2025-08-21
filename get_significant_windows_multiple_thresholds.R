### Get start and end times of windows of adjacent significant samples
### Fall 2021
### adam.milton.morgan@gmail.com

get.significant.windows.multiple.thresholds <- 
  function(..p.values,
           ..alphas = c(.05, .01),
           ..exclude.sig.durations.under.ms = c(100, 50),
           ..times = NULL,
           ..samples = NULL,
           ..sample.labels = NULL,
           ..alternative.labels = NULL,
           ..output.class = c('list','data.frame')[1],
           ..include.duration = FALSE,
           ..exclude.times.before.ms = NULL,
           ..exclude.times.after.ms = NULL,
           ..output.units = c('times','samples','alternative.labels')[1],
           ..sampling.rate = 512){
    
    library('dplyr')
    # Function to convert between samples, sample labels, and times
    source(paste0(path,'/analysis/R/functions/time_convert.R'))
    # Get significant windows
    source(paste0(path,'/analysis/R/functions/get_significant_windows.R'))
    # Get significant windows inverse
    source(paste0(path,'/analysis/R/functions/get_significant_windows_inverse.R'))
    
    # Create ordinal indices if not given
    if(sum(as.numeric(is.null(..times)), 
           as.numeric(is.null(..samples)), 
           as.numeric(is.null(..alternative.labels)), 
           as.numeric(is.null(..sample.labels))) != 3){
      message('WARNING: One (and only one) of these must be given: ..times, ..samples, ..sample.labels, ..alternative.labels. Assigning samples starting at 1.')
      ..samples = 1:length(..p.values)
    }
    if(!is.null(..times)){
      ..samples <- time.convert(..times, "times", "samples", ..sampling.rate)
    }
    if(!is.null(..sample.labels)){
      ..samples <- time.convert(..sample.labels, "sample.labels", "samples", ..sampling.rate)
    }
    if(!is.null(..alternative.labels)){
      ..samples <- 1:length(..alternative.labels)
    }
    
    if(length(..alphas) != length(..exclude.sig.durations.under.ms)){
      stop('ERROR (get.significant.windows.multiple.thresholds): "..alphas" and ".exclude.sig.duration.under.ms" are paired (i.e., they have different lengths).')
    }
    
    ### Get sig values for each threshold (i.e., vectors of 1s and 0s)
    all.sigs <- rep(0, times = length(..samples))
    
    for(threshold.loop in 1:length(..alphas)){ # threshold.loop = 1
      current.sig.windows <-
        get.significant.windows(sig.vals = ..p.values < ..alphas[threshold.loop],
                              .exclude.sig.durations.under.ms = ..exclude.sig.durations.under.ms[threshold.loop],
                              .samples = ..samples,
                              output.class = 'data.frame',
                              include.duration = TRUE,
                              .exclude.times.before.ms = ..exclude.times.before.ms,
                              .exclude.times.after.ms = ..exclude.times.after.ms,
                              .sampling.rate = ..sampling.rate)
    
      all.sigs <- all.sigs + 
        get.significant.windows.inverse(sig.windows = current.sig.windows,
                                        .samples = ..samples,
                                        .sampling.rate = ..sampling.rate)
    }; rm(threshold.loop)
    
    # Combine significant samples: true anywhere significant for any threshold
    all.sigs <- (all.sigs > 0)
    
    ### Get sig windows
    sig.windows <-
      get.significant.windows(all.sigs,
                              .samples = ..samples,
                              output.class = ..output.class,
                              .output.units = ..output.units,
                              include.duration = ..include.duration,
                              .sampling.rate = ..sampling.rate)
     
    # End
    return(sig.windows)
  }