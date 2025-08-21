### Time convert function
### Winter 2021
### adam.milton.morgan@gmail.com


### Formats for input/output:
# 'samples': -512 or 1024
# 'sample.labels': "sample_neg512" or "sample_1024"
# 'times': -1000 or 2000 

time.convert <- function(input, 
                         input.type = 'samples', 
                         output.type = 'times',
                         input.sampling.rate = 512,
                         output.sampling.rate = NULL,
                         drop.labels = FALSE){
  
  # R's default "round" function rounds .5 to the nearest EVEN integer, so using a custom function to round .5s up 
  my.round = function(x, digits = 0){
    out <- x * 10 ^ digits
    after.dot <- abs(out - floor(out))
    out[after.dot == .5] <- out[after.dot == .5] + .5
    out <- round(out, digits = 0)
    return(out)
  } # my.round()
  
  # Get sample.labels, samples, and times at the input sampling rate
  if(input.type == 'sample.labels'){
    sample.labels = input
    samples = as.numeric(gsub('sample_','',gsub('neg','-',sample.labels)))
    times = my.round(samples / input.sampling.rate * 1000)
  }
  if(input.type == 'samples'){
    samples = as.numeric(input)
    sample.labels = paste0('sample_',gsub('-','neg',as.character(samples)))
    times = my.round(samples / input.sampling.rate * 1000)
  }
  if(input.type == 'times'){
    times = as.numeric(input)
    samples = my.round(times * input.sampling.rate / 1000)
    sample.labels = paste0('sample_',gsub('-','neg',as.character(samples)))
  }
  
  # Translate sampling rates
  if(is.null(output.sampling.rate)){
    output.sampling.rate <- input.sampling.rate
  }else{
    if(output.sampling.rate != input.sampling.rate){
      
      # Translate from input to output sampling rate, locked to 0
      samples <- my.round(samples / input.sampling.rate * output.sampling.rate)
      sample.labels <- paste0('sample_',gsub('-','neg',as.character(samples)))
      
    } # if(output.sampling.rate != input.sampling.rate){
  } # if(is.null(output.sampling.rate)){}else{
  
  # Get output
  output <- list('times'=times,
                 'samples'=samples, 
                 'sample.labels'=sample.labels)[[output.type]]
  
  # If non-unique (e.g., because sampling rate decrease rounded two samples to same value), message
  if(length(unique(output)) != length(output)){
    # (Unless input values were repeated)
    if(length(unique(output)) != length(unique(input))){
      message('WARNING: Repeated values in output of time.convert()! (Possibly due to downsampling and rounding.)') 
    }
  }
  
  # Add back labels
  if((! is.null(names(input))) & (! drop.labels)){
    if(length(output) == length(input)){
      names(output) <- names(input)  
    } # if(length(output) == length(input)){
  } # if((! is.null(names(input))) & (! drop.labels)){
  return(output)
}