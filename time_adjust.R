### Time convert function
### Winter 2021
### adam.milton.morgan@gmail.com


### Takes a time value and returns the closest time for which there's an observation (in 512 Hz space)
# E.g.:
# input: -999
# output: -998

### Set path
if(Sys.info()['sysname'] == 'Darwin'){
  path = '/Users/adam/Dropbox/Research/ChickenSyntax/'} # Mac
if(Sys.info()['sysname'] == 'Linux'){
  path = '/home/adam/Dropbox/Research/ChickenSyntax/'} # Ubuntu

### Lemmas
# Function to convert between samples, sample labels, and times
source(paste0(path,'/analysis/R/functions/time_convert.R'))

### Define time.adjust function
time.adjust <- function(times){
  time.convert(time.convert(times,
                            input.type = "times", 
                            output.type = "samples"),
               input.type = "samples", 
               output.type = "times")
}

