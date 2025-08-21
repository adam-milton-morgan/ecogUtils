### Sigmoid
### December 2022
### adam.milton.morgan@gmail.com

### Set path
if(! exists("path")){
  if(Sys.info()['sysname'] == 'Darwin'){ # Mac
    path = '/Users/adam/Dropbox/Research/ChickenSyntax/'
    if(Sys.info()['nodename'] == 'FLINKERLABMBP06'){
      path = '/Users/am4611/Dropbox/Research/ChickenSyntax/'
    }
  }
  if(Sys.info()['sysname'] == 'Linux'){ # Ubuntu
    path = '/home/adam/Dropbox/Research/ChickenSyntax/'
  }
} # if(! exists("path")){

# Required functions
source(paste0(path,'analysis/R/functions/sigmoid.R'))
source(paste0(path,'analysis/R/functions/smoothing.R'))

# Function
convert.ps.to.sigmoidal.significance.metric <- 
  function(
    ps = seq(10e-12,1-(10e-12),length.out = 1000),
    not.sig.asymptote = 0,
    sig.asymptote = 1,
    smoothing = TRUE,
    n.smoothing.samples.pre.and.post = 20
  ){
    # Stretch so that non-significant ps (<.8) get closer to 0 and higher ps (>.95) get closer to 1
    out <- my.sigmoid(x = 1 - ps,
                      left.asymptote = 0,
                      right.asymptote = 1,
                      x1 = .8, y1 = .1,
                      x2 = .95, y2 = .9)
    
    # Stretch out to meet input range (i.e., sig - not.sig)
    out <- out * (sig.asymptote - not.sig.asymptote)
    
    # Shift up or down to not.sig.asymptote
    out <- out + not.sig.asymptote
    
    out <- smoothing(out, n.samples.pre = n.smoothing.samples.pre.and.post)
    
    # Return
    return(out)
  }
