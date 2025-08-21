### Curb high (>1) Bayes Factors and shrink low (<1) BFs toward 0
### June 2024
### adam.milton.morgan@gmail.com

## Curb extreme values -- lower than 1 toward 0, higher than 1 asymptote to 10
squish <- function(vals.to.squish, upper.asymptote = 5){
  
  ## Set up
  # Get indices of NAs and temporarily replace them with 1s, to be switched back at end
  na.indices <- which(is.na(vals.to.squish))
  vals.to.squish[na.indices] <- 1
  out <- vals.to.squish
  
  ## Squish
  # Reduce high values
  # out[out > 1] <- log(out[out > 1]) + 1 # Log
  
  # Curb high values to ~10
  discontinuity.correction <- (1 - upper.asymptote * (1 - exp(-1/upper.asymptote)))
  out[out>1] <- (upper.asymptote * (1 - exp(-out[out>1] / upper.asymptote))) + discontinuity.correction
  
  # Reduce low values (i.e., more evidence for no differences in BF)
  # out[out < 1] <- (exp(out[out < 1]) / exp(1)) ^ 3 
  out[out < 1] <- 10^(out[out<1]) / 10
  # Put NAs back
  out[na.indices] <- NA
  
  ## Output
  return(out)
} # squish()

