### Get values for upper and lower tails of confidence interval around a mean
### Fall 2021
### adam.milton.morgan@gmail.com

get.confidence.interval <- function(mean,
                                    sd,
                                    which.tail = c('upper','lower')[1],
                                    alpha = .05){
  
  # How much to scale SD by
  scale = qnorm(p = 1 - (alpha / 2))
  
  # Make negative if lower tail
  if(which.tail == 'lower'){scale = -scale}
  
  # Calculate val
  output = mean + (scale * sd)
  
  # End
  return(output)
}