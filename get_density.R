### Get density functions for gamma and normal distributions
### Winter 2024 (!!!!!!)
### adam.milton.morgan@gmail.com


### Define function to get density for violin plots
## Normal distribution

get.density <- function(.data,
                        .smoothing.factor = 1,
                        .distribution = c('normal','gamma')[1],
                        .gamma.dist.lower.limit = NULL){
  if(.distribution == 'normal'){
    my.density <- function(.data){
      .density <- density(.data, adjust = .smoothing.factor)
      .density <- data.frame('x' = .density$x, 'y' = .density$y)
      return(.density)
    }
  } # if(.distribution == 'normal'){
  ## Gamma distribution
  if(.distribution == 'gamma'){
    my.density <- function(.data){
      # Hacky - density() tapers at both ends (as in normal distribution).
      # This removes the taper and negative values from left end and smooths with a Hann window.
      library('gsignal') # for hann window
      .density <- density(.data, adjust = .smoothing.factor)
      .density <- data.frame('x' = .density$x, 'y' = .density$y)
      
      # Remove any rows corresponding to values below the observed minimum
      if(is.null(.gamma.dist.lower.limit)){
        .gamma.dist.lower.limit <- min(.data)
      }
      .density <- .density[.density$x >= max(.density$x[.density$x < .gamma.dist.lower.limit]),]
      .density$y[1] <- 0
      # Adjust first several points -- function of where the global peak is, bounded at 3 and 100
      points.to.modify <- max(3, min(100, round(which.max(.density$y) * .5)))
      .density$y[1:points.to.modify] <- 
        # .density$y[1:points.to.modify] * seq(0, 1, length.out = points.to.modify)
        .density$y[1:points.to.modify] * (hann(n = points.to.modify * 2)[1:points.to.modify] ^ .25)
      # lines(x = .density$x, y = .density$y, col = 'blue')
      return(.density)
    } # gamma.density()  
  } # if(.distribution == 'gamma'){
  
  return(my.density(.data))
  
}

