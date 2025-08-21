### Functions for subsetting data to just good trials
### Spring 2023
### adam.milton.morgan@gmail.com

# Function to calculate circular mean (i.e., mean of phase angles)
circular.mean <- function(phase.data, na.rm = TRUE) {
  # Remove NAs
  if(na.rm){
    phase.data <- phase.data[! is.na(phase.data)]
  } # if(na.rm)
  
  # Convert phase data to Cartesian coordinates
  x <- cos(phase.data)
  y <- sin(phase.data)
  
  # Calculate the mean of the Cartesian coordinates
  mean.x <- mean(x)
  mean.y <- mean(y)
  
  # Convert the mean coordinates back to an angle
  circular.mean <- atan2(mean.y, mean.x)
  
  return(circular.mean)
} # circular.mean()

# Function to calculate circular standard deviation
circular.sd <- function(phase.data, na.rm = TRUE) {
  
  # Remove NAs
  if(na.rm){
    phase.data <- phase.data[! is.na(phase.data)]
  } # if(na.rm)
  
  # Convert phase data to Cartesian coordinates
  x <- cos(phase.data)
  y <- sin(phase.data)
  
  # Calculate the mean of the Cartesian coordinates
  mean.x <- mean(x)
  mean.y <- mean(y)
  
  # Calculate the mean resultant length
  r.bar <- sqrt(mean.x^2 + mean.y^2)
  
  # Calculate the circular variance
  circular.variance <- 1 - r.bar
  
  # Calculate the circular standard deviation
  circular.sd <- sqrt(-2 * log(r.bar))
  
  return(circular.sd)
} # circular.sd()

# Function to calculate the standardized angular deviation
standardized.angular.deviation <- function(phase.data, circular.mean, circular.sd) {
  # Calculate the difference between each phase and the circular mean
  angular.deviation <- phase.data - circular.mean
  
  # Wrap the angular deviation to the range -π to π
  angular.deviation <- atan2(sin(angular.deviation), cos(angular.deviation))
  
  # Calculate the standardized angular deviation
  standardized.deviation <- angular.deviation / circular.sd
  
  return(standardized.deviation)
} # standardized.angular.deviation()