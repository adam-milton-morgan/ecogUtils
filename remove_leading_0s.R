### Functions for subsetting data to just good trials
### Spring 2023
### adam.milton.morgan@gmail.com

# Remove any leading 0s from elec labels (ugh)
remove.leading.0s <- function(strings) {
  # Initialize an empty vector to store the results
  result <- vector(length = length(strings), mode = "character")
  
  # Loop through each string in the vector
  for (i in 1:length(strings)) {
    # Extract the string
    string <- strings[i]
    
    # Split the string into letters and numbers
    letters <- gsub("[0-9]*$", "", string)
    numbers <- gsub("^.*?([0-9]+)$", "\\1", string)
    
    # Remove leading zeros from the numbers
    numbers <- as.numeric(numbers)
    numbers <- as.character(numbers)
    
    # Combine the letters and numbers back into a string
    result[i] <- paste0(letters, numbers)
  }
  
  # Return the vector of results
  return(result)
}