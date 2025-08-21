### Remove leading 0s in electrode labels
### So goddamn frustrating
### 9 November 2022
### adam.milton.morgan@gmail.com

remove.leading.0s.from.electrode.labels <- 
  function(labels){
    # labels = c("G08","PIT02","AIT12","G20","DPMT07","AIT01","G46","DPMT40")
    library('stringr')
    
    # Convert to dataframe separating characters from digits
    labels <- data.frame('original' = labels,
                         'character' = str_extract(labels, "[aA-zZ]+"),
                         'number' = str_extract(labels, "[0-9]+"))
    
    # Remove leading 0s
    labels$number.new <- str_remove(labels$number, "^0+")
    
    # Recombine
    output <- paste0(labels$character, labels$number.new)
    
    return(output)
  }