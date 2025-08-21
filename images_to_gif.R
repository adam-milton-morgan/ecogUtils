### Turn images in a directory into a gif
### Spring 2022
### adam.milton.morgan@gmail.com
### Based on code by VP Nagraj: https://www.nagraj.net/

images.to.gif <- function(image.read.dir,
                          gif.save.dir,
                          view = FALSE,
                          frames.per.second = 4){
  
  library(magick)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  ## Create save directory
  dir.create(gif.save.dir, recursive = TRUE, showWarnings = FALSE)
  
  ## Read files
  images <- list.files(image.read.dir, full.names = TRUE)
  image.list <- lapply(images, image_read)
  
  ## Join images
  images.joined <- image_join(image.list)
  
  ## Animate
  images.animated <- image_animate(images.joined, 
                                   fps = frames.per.second)
  
  ## View animated image
  if(view){images.animated}
  
  ## Save
  image_write(image = images.animated,
              path = paste0(gif.save.dir, patient, "_subtraction.gif"))
  
  ## Update
  message("Gif created and saved to disk!")
}

