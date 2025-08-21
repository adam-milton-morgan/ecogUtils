### Plot time series - general function
### Winter 2023 (!!!!!!)
### adam.milton.morgan@gmail.com

plot.axes <- 
  function(.y.values = NULL,
           .x.values = NULL,
           .which.y.axis = c(1,2)[1],
           .colors = rgb(.3,.3,.3),
           .y.limits = NULL,
           .y2.limits = NULL,
           .y.limits.min.at.least = NULL,
           .y.limits.max.at.least = NULL,
           .y.limits.symmetric = FALSE,
           .y2.limits.min.at.least = NULL,
           .y2.limits.max.at.least = NULL,
           .x.limits = NULL,
           .title = '',
           .y.label = 'signal',
           .x.label = 'time (ms)',
           .sampling.rate = 512,
           .y2.label = NULL,
           .error.bars = NULL,
           .error.bars.upper = NULL,
           .error.bars.lower = NULL,
           .sig.windows = NULL,
           .shuffle.dist.mean = NULL,
           .shuffle.dist.x.vals = NULL,
           .shuffle.dist.error.bars = NULL,
           .shuffle.dist.error.bars.upper = NULL,
           .shuffle.dist.error.bars.lower = NULL,
           .shuffle.dist.lty = 1,
           .shuffle.dist.lwd = 3,
           .shuffle.dist.color = rgb(.5,.5,.5,1),
           .polygons.x = NULL,
           .polygons.y = NULL,
           .polygons.color = NULL,
           show.polygons = TRUE,
           .highlight.x.axis.segments = NULL,
           .highlight.x.axis.segments.lwd = 3,
           .highlight.x.axis.segments.color = rgb(.5,.5,.5),
           .log.axis = c("", "x", "y", "xy")[1],
           .y.ticks = NULL,
           .y.tick.labels = NULL,
           .y2.ticks = NULL,
           .y2.tick.labels = NULL,
           .y.axis.color = NULL,
           .y2.axis.color = NULL,
           .x.ticks = NULL,
           .x.tick.labels = NULL,
           .theme = c('black','white')[1],
           .sig.color = ifelse(.theme == 'black', rgb(.5,.5,.5,1), rgb(.75,.75,.75,1)),
           .y.lwd = 3,
           .y.lty = 1,
           .zoom = 1,
           .margin = NULL,
           show.data = TRUE,
           show.sig.bars = TRUE,
           .ignore.empty.sig.windows = FALSE,
           show.which = NULL,
           show.x.axis = TRUE,
           .x.tick.labels.vertical = FALSE,
           show.y.axis = TRUE){
    
    
    # Packages etc.
    source(paste0(path,'/analysis/R/functions/adjust_transparency.R'))
    source(paste0(path,'/analysis/R/functions/time_convert.R'))
    library('pals')
    
    # Text sizes
    text.size.big <- 1.8
    text.size.med <- 1.6
    text.size.small <- 1.4
    
    # X values
    if(is.null(.x.values) & (! is.null(.y.values))){
      .x.values <- lapply(.y.values, function(x){1:length(x)})
    }
    
    # Convert y-values to a list if it's not already -- so you can loop thru list and plot lines
    if(! is.null(.y.values)){
      if(class(.y.values) != "list"){
        .y.values <- list(.y.values)
      }
    }
    
    # Make sure x-values are in a list
    if(class(.x.values) != "list"){
      .x.values <- list(.x.values)
    }
    # If x-values are sample labels, convert to times
    for(.x.loop in 1:length(.x.values)){
      if(class(.x.values[[.x.loop]]) == "character"){
        .x.values[[.x.loop]] <- time.convert(.x.values[[.x.loop]], "sample.labels", "times", input.sampling.rate = .sampling.rate)
      } # if(class(.x.values[[.x.loop]]) == "character"){
    } # .x.loop
    # Make sure there are as many lists of x-values as there are y-values
    if((length(.y.values) > 1) & (length(.x.values) == 1)){
      for(i in 2:length(.y.values)){
        .x.values[[i]] <- .x.values[[1]]
      }; rm(i)
    }
    
    # Make sure there are as many colors as there are y-values
    if(length(.colors) < length(.y.values)){
      .colors <- rep(.colors, length.out = length(.y.values))
    }
    
    # Make sure there are as many line width values as there are y-values
    if(length(.y.lwd) < length(.y.values)){
      .y.lwd <- rep(.y.lwd, length.out = length(.y.values))
    }
    
    # Make sure there are as many line type values as there are y-values
    if(length(.y.lty) < length(.y.values)){
      .y.lty <- rep(.y.lty, length.out = length(.y.values))
    }
    
    # Make .sig.windows a list of dataframes, if not already
    if(! is.null(.sig.windows)){
      if(class(.sig.windows) == "data.frame"){
        .sig.windows <- list(.sig.windows)
      }
      # And make .sig.color a list of colors if not already
      if(class(.sig.color) != 'list'){
        .sig.color <- as.list(.sig.color)
      }
      # If there aren't as many sig colors as sig window lists, copy the last one
      if(length(.sig.color) < length(.sig.windows)){
        for(i in (length(.sig.color)+1):length(.sig.windows)){
          .sig.color[[i]] <- .sig.color[[i-1]]
        }; rm(i)
      }
      # Remove sig window entries with no sig windows?
      if(.ignore.empty.sig.windows){
        # If no sig windows, just ignore
        if(sum(sapply(.sig.windows, nrow)) == 0){
          show.sig.bars <- FALSE
          show.sig.highlights <- FALSE
        }else{
          keep.sig.windows <- which(sapply(.sig.windows, nrow) > 0)
          .sig.windows <- .sig.windows[keep.sig.windows]
          .sig.color <- .sig.color[keep.sig.windows]
        }
      } # if(.ignore.empty.sig.windows){
    } # if(! is.null(.sig.windows)){
    
    # Input error messages:
    if(is.null(.error.bars) & is.null(.error.bars.upper) & is.null(.error.bars.lower)){
      show.error <- FALSE
    }else{
      if(! is.null(.error.bars)){
        if((! is.null(.error.bars.upper)) | (! is.null(.error.bars.lower))){
          message("Error: provided both '.error.bars' and {'error.bars.upper' and/or 'error.bars.lower'}. Ignoring the latter.")
        }
        if(class(.error.bars) != "list"){
          .error.bars <- list(.error.bars)
        }
        if(! all.equal(length(.y.values), length(.error.bars))){
          message("Error: .error.bars not provided for each time series .y.values.")
        }
        
        # Convert to upper and lower
        .error.bars.upper <- .error.bars
        .error.bars.lower <- .error.bars
      }else{
        if(class(.error.bars.upper) != "list"){
          .error.bars.upper <- list(.error.bars.upper)
        }
        if(class(.error.bars.lower) != "list"){
          .error.bars.lower <- list(.error.bars.lower)
        }
        if(mean(unlist(.error.bars.lower) < 0) > .5){
          message("Lower error bars given as negative values but should be magnitudes. Switching to positive.")
          .error.bars.lower <- lapply(.error.bars.lower, abs)
        }
      }
    }
    
    # Assign values to left or right y-axis
    if(length(.which.y.axis) == 1){
      .which.y.axis <- rep(.which.y.axis, length.out = length(.y.values))
    }
    .which.y1 <- which(.which.y.axis == 1)
    .which.y2 <- which(.which.y.axis == 2)
    
    ## Set (left) y-axis details:
    # Calculate Y1-limits: absolute max and min across plots within patient
    if(is.null(.y.limits)){
      if(is.null(.error.bars.lower) & is.null(.error.bars.upper)){
        all.y.vals <- unlist(.y.values[.which.y1])
      }else{
        all.y.vals <- c(unlist(.y.values[.which.y1]) + unlist(.error.bars.upper[.which.y1]),
                        unlist(.y.values[.which.y1]) - unlist(.error.bars.lower[.which.y1]))
      }
      
      # If a single min or max is specified:
      if(! is.null(.y.limits.min.at.least)){
        all.y.vals <- c(all.y.vals, .y.limits.min.at.least)
      }
      if(! is.null(.y.limits.max.at.least)){
        all.y.vals <- c(all.y.vals, .y.limits.max.at.least)
      }
      
      # If .shuffle.dist.error.bars are given
      if(! is.null(.shuffle.dist.error.bars)){
        if(! (is.null(.shuffle.dist.error.bars.upper) & is.null(.shuffle.dist.error.bars.lower))){
          message("WARNING: Only give .shuffle.dist.error.bars or {.shuffle.dist.error.bars.upper and/or ....lower}. Ignoring upper and lower.")
        }
        .shuffle.dist.error.bars.upper <- .shuffle.dist.error.bars
        .shuffle.dist.error.bars.lower <- .shuffle.dist.error.bars
        .shuffle.dist.error.bars <- 'stored in .shuffle.dist.error.bars.upper and .shuffle.dist.error.bars.lower (i.e., not null!)'
      }
      
      # If most .shuffle.dist.error.bars.lower are negative (should be given as magnitudes, positive values)
      if(! is.null(.shuffle.dist.error.bars.lower)){
        if(mean(.shuffle.dist.error.bars.lower < 0) > .5){
          message("WARNING: .shuffle.dist.error.bars.lower should be given as magnitudes (i.e., positive values). Majority are negative. Multiplying by -1.")
          .shuffle.dist.error.bars.lower <- -1 * .shuffle.dist.error.bars.lower
        }
      }
      
      # If a shuffle distribution is given
      if(! is.null(.shuffle.dist.mean)){
        all.y.vals <- c(all.y.vals, .shuffle.dist.mean)
      }
      
      # If error bars, add to all.y.vals
      if(! is.null(.shuffle.dist.error.bars.lower)){
        all.y.vals <- c(all.y.vals, .shuffle.dist.mean - .shuffle.dist.error.bars.lower)
      }
      if(! is.null(.shuffle.dist.error.bars.upper)){
        all.y.vals <- c(all.y.vals, .shuffle.dist.mean + .shuffle.dist.error.bars.upper)
      }
      
      # Limits and range
      .y.limits <- c('min' = min(all.y.vals, na.rm = TRUE), 
                     'max' = max(all.y.vals, na.rm = TRUE))
      
    } # if(is.null(.y.limits)){
    
    # Make symmetrical?
    if(.y.limits.symmetric){
      .y.limits <- c('min' = -max(abs(.y.limits)),
                     'max' = max(abs(.y.limits)))
    } # if(.y.limits.symmetric){
    
    # Add space at bottom for significance bars
    .y.range <- as.numeric(.y.limits[2] - .y.limits[1])
    if(show.sig.bars & (! is.null(.sig.windows))){
      sig.line.y.space <- .y.range / 20
      .y.limits[1] <- .y.limits[1] - (length(.sig.windows) * sig.line.y.space)
      .y.range <- as.numeric(.y.limits[2] - .y.limits[1])  
    }
    
    ## Axis colors
    if(is.null(.y.axis.color)){
      .y.axis.color <- ifelse(.theme == 'black', 'white', 'black')
    }
    if(is.null(.y.axis.color)){
      .y2.axis.color <- ifelse(.theme == 'black', 'white', 'black')
    }
    
    ## Set right y-axis ("y2") details:
    if(length(.which.y2) > 0){
      # Calculate Y1-limits: absolute max and min across plots within patient
      if(is.null(.y2.limits)){
        if(is.null(.error.bars.upper) & is.null(.error.bars.lower)){
          all.y2.vals <- unlist(.y.values[.which.y2])
        }else{
          all.y2.vals <- c(unlist(.y.values[.which.y2]) + unlist(.error.bars.upper[.which.y2]),
                           unlist(.y.values[.which.y2]) - unlist(.error.bars.lower[.which.y2]))
        }
        
        # If a single min or max is specified:
        if(! is.null(.y2.limits.min.at.least)){
          all.y2.vals <- c(all.y2.vals, .y2.limits.min.at.least)
        }
        if(! is.null(.y2.limits.max.at.least)){
          all.y2.vals <- c(all.y2.vals, .y2.limits.max.at.least)
        }
        
        # Limits and range
        .y2.limits <- c('min' = min(all.y2.vals, na.rm = TRUE), 
                        'max' = max(all.y2.vals, na.rm = TRUE))
      } # if(is.null(.y2.limits)){
      
      # Add space at bottom for significance bars
      .y2.range <- as.numeric(.y2.limits[2] - .y2.limits[1])
      if(show.sig.bars & (! is.null(.sig.windows))){
        sig.line.y2.space <- .y2.range / 20
        .y2.limits[1] <- .y2.limits[1] - (length(.sig.windows) * sig.line.y2.space)
        .y2.range <- as.numeric(.y2.limits[2] - .y2.limits[1])  
      } # if(show.sig.bars & (! is.null(.sig.windows))){
    } # if there's a right y-axis ("y2")
    
    # Axis labels etc.
    title(.title,
          cex.main = text.size.big * .zoom,
          col.main = ifelse(.theme == 'black', 'white', 'black'))
    title(ylab = .y.label,
          cex.lab = text.size.big * .zoom,
          line = 3.75 * .zoom,
          #col.lab = ifelse(.theme == 'black', 'white', 'black')
          col.lab = .y.axis.color)
    title(xlab = .x.label,
          cex.lab = text.size.big * .zoom,
          line = 3.5 * .zoom,
          col.lab = ifelse(.theme == 'black', 'white', 'black'))
    if(!is.null(.y2.label)){
      mtext(text = .y2.label,
            side = 4,
            cex = text.size.big * .zoom,
            line = 3.75 * .zoom,
            col = .y2.axis.color)
    } # if(!is.null(.y2.label)){
  

### Axes

# Set up plot area with right sizes etc.
if(show.x.axis){
  if(is.null(.x.ticks)){
    if(! is.null(.x.tick.labels)){message("Disregarding '.x.tick.labels' because '.x.ticks' not specified.")}
    if(! is.null(.highlight.x.axis.segments)){
      pre.lend.setting <- par("lend")
      par(xpd = TRUE, # plot outside plot area
          lend = 2) # butt line ends
      if(! "list" %in% class(.highlight.x.axis.segments)){.highlight.x.axis.segments <- list(.highlight.x.axis.segments)}
      if(length(.highlight.x.axis.segments.lwd) == 1){rep(.highlight.x.axis.segments.lwd, times = length(.highlight.x.axis.segments))}
      if(length(.highlight.x.axis.segments.color) == 1){rep(.highlight.x.axis.segments.color, times = length(.highlight.x.axis.segments))}
      for(i in 1:length(.highlight.x.axis.segments)){
        arrows(x0 = .highlight.x.axis.segments[[i]][1],
               x1 = .highlight.x.axis.segments[[i]][2],
               y0 = par('usr')[3], # y-value for x-axis
               code = 3,
               lwd = .highlight.x.axis.segments.lwd[i] * .zoom,
               col = .highlight.x.axis.segments.color[i],
               length = .05 * .zoom,
               angle = 90)
      }; rm(i)
      par(xpd = FALSE,
          lend = pre.lend.setting) # reset
    } # if(! is.na(.highlight.x.axis.segments)){
    axis(side = 1,
         #at = x.ticks,
         #labels = x.ticks,
         las = ifelse(.x.tick.labels.vertical, 2, 1),
         tck = -.025 * .zoom, # length of tick
         padj = ifelse(.x.tick.labels.vertical, .3, .6) * .zoom, # distance between tick and label
         lwd = 1.5 * .zoom,
         lwd.ticks = 1.5 * .zoom,
         cex.axis = text.size.med * .zoom,
         col = ifelse(.theme == 'black', 'white', 'black'),
         col.axis = ifelse(.theme == 'black', 'white', 'black'))  
  }else{ # if(is.null(.x.ticks)){
    if(is.null(.x.tick.labels)){
      .x.tick.labels <- .x.ticks
    }
    if(! is.null(.highlight.x.axis.segments)){
      par(xpd = TRUE, # plot outside plot area
          lend = 2) # butt line ends
      if(! "list" %in% class(.highlight.x.axis.segments)){.highlight.x.axis.segments <- list(.highlight.x.axis.segments)}
      if(length(.highlight.x.axis.segments.lwd) == 1){rep(.highlight.x.axis.segments.lwd, times = length(.highlight.x.axis.segments))}
      if(length(.highlight.x.axis.segments.color) == 1){rep(.highlight.x.axis.segments.color, times = length(.highlight.x.axis.segments))}
      for(i in 1:length(.highlight.x.axis.segments)){
        arrows(x0 = .highlight.x.axis.segments[[i]][1],
               x1 = .highlight.x.axis.segments[[i]][2],
               y0 = par('usr')[3], # y-value for x-axis
               code = 3,
               lwd = .highlight.x.axis.segments.lwd[i] * .zoom,
               col = .highlight.x.axis.segments.color[i],
               length = .05 * .zoom,
               angle = 90)
      }; rm(i)
      par(xpd = FALSE,
          lend = 0) # reset
    } # if(! is.na(.highlight.x.axis.segments)){
    axis(side = 1,
         at = .x.ticks,
         labels = .x.tick.labels,
         las = ifelse(.x.tick.labels.vertical, 2, 1),
         tck = -.025 * .zoom, # length of tick
         padj = ifelse(.x.tick.labels.vertical, .3, .6) * .zoom, # distance between tick and label
         lwd = 1.5 * .zoom,
         lwd.ticks = 1.5 * .zoom,
         cex.axis = text.size.med * .zoom,
         col = ifelse(.theme == 'black', 'white', 'black'),
         col.axis = ifelse(.theme == 'black', 'white', 'black'))
  } # if(is.null(.x.ticks)){}else{
} # if(show.x.axis){
if(show.y.axis){
  if(is.null(.y.ticks)){
    if(! is.null(.y.tick.labels)){message("Disregarding '.y.tick.labels' because '.y.ticks' not specified.")}
    axis(side = 2,
         #at = seq(from = .16, to = .26, by = .02),
         las = 0,
         tck = -.025 * .zoom, # length of tick
         padj = -.45 * .zoom, # distance between tick and label
         lwd = 1.5 * .zoom,
         lwd.ticks = 1.5 * .zoom,
         cex.axis = text.size.med * .zoom,
         col = .y.axis.color,
         col.axis = .y.axis.color)  
  }else{ # if(is.null(.y.ticks)){
    if(is.null(.y.tick.labels)){
      .y.tick.labels <- .y.ticks
    }
    axis(side = 2,
         at = .y.ticks,
         labels = .y.tick.labels,
         las = 0,
         tck = -.025 * .zoom, # length of tick
         padj = -.45 * .zoom, # distance between tick and label
         lwd = 1.5 * .zoom,
         lwd.ticks = 1.5 * .zoom,
         cex.axis = text.size.med * .zoom,
         col = .y.axis.color,
         col.axis = .y.axis.color)
  } # if(is.null(.y.ticks)){}else{
} # if(show.y.axis){
## Secondary (right) y-axies ("y2")
if(length(.which.y2) > 0){
  # Set up plot area with right sizes etc.
  par(new = TRUE)
  plot(y = NULL,
       x = NULL,
       ylim = .y2.limits,
       xlim = .x.limits,
       bty = 'n',
       xlab = '',
       ylab = '',
       xaxt = 'n',
       yaxt = 'n',
       log = .log.axis)
  if(is.null(.y2.ticks)){
    if(! is.null(.y2.tick.labels)){message("Disregarding '.y2.tick.labels' because '.y2.ticks' not specified.")}
    axis(side = 4,
         #at = seq(from = .16, to = .26, by = .02),
         las = 0,
         tck = -.025 * .zoom, # length of tick
         padj = .5 * .zoom, # distance between tick and label
         lwd = 1.5 * .zoom,
         lwd.ticks = 1.5 * .zoom,
         cex.axis = text.size.med * .zoom,
         col = .y2.axis.color,
         col.axis = .y2.axis.color)  
  }else{
    if(is.null(.y2.tick.labels)){
      .y2.tick.labels <- .y2.ticks
    }
    axis(side = 4,
         at = .y2.ticks,
         labels = .y2.tick.labels,
         las = 0,
         tck = -.025 * .zoom, # length of tick
         padj = .5 * .zoom, # distance between tick and label
         lwd = 1.5 * .zoom,
         lwd.ticks = 1.5 * .zoom,
         cex.axis = text.size.med * .zoom,
         col = .y2.axis.color,
         col.axis = .y2.axis.color)
  }
} # if any y2 vals

}


