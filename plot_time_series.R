### Plot time series - general function
### Winter 2023 (!!!!!!)
### adam.milton.morgan@gmail.com

plot.time.series <- 
  function(.y.values,
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
           .time.lock = c('locked_to_production_onset','locked_to_stimulus_onset')[1],
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
           .background = NULL,
           .sig.color = ifelse(.theme == 'black', rgb(.5,.5,.5,1), rgb(.75,.75,.75,1)),
           .y.lwd = 3,
           .y.lty = 1,
           .zoom = 1,
           .margin = NULL,
           show.data = TRUE,
           show.sig.bars = TRUE,
           .ignore.empty.sig.windows = FALSE,
           .center.sig.bars.vertically = TRUE,
           show.sig.highlights = FALSE,
           .sig.highlights.lwd = NULL,
           .sig.highlights.in.front = FALSE,
           show.error = TRUE,
           show.which = NULL,
           show.t0 = TRUE,
           show.x.axis = TRUE,
           .x.tick.labels.vertical = FALSE,
           show.y.axis = TRUE,
           show.y2.axis = TRUE,
           .horizontal.line.at = NULL,
           .vertical.line.at = NULL,
           add = FALSE,
           .path = path){
    
    
    ### DOCUMENTATION
    ### .polygons.y, .polygons.x, .polygons.color: Plots polygons in background of plot. To plot a rectangle (e.g., to highlight a particular time window), only need to specify the beginning and end x-values (.polygons.x). .polygons.color must be a vector, not a list; the others can be either a single vector of points or a list of points for multiple polygons.
    
    
    
    # Packages etc.
    source(paste0(.path,'/analysis/R/functions/adjust_transparency.R'))
    source(paste0(.path,'/analysis/R/functions/time_convert.R'))
    source(paste0(.path,'/analysis/R/functions/smoothing.R'))
    library('pals')
    
    
    # Text sizes
    text.size.big <- 1.8
    text.size.med <- 1.6
    text.size.small <- 1.4
    
    # X values
    if(is.null(.x.values)){
      .x.values <- lapply(.y.values, function(x){1:length(x)})
    }
    
    # X-limits and tick marks
    if(is.null(.x.limits)){
      if(.time.lock == 'locked_to_production_onset'){
        .x.limits <- c(-1000, 500)
        if(! is.null(.x.ticks)){message("Disregarding '.x.ticks' input because '.time.lock' was specified.")}
        .x.ticks <- c(-1000, -500, 0, 500)
      } # if(.time.lock == 'locked_to_production_onset'){
      if(.time.lock == 'locked_to_stimulus_onset'){
        .x.limits <- c(-500, 1000)
        if(! is.null(.x.ticks)){message("Disregarding '.x.ticks' input because '.time.lock' was specified.")}
        .x.ticks <- c(-500, 0, 500, 1000)
      } # if(.time.lock == 'locked_to_stimulus_onset'){
    } # if(is.null(.x.limits)){
    
    # Convert y-values to a list if it's not already -- so you can loop thru list and plot lines
    if(class(.y.values) != "list"){
      .y.values <- list(.y.values)
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
    
    ## Set shuffle distribution error bars, if any
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
    
    # Background color
    if(is.null(.background)){
      .background <- .theme
    }
    
    # Plot area
    if(is.null(.margin)){
      par(mar = c(5, 5.5, ifelse(.title == '', 1, 5), ifelse(length(.which.y2) > 0, 5.5, 1)) * .zoom,
          bg = .background)
    }else{
      par(mar = .margin * .zoom,
          bg = .background)  
    }
    
    # Set up plot
    if(!add){
      plot(y = NULL,
           x = NULL,
           ylim = .y.limits,
           xlim = .x.limits,
           bty = 'n',
           xlab = '',
           ylab = '',
           xaxt = 'n',
           yaxt = 'n',
           log = .log.axis)
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
        ## BEGIN: Produce the secondary y-axis label flipped 180 degrees, but positioning is bad
        # line.height.in.inches <- par('cin')[2] * par('cex') * par('lheight') 
        # inch.in.user.coordinates <- diff(grconvertX(0:1, 'inches', 'user'))
        # line = 3.75 * .zoom
        # right.margin.position.user <- par('usr')[2] + (line * inch.in.user.coordinates * line.height.in.inches)
        # text(x = right.margin.position.user,
        #    y = mean(par("usr")[c(3,4)]),
        #    labels = .y2.label,
        #    cex = text.size.big * .zoom,
        #    xpd = TRUE,
        #    col = .y2.axis.color,
        #    #col = ifelse(.theme == 'black', 'white', 'black'),
        #    srt = 270)
        ## END: Produce the secondary y-axis label flipped 180 degrees, but positioning is bad
      } # if(!is.null(.y2.label)){
    } # if(!add)
    
    ### Background polygons (e.g., shading regions of plot to highlight time window)
    if(show.polygons){
      if(is.null(.polygons.x) & ! is.null(.polygons.y)){message('Error: Must supply at least .polygons.x values to plot polygons.')}
      if(! is.null(.polygons.x)){
        # Make sure .polygons.x is a list of vectors
        if(class(.polygons.x) != 'list'){.polygons.x <- list(.polygons.x)}
        # If just beginning and end vals, set to repeat
        for(i in 1:length(.polygons.x)){
          # If just 2 vals, set up for rectangle coords
          if(length(.polygons.x[[i]]) == 2){.polygons.x[[i]] <- .polygons.x[[i]][c(1,2,2,1)]}
          # If 4 vals (for rectangle), make sure in right order
          if((length(.polygons.x[[i]]) == 4) & (length(unique(.polygons.x[[i]])) == 2)){
            .polygons.x[[i]] <- unique(.polygons.x[[i]])[c(1,2,2,1)]
          }
        }; rm(i)
        
        # Set up y-values
        if(is.null(.polygons.y)){
          # Set y-vals to fill plot area if not specified
          .polygons.y <- list(c(.y.limits[1] - abs(.y.limits[1]),
                                .y.limits[2] + abs(.y.limits[2])))
        }else{ # if(is.null(.polygons.y)){
          if(class(.polygons.y) != 'list'){
            .polygons.y <- list(.polygons.y)
          } # if(class(.polygons.x) != 'list'){
        } # if(is.null(.polygons.y)){
        # Set number of list elements in .polygons.x to length of .polygons.y
        if(length(.polygons.y) < length(.polygons.x)){
          for(i in 1:length(.polygons.x)){
            .polygons.y[[i]] <- .polygons.y[[1]]
          }; rm(i)
        } # if(length(.polygons.y) < length(.polygons.x)){
        # If just top and bottom vals, set up for rectangle coords
        for(i in 1:length(.polygons.y)){
          # If just 2 values, set up for rectangle coords
          if(length(.polygons.y[[i]]) == 2){.polygons.y[[i]] <- .polygons.y[[i]][c(1,1,2,2)]}  
          # IF 4 vals, make sure in right order
          if((length(.polygons.y[[i]]) == 4) & (length(unique(.polygons.y[[i]])) == 2)){
            .polygons.y[[i]] <- unique(.polygons.y[[i]])[c(1,1,2,2)]
          }
        } # for(i in 1:length(.polygons.y)){
        
        ## Set up colors
        # Set to grey if not specified
        if(is.null(.polygons.color)){
          .polygons.color <- ifelse(.theme == 'black', rgb(.3,.3,.3), rgb(.85,.85,.85))
        }
        # Make sure as many colors as polygons
        if(length(.polygons.color) < length(.polygons.x)){
          .polygons.color <- rep(.polygons.color, times = length(.polygons.x))
        }
        
        ## Plot polygons
        for(i in 1:length(.polygons.x)){
          polygon(x = .polygons.x[[i]],
                  y = .polygons.y[[i]],
                  border = NA,
                  col = .polygons.color[i])  
        }; rm(i)
      } # if(! is.null(.polygons.x)){
    } # if(show.polygons){
    
    # Plot t=0 line
    if(show.t0){
      # abline(v = 0,
      #        col = ifelse(.theme == 'black', 'white', 'black'),
      #        lwd = 1.5 * .zoom,
      #        lty = 2)  
      segments(x0 = 0,
               y0 = .y.limits[1] - (.y.range / 5),
               y1 = .y.limits[2],
               col = ifelse(.theme == 'black', 'white', 'black'),
               lwd = 1.5 * .zoom,
               lty = 2)  
    } # if(show.t0){
    
    # Plot horizontal lines
    if(! is.null(.horizontal.line.at)){
      for(i in 1:length(.horizontal.line.at)){
        abline(h = .horizontal.line.at[i],
               col = ifelse(.theme == 'black', 'white', 'black'),
               lwd = 1.5 * .zoom,
               lty = 2) 
      }; rm(i)
    } # if(! is.null(.horizontal.line.at)){
    
    # Plot vertical lines
    if(! is.null(.vertical.line.at)){
      for(i in 1:length(.vertical.line.at)){
        abline(v = .vertical.line.at[i],
               col = ifelse(.theme == 'black', 'white', 'black'),
               lwd = 1.5 * .zoom,
               lty = 2) 
      }; rm(i)
    } # if(! is.null(.horizontal.line.at)){
    
    # Plot noise distribution
    if(! is.null(.shuffle.dist.mean)){
      if(is.null(.shuffle.dist.x.vals)){.shuffle.dist.x.vals <- .x.values[[1]]}
      # Remove NAs
      .shuffle.dist.na.indices <- which(is.na(.shuffle.dist.mean))
      if(length(.shuffle.dist.na.indices)){ # if any NAs
        .shuffle.dist.mean <- .shuffle.dist.mean[-.shuffle.dist.na.indices]
        .shuffle.dist.error.bars.upper <- .shuffle.dist.error.bars.upper[-.shuffle.dist.na.indices]
        .shuffle.dist.error.bars.lower <- .shuffle.dist.error.bars.lower[-.shuffle.dist.na.indices]
        .shuffle.dist.x.vals <- .shuffle.dist.x.vals[-.shuffle.dist.na.indices]
      } # if any NAs
      # Plot error bars
      polygon(x = c(.shuffle.dist.x.vals, rev(.shuffle.dist.x.vals)),
              y = c(.shuffle.dist.mean - .shuffle.dist.error.bars.lower, 
                    rev(.shuffle.dist.mean + .shuffle.dist.error.bars.upper)),
              border = FALSE,
              col = adjust.transparency(.shuffle.dist.color,
                                        alpha = .6))
      # Plot mean
      lines(y = .shuffle.dist.mean,
            x = .shuffle.dist.x.vals,
            col = .shuffle.dist.color,
            lty = .shuffle.dist.lty,
            lwd = .shuffle.dist.lwd * .zoom)  
    } # if(! is.null(.shuffle.dist.mean)){
    
    # Plot data
    if(show.data){
      # Which time series in list of input time series to show? If not specified, show all
      if(is.null(show.which)){
        show.which <- 1:length(.y.values)
      } # if(is.null(show.which)){
      # Significant windows
      if(! is.null(.sig.windows)){
        if(show.sig.bars | show.sig.highlights){ 
          # Which dataframes of sig windows in the input list have data?
          sig.line.indices <- which(sapply(.sig.windows, nrow) > 0)
          # Loop thru those and plot them
          for(sig.line.loop in sig.line.indices){ 
            # sig.line.loop = 1
            for(window.loop in 1:nrow(.sig.windows[[sig.line.loop]])){
              # window.loop = 1
              .x0 <- .sig.windows[[sig.line.loop]]$start.time[window.loop]
              .x1 <- .sig.windows[[sig.line.loop]]$end.time[window.loop]
              .current.sig.color <- colorRampPalette(.sig.color[[sig.line.loop]])(length(.x0))
              if(show.sig.bars){
                # Get x and y coordinates
                .x0 <- .x0:.x1
                if(.center.sig.bars.vertically & 
                   (length(.sig.windows) > 1) &
                   (any(sapply(.sig.windows, nrow) == 0))){
                  .bottom <- .y.limits[1]
                  .top <- .bottom + (sig.line.y.space * (length(.sig.windows) - 1))
                  .center <- mean(.bottom, .top)
                  .space <- .top - .bottom
                  # Keep the spacing between bars the same as if all were shown
                  .n.spaces.intended <- length(.sig.windows) - 1
                  .space.bt.bars.intended <- .space / .n.spaces.intended
                  .n.actual.spaces <- length(sig.line.indices) - 1
                  .extra.space <- .space.bt.bars.intended * (.n.spaces.intended - .n.actual.spaces)
                  .y.positions <- seq(.bottom + (.extra.space / 2),
                                      .top - (.extra.space / 2),
                                      length.out = length(sig.line.indices))
                  # .new.bottom <- .center - (.n.actual.spaces * .space.bt.bars.intended / 2)
                  # .new.top <- .center + (.n.actual.spaces * .space.bt.bars.intended / 2)
                  # .y.positions <- seq(.new.bottom, .new.top, length.out = length(sig.line.indices))
                  .y0 <- .y.positions[which(sig.line.indices == sig.line.loop)]
                }else{
                  .y0 <- .y.limits[1] + (sig.line.y.space * (sig.line.loop - 1))  
                }
                # Plot significance bars at bottom
                segments(x0 = .x0[-length(.x0)],
                         x1 = .x0[length(.x0)],
                         y0 = rep(.y0, times = length(.x0)-1),
                         col = .current.sig.color,
                         lwd = 3.5 * .zoom)
              } # if(show.sig.bars){
              if(show.sig.highlights){
                if(is.null(.sig.highlights.lwd)){.sig.highlights.lwd <- .y.lwd[sig.line.loop] * 2 * .zoom}
                # Plot significance as highlights behind time series
                lines(x = .x.values[[sig.line.loop]][which((.x.values[[sig.line.loop]] >= .x0) & 
                                                             (.x.values[[sig.line.loop]] <= .x1))],
                      y = .y.values[[sig.line.loop]][which((.x.values[[sig.line.loop]] >= .x0) & 
                                                             (.x.values[[sig.line.loop]] <= .x1))],
                      lwd = .sig.highlights.lwd,
                      col = .current.sig.color)
              } # if(show.sig.highlights){
            }; rm(window.loop)
          }; rm(sig.line.loop) 
        } # if(show.sig & ...
      } # if(! is.null(.sig.windows)){
      if(show.error){
        for(i in show.which){
          # Set up plot area
          par(new = TRUE)
          plot(y = NULL,
               x = NULL,
               ylim = list(.y.limits, .y2.limits)[[.which.y.axis[i]]],
               xlim = .x.limits,
               bty = 'n',
               xlab = '',
               ylab = '',
               xaxt = 'n',
               yaxt = 'n',
               log = .log.axis)
          # Plot error bars
          polygon(x = c(.x.values[[i]], rev(.x.values[[i]])),
                  y = c(.y.values[[i]] - .error.bars.lower[[i]], 
                        rev(.y.values[[i]] + .error.bars.upper[[i]])),
                  border = FALSE,
                  col = adjust.transparency(.colors[i],
                                            alpha = .6))
        }; rm(i)
      } # if(show.error){
      
      # Plot data
      for(i in show.which){
        par(new = TRUE)
        plot(y = .y.values[[i]],
             x = .x.values[[i]],
             col = .colors[i],
             type = 'l',
             lty = .y.lty[i],
             lwd = .y.lwd[i] * .zoom,
             ylim = list(.y.limits, .y2.limits)[[.which.y.axis[i]]],
             xlim = .x.limits,
             bty = 'n',
             xlab = '',
             ylab = '',
             xaxt = 'n',
             yaxt = 'n',
             log = .log.axis)  
      }; rm(i)
      
      
      # Significant highlights on top of data
      if(! is.null(.sig.windows)){
        if(show.sig.highlights & .sig.highlights.in.front){ 
          # Which dataframes of sig windows in the input list have data?
          sig.line.indices <- which(sapply(.sig.windows, nrow) > 0)
          # Loop thru those and plot them
          for(sig.line.loop in sig.line.indices){ 
            # sig.line.loop = 1
            for(window.loop in 1:nrow(.sig.windows[[sig.line.loop]])){
              # window.loop = 1
              .x0 <- .sig.windows[[sig.line.loop]]$start.time[window.loop]
              .x1 <- .sig.windows[[sig.line.loop]]$end.time[window.loop]
              .current.sig.color <- colorRampPalette(.sig.color[[sig.line.loop]])(length(.x0))
                if(is.null(.sig.highlights.lwd)){.sig.highlights.lwd <- .y.lwd[sig.line.loop] * 2 * .zoom}
                  # Plot significance as highlights behind time series
                  lines(x = .x.values[[sig.line.loop]][which((.x.values[[sig.line.loop]] >= .x0) & 
                                                               (.x.values[[sig.line.loop]] <= .x1))],
                        y = .y.values[[sig.line.loop]][which((.x.values[[sig.line.loop]] >= .x0) & 
                                                               (.x.values[[sig.line.loop]] <= .x1))],
                        lwd = .sig.highlights.lwd,
                        col = .current.sig.color)
            }; rm(window.loop)
          }; rm(sig.line.loop) 
        } # if(show.sig & ...
      } # if(! is.null(.sig.windows)){
      
    } # if(show.data){
    
    ### Axes
    if(! add){
      # Set up plot area with right sizes etc.
      par(new = TRUE)
      plot(y = NULL,
           x = NULL,
           ylim = .y.limits,
           xlim = .x.limits,
           bty = 'n',
           xlab = '',
           ylab = '',
           xaxt = 'n',
           yaxt = 'n',
           log = .log.axis)
      if(show.x.axis){
        if(is.null(.x.ticks)){
          if(! is.null(.x.tick.labels)){message("Disregarding '.x.tick.labels' because '.x.ticks' not specified.")}
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
      if(show.y2.axis){
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
               line = .75 * .zoom,
               tck = -.025 * .zoom, # length of tick
               padj = .55 * .zoom, # distance between tick and label
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
               line = .75 * .zoom,
               tck = -.025 * .zoom, # length of tick
               padj = .55 * .zoom, # distance between tick and label
               lwd = 1.5 * .zoom,
               lwd.ticks = 1.5 * .zoom,
               cex.axis = text.size.med * .zoom,
               col = .y2.axis.color,
               col.axis = .y2.axis.color)
        }
      } # if any y2 vals
      } # if(.show.y2.axis){
    } # if(!add){
  }


