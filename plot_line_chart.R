### Plot time series - general function
### Winter 2023 (!!!!!!)
### adam.milton.morgan@gmail.com

plot.line.chart <- 
  function(.y.values,
           .x.values = NULL,
           .which.y.axis = c(1,2)[1],
           .colors = NULL,
           .y.limits = NULL,
           .y2.limits = NULL,
           .y.limits.min.at.least = NULL,
           .y.limits.max.at.least = NULL,
           .y2.limits.min.at.least = NULL,
           .y2.limits.max.at.least = NULL,
           .x.limits = NULL,
           .x.left.pad = 0,
           .title = '',
           .x.label = '',
           .y.label = '',
           .y2.label = NULL,
           .error.bars = NULL,
           .error.bars.upper = NULL,
           .error.bars.lower = NULL,
           .error.bar.lower.limit = NA, # for bounded variables like proportions
           .error.bar.upper.limit = NA, # for bounded variables like proportions
           .jitter.x = FALSE,
           .jitter.x.range = .2,
           .p.values = NULL,
           .sig.key = c('.' = .1, '*' = .05, '**' = .01, '***' = .001),
           .shuffle.dist.mean = NULL,
           .shuffle.dist.x.vals = NULL,
           .shuffle.dist.error.bars = NULL,
           .shuffle.dist.lty = 1,
           .shuffle.dist.lwd = 3,
           .shuffle.dist.color = rgb(.5,.5,.5,1),
           .polygons.x = NULL,
           .polygons.y = NULL,
           .polygons.color = NULL,
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
           show.sig.points = TRUE,
           show.legend = TRUE,
           .sig.points.location = c("top","bottom","above.val","below.val")[1],
           .ignore.empty.p.values.for.vertical.bar.spacing = FALSE,
           .center.sig.bars.vertically = TRUE,
           show.error = TRUE,
           show.which = NULL,
           show.t0 = FALSE,
           show.x.axis = TRUE,
           show.y.axis = TRUE,
           .horizontal.line.at = NA,
           .vertical.line.at = NA,
           add = FALSE){
    
    
    ### DOCUMENTATION
    ### .polygons.y, .polygons.x, .polygons.color: Plots polygons in background of plot. To plot a rectangle (e.g., to highlight a particular time window), only need to specify the beginning and end x-values (.polygons.x). .polygons.color must be a vector, not a list; the others can be either a single vector of points or a list of points for multiple polygons.
    
    
    
    # Packages etc.
    source(paste0(path,'/analysis/R/functions/add_text_line_multiple_colors.R'))
    source(paste0(path,'/analysis/R/functions/adjust_transparency.R'))
    source(paste0(path,'/analysis/R/functions/time_convert.R'))
    source(paste0(path,'/analysis/R/functions/smoothing.R'))
    library('pals')
    
    
    # Text sizes
    text.size.big <- 1.8
    text.size.med <- 1.6
    text.size.small <- 1.4
    
    
    # Convert y-values to a list if it's not already -- so you can loop thru list and plot lines
    if("matrix" %in% class(.y.values)){.y.values <- data.frame(.y.values)}
    if(class(.y.values) == "data.frame"){
      if(is.null(.x.tick.labels)){.x.tick.labels <- rownames(.y.values)}
      .y.values <- as.list(.y.values)
    }
    if(class(.y.values) != "list"){
      .y.values <- list(.y.values)
    }
    
    # Make sure x-values are in a list
    if(is.null(.x.values)){.x.values <- list(1:length(.y.values[[1]]))}
    if(class(.x.values) != "list"){
      .x.values <- list(.x.values)
    }
    # Make sure there are as many lists of x-values as there are y-values
    if((length(.y.values) > 1) & (length(.x.values) == 1)){
      for(i in 2:length(.y.values)){
        .x.values[[i]] <- .x.values[[1]]
      }; rm(i)
    }
    # Add jitter
    if(.jitter.x){
      .jitters <- .jitter.x.range / (length(.x.values) - 1)
      for(i in 1:length(.x.values)){
        .x.values[[i]] <- .x.values[[i]] - (.jitter.x.range / 2) + (.jitters * (i - 1))
      }; rm(i)
    }
    
    # X-limits and tick marks
    if(is.null(.x.limits)){
      .x.limits <- range(unlist(.x.values))
      .x.limits[1] <- .x.limits[1] - .x.left.pad # padding
    } # if(is.null(.x.limits)){
    
    # Make sure there are as many colors as there are y-values
    if(is.null(.colors)){.colors <- cubicl(length(.y.values))}
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
    
    # Make sure .sig.symbols are sorted
    .sig.thresholds <- rev(sort(.sig.key))
    .sig.symbols <- names(.sig.thresholds)
    
    # Make .p.values a list of vectors of indices of sig vals, if not already
    if(! is.null(.p.values)){
      if("matrix" %in% class(.p.values)){.p.values <- data.frame(.p.values)}
      if(class(.p.values) == "data.frame"){
        .p.values <- as.list(.p.values)
      }
      if(class(.p.values) %in% c("numeric","character")){ # if .p.values isn't a list
        .p.values <- list(.p.values)
      } # if .p.values isn't a list
      
      # If not as many entries as x.vals, add empty list entries
      if(length(.p.values) < length(.y.values)){
        message('Not as many vectors of p-values as y-values. Adding vectors of non-significant ps.')
        for(.y.entry.loop in (1 + length(.p.values)):length(.y.values)){
          .p.values[[.y.entry.loop]] <- rep(.99, times = length(.y.values[[.y.entry.loop]]))
        }; rm(.y.entry.loop)
      } # if(length(.p.values) < length(.y.values)){
      
      # How many lines have any significant points?
      n.lines.with.sig.values <- length(which(sapply(.p.values, function(x){any(x < max(.sig.thresholds))})))
      
      # Convert character vectors (names of sig vals) to corresponding indices
      # for(sig.label.loop in 1:length(.p.values)){
      #   if(class(.p.values[[sig.label.loop]]) == "character"){ # if .p.values is a vector of labels
      #     for(sig.value.loop in 1:length(.p.values[[sig.label.loop]])){
      #       if(! .p.values[[sig.label.loop]][[sig.value.loop]] %in% names(.y.values)){ # If .p.values labels don't correspond to .y.labels
      #         message("Warning! '.p.values' is a character vector, but entries are not labels of '.y.values'.")
      #       }else{ # If .p.values labels don't correspond to .y.labels {}else{
      #         .p.values[[sig.label.loop]][[sig.value.loop]] <-
      #           which(names(.y.values) == .p.values[[sig.label.loop]][[sig.value.loop]])
      #       } # If .p.values labels don't correspond to .y.labels {}else{} 
      #     }; rm(sig.value.loop)
      #     .p.values[[sig.label.loop]] <- as.numeric(.p.values[[sig.label.loop]])
      #   } # # if .p.values is a vector of labels
      # }; rm(sig.label.loop)
    } # if .p.values isn't null
    
    # Input error messages:
    if(is.null(.error.bars) & is.null(.error.bars.lower) & is.null(.error.bars.upper)){
      show.error <- FALSE
    }else{
      if(is.null(.error.bars)){
        if("matrix" %in% class(.error.bars.upper)){.error.bars.upper <- data.frame(.error.bars.upper)}
        if("matrix" %in% class(.error.bars.lower)){.error.bars.lower <- data.frame(.error.bars.lower)}
        
        if((class(.error.bars.upper) == "data.frame") & (class(.error.bars.lower) == "data.frame")){
          .error.bars.upper <- as.list(.error.bars.upper)
          .error.bars.lower <- as.list(.error.bars.lower)
        }
        if(! all.equal(length(.y.values), length(.error.bars.lower), length(.error.bars.upper))){
          message("Error: .error.bars not provided for each time series .y.values.")
        }
      }else{
        if("matrix" %in% class(.error.bars)){.error.bars <- data.frame(.error.bars)}
        if(class(.error.bars) == "data.frame"){
          .error.bars <- as.list(error.bars)
        }
        if(! all.equal(length(.y.values), length(.error.bars))){
          message("Error: .error.bars not provided for each time series .y.values.")
        }else{
          .error.bars.upper <- .error.bars
          .error.bars.lower <- .error.bars
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
      all.y.vals <- unlist(.y.values[.which.y1])
      if(! is.null(.error.bars)){
        all.y.vals <- c(unlist(.y.values[.which.y1]) + unlist(.error.bars[.which.y1]),
                        unlist(.y.values[.which.y1]) - unlist(.error.bars[.which.y1]))
      }
      if(! is.null(.error.bars.upper)){
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
      if((! is.null(.shuffle.dist.mean)) & (! is.null(.shuffle.dist.error.bars))){
        all.y.vals <- c(all.y.vals,
                        .shuffle.dist.mean - .shuffle.dist.error.bars, 
                        .shuffle.dist.mean + .shuffle.dist.error.bars)
      }
      
      # Limits and range
      .y.limits <- c('min' = min(all.y.vals, na.rm = TRUE), 
                     'max' = max(all.y.vals, na.rm = TRUE))
    } # if(is.null(.y.limits)){
    
    # Add space at bottom for significance bars
    .y.range <- as.numeric(.y.limits[2] - .y.limits[1])
    if(show.sig.points & (! is.null(.p.values))){
      sig.line.y.space <- .y.range / 20
      if(.sig.points.location == "top"){
        .y.limits[2] <- .y.limits[2] + (n.lines.with.sig.values * sig.line.y.space)
        .y.range <- as.numeric(.y.limits[2] - .y.limits[1]) 
      } # if(.sig.points.location == "top"){
      if(.sig.points.location == "bottom"){
        .y.limits[1] <- .y.limits[1] - (n.lines.with.sig.values * sig.line.y.space)
        .y.range <- as.numeric(.y.limits[2] - .y.limits[1])    
      } # if(.sig.points.location == "bottom"){
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
        if(is.null(.error.bars)){
          all.y2.vals <- unlist(.y.values[.which.y2])
        }else{
          all.y2.vals <- c(unlist(.y.values[.which.y2]) + unlist(.error.bars[.which.y2]),
                           unlist(.y.values[.which.y2]) - unlist(.error.bars[.which.y2]))
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
      if(show.sig.points & (! is.null(.p.values))){
        sig.line.y2.space <- .y2.range / 20
        sig.line.y.space <- .y.range / 20
        if(.sig.points.location == "top"){
          .y2.limits[2] <- .y2.limits[2] + (n.lines.with.sig.values * sig.line.y2.space)
          .y2.range <- as.numeric(.y2.limits[2] - .y2.limits[1]) 
        } # if(.sig.points.location == "top"){
        if(.sig.points.location == "bottom"){
          .y2.limits[1] <- .y2.limits[1] - (n.lines.with.sig.values * sig.line.y2.space)
          .y2.range <- as.numeric(.y2.limits[2] - .y2.limits[1])    
        } # if(.sig.points.location == "bottom"){
      } # if(show.sig.points & (! is.null(.p.values))){
    } # if there's a right y-axis ("y2")
    
    # Background color
    if(is.null(.background)){
      .background <- .theme
    }
    
    # Plot area
    if(is.null(.margin)){
      .margin = c(5, 5.5, ifelse(.title == '', 1, 5), ifelse(length(.which.y2) > 0, 5.5, 3))
    }
    par(mar = .margin * .zoom,
        bg = .background)
    
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
      } # if(!is.null(.y2.label)){
    } # if(!add)
    
    ### Background polygons (e.g., shading regions of plot to highlight time window)
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
    if(! is.na(.horizontal.line.at)){
      for(i in 1:length(.horizontal.line.at)){
        abline(h = .horizontal.line.at[i],
               col = ifelse(.theme == 'black', 'white', 'black'),
               lwd = 1.5 * .zoom,
               lty = 2) 
      }; rm(i)
    } # if(! is.na(.horizontal.line.at)){
    
    # Plot vertical lines
    if(! is.na(.horizontal.line.at)){
      for(i in 1:length(.vertical.line.at)){
        abline(v = .vertical.line.at[i],
               col = ifelse(.theme == 'black', 'white', 'black'),
               lwd = 1.5 * .zoom,
               lty = 2) 
      }; rm(i)
    } # if(! is.na(.horizontal.line.at)){
    
    # Plot noise distribution
    if(! is.null(.shuffle.dist.mean)){
      if(is.null(.shuffle.dist.x.vals)){.shuffle.dist.x.vals <- .x.values[[1]]}
      # Remove NAs
      .shuffle.dist.na.indices <- which(is.na(.shuffle.dist.mean))
      if(length(.shuffle.dist.na.indices)){ # if any NAs
        .shuffle.dist.mean <- .shuffle.dist.mean[-.shuffle.dist.na.indices]
        .shuffle.dist.error.bars <- .shuffle.dist.error.bars[-.shuffle.dist.na.indices]
        .shuffle.dist.x.vals <- .shuffle.dist.x.vals[-.shuffle.dist.na.indices]
      } # if any NAs
      # Plot error bars
      polygon(x = c(.shuffle.dist.x.vals, rev(.shuffle.dist.x.vals)),
              y = c(.shuffle.dist.mean - .shuffle.dist.error.bars, 
                    rev(.shuffle.dist.mean + .shuffle.dist.error.bars)),
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
    
    # Plot legend
    if(show.legend){
      add.text.line.multiple.colors(
        text.segments = names(.y.values),
        text.colors = .colors,
        .side = 3,
        .line = 0,
        .outer = FALSE)
    } # if(show.legend){
    
    # Plot data
    if(show.data){
      # Which time series in list of input time series to show? If not specified, show all
      if(is.null(show.which)){
        show.which <- 1:length(.y.values)
      } # if(is.null(show.which)){
      # Significant values
      if(! is.null(.p.values)){
        if(show.sig.points & (n.lines.with.sig.values > 0)){ 
          
          # Loop thru those and plot them
          .nth.sig.line.loop <- 0
          for(sig.line.loop in 1:length(.p.values)){ 
            # sig.line.loop = 1
            
            if(any(.p.values[[sig.line.loop]] < max(.sig.thresholds))){
              
              # Iterate
              .nth.sig.line.loop <- .nth.sig.line.loop + 1
              
              if(.sig.points.location == 'top'){
                .current.y <- .y.limits[2] - (sig.line.y.space * (.nth.sig.line.loop - 1))    
              }
              if(.sig.points.location == 'bottom'){
                .current.y <- .y.limits[1] + (sig.line.y.space * (.nth.sig.line.loop - 1))    
              }
              
              
              for(sig.point.loop in 1:length(.p.values[[sig.line.loop]])){
                if(.p.values[[sig.line.loop]][sig.point.loop] < max(.sig.thresholds)){ # If significant, plot
                  current.shape <- names(max(.sig.thresholds))
                  for(threshold.loop in 1:length(.sig.symbols)){
                    if(.p.values[[sig.line.loop]][sig.point.loop] < .sig.thresholds[threshold.loop]){
                      current.shape <- .sig.symbols[threshold.loop]
                    } # if(.p.values[[sig.line.loop]][sig.point.loop] < threshold.loop){
                  }; rm(threshold.loop)
                  points(x = sig.point.loop,
                         y = .current.y,
                         col = .colors[sig.line.loop],
                         pch = ifelse(current.shape == ".", 20, current.shape),
                         cex = ifelse(current.shape == ".", .9, 2) * .zoom) 
                } # If significant, plot
              }; rm(sig.point.loop)
            } # if(any(.p.values[[sig.lin.loop]] > max(.sig.thresholds))){
          }; rm(sig.line.loop) 
        } # if(show.sig & ...
      } # if(! is.null(.p.values)){
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
          for(point.loop in 1:length(.y.values[[i]])){
            segments(x0 = .x.values[[i]][point.loop],
                     y0 = ifelse(is.na(.error.bar.lower.limit),
                                 .y.values[[i]][point.loop] - .error.bars.lower[[i]][point.loop],
                                 max(.error.bar.lower.limit,
                                     .y.values[[i]][point.loop] - .error.bars.lower[[i]][point.loop])),
                     y1 = ifelse(is.na(.error.bar.upper.limit),
                                 .y.values[[i]][point.loop] + .error.bars.upper[[i]][point.loop],
                                 min(.error.bar.upper.limit,
                                     .y.values[[i]][point.loop] + .error.bars.upper[[i]][point.loop])),
                     col = adjust.transparency(.colors[i], alpha = .5),
                     lwd = .y.lwd[i] * .zoom)
            # # Caps
            # segments(x0 = .x.values[[i]][point.loop] - .1,
            #          x1 = .x.values[[i]][point.loop] + .1,
            #          y0 = .y.values[[i]][point.loop] - .error.bars.lower[[i]][point.loop],
            #          col = adjust.transparency(.colors[i], alpha = .5),
            #          lwd = .8 * .y.lwd[i] * .zoom)
            # segments(x0 = .x.values[[i]][point.loop] - .1,
            #          x1 = .x.values[[i]][point.loop] + .1,
            #          y0 = .y.values[[i]][point.loop] + .error.bars.upper[[i]][point.loop],
            #          col = adjust.transparency(.colors[i], alpha = .5),
            #          lwd = .8 * .y.lwd[i] * .zoom)
          }; rm(point.loop)
        }; rm(i)
      } # if(show.error){
      
      # Plot data
      for(i in show.which){
        par(new = TRUE)
        plot(y = .y.values[[i]],
             x = .x.values[[i]],
             col = .colors[i],
             type = 'b',
             pch = 20,
             lty = .y.lty[i],
             lwd = .y.lwd[i] * .zoom,
             ylim = list(.y.limits, .y2.limits)[[.which.y.axis[i]]],
             xlim = .x.limits,
             bty = 'n',
             xlab = '',
             ylab = '',
             xaxt = 'n',
             yaxt = 'n',
             cex = 2 * .zoom,
             log = .log.axis)  
      }; rm(i)
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
        if(is.null(.x.ticks)){.x.ticks <- 1:length(.y.values[[1]])}
        if(is.null(.x.tick.labels)){.x.tick.labels <- .x.ticks}
        axis(side = 1,
             at = .x.ticks,
             labels = .x.tick.labels,
             las = 1,
             tck = -.025 * .zoom, # length of tick
             padj = .6 * .zoom, # distance between tick and label
             lwd = 1.5 * .zoom,
             lwd.ticks = 1.5 * .zoom,
             cex.axis = text.size.med * .zoom,
             col = ifelse(.theme == 'black', 'white', 'black'),
             col.axis = ifelse(.theme == 'black', 'white', 'black'))
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
    } # if(!add){
  }


