### Plot PN classification accuracy significant windows and training window
### Spring 2023
### adam.milton.morgan@gmail.com

### Plot parameters
# Font sizes
text.size.big <- 1.8
text.size.med <- 1.6
text.size.small <- 1.4


### Plot
plot.time.series.stack <- function(
    .cluster.times.df,
    .sig.windows,
    .sig.windows2 = NULL,
    .time.series.y = NULL,
    .time.series.x = NULL,
    .time.series2.y = NULL,
    .time.series2.x = NULL,
    .sampling.rate = 512,
    .title = 'Significant Prediction Windows',
    .y.label = 'Clusters',
    .x.label = 'time from speech onset (ms)',
    .x.limits = c(-1000, 500),
    .x.ticks = NULL,
    .x.tick.labels = NULL,
    .row.labels = NULL,
    .sig.colors = NULL,
    .sig2.colors = NULL,
    .train.time.color = NULL,
    .time.series.color = NULL,
    .time.series2.color = NULL,
    .time.series.lwd = NULL,
    .time.series2.lwd = NULL,
    .time.series.scale.factor = 1,
    .train.time.cap.length = 0,
    .density.line.color = NULL,
    .density.fill.color = NULL,
    .density.height.proportion = .05, # proportion of plot area height
    .density.height.scale = 1, # proportion of density plot area to fill vertically
    .density.smoothing.samples = 3,
    .density.bandwidth = 'nrd0', # a method or a number, usually between 0 and 1
    .density.variable = c('mean.time.series','significance')[2],
    .zoom = 1,
    .theme = 'white',
    .margin = NULL,
    plot.guidelines = TRUE,
    show.row.labels = TRUE,
    show.sig.bars = TRUE,
    show.sig.highlights = FALSE,
    show.train.times = TRUE,
    show.density = TRUE,
    show.density1 = NULL,
    show.density2 = NULL,
    .cluster.line.width.zoom = 1,
    .train.line.width.zoom = 1,
    offset.sig.windows = FALSE,
    .omit.clusters.with.no.sig = TRUE,
    .background = .theme,
    show.t0 = TRUE
){
  
  ### Key
  # .cluster.times.df: dataframe with row labels corresponding to .sig.windows names, and columns 'start.time', and 'end.time' defining training window
  
  
  # Colors
  source(paste0(path,'analysis/R/functions/adjust_transparency.R'))
  source(paste0(path,'analysis/R/functions/smoothing.R'))
  source(paste0(path,'analysis/R/functions/get_significant_windows_inverse.R'))
  colors_bright <- read.csv(paste0(path,
                                   'analysis/R/color palettes/output/theme_',.theme,'/rainbow_bright.csv'), 
                            row.names = 1)
  colors_darkest <- read.csv(paste0(path,
                                    'analysis/R/color palettes/output/theme_',.theme,'/rainbow_darkest.csv'), 
                             row.names = 1)
  if(is.null(.sig.colors)){.sig.colors <- colors_bright['purple','hex']}
  if(is.null(.sig2.colors)){.sig2.colors <- colors_bright['green','hex']}
  if(is.null(.time.series.color)){
    .time.series.color <- colorspace::lighten(.sig.colors, amount = .1, method = 'absolute')}
  if(is.null(.time.series2.color)){
    .time.series2.color <- colorspace::lighten(.sig2.colors, amount = .1, method = 'absolute')}
  if(is.null(.train.time.color)){.train.time.color <- colors_bright['orange','hex']}
  
  # Omit clusters where there are no sig windows
  if(! is.null(.row.labels)){
    names(.row.labels) <- rownames(.cluster.times.df)
  }
  if(.omit.clusters.with.no.sig){
    for(.group.loop in rownames(.cluster.times.df)){
      if(nrow(.sig.windows[[.group.loop]]) == 0){
        if(! is.null(.sig.windows2)){
          if(nrow(.sig.windows2[[.group.loop]]) == 0){
            # If no sig windows, remove!
            .sig.windows[[.group.loop]] <- NULL
            .sig.windows2[[.group.loop]] <- NULL
            .cluster.times.df <- .cluster.times.df[-which(rownames(.cluster.times.df) == .group.loop),]  
            if(! is.null(.row.labels)){.row.labels[.group.loop] <- 'REMOVE.THIS.LABEL'}
          }
        }else{
          .sig.windows[[.group.loop]] <- NULL
          .cluster.times.df <- .cluster.times.df[-which(rownames(.cluster.times.df) == .group.loop),]
          if(! is.null(.row.labels)){.row.labels[.group.loop] <- 'REMOVE.THIS.LABEL'}
        }
      }
    }; rm(.group.loop)
    if(! is.null(.row.labels)){.row.labels <- .row.labels[.row.labels != 'REMOVE.THIS.LABEL']}
  }
  
  # Sort by median train time
  .cluster.times.df$middle.time <- apply(.cluster.times.df[,c('start.time', 'end.time')], 1, mean)
  cluster.middle.time.order <- order(.cluster.times.df$middle.time)
  .cluster.times.df <- .cluster.times.df[cluster.middle.time.order,]
  .row.labels <- .row.labels[cluster.middle.time.order]
  
  # Get enough colors for each line
  if(length(.sig.colors) < nrow(.cluster.times.df)){
    .sig.colors <- rep(.sig.colors, length.out = nrow(.cluster.times.df))
  }
  .sig.colors <- .sig.colors[cluster.middle.time.order]
  if(length(.sig2.colors) < nrow(.cluster.times.df)){
    .sig2.colors <- rep(.sig2.colors, length.out = nrow(.cluster.times.df))
  }
  .sig2.colors <- .sig2.colors[cluster.middle.time.order]
  
  # Show density plots?
  if(is.null(show.density1)){
    show.density1 <- show.density
  }
  if(is.null(show.density2)){
    if(is.null(.sig.windows2)){
      show.density2 <- FALSE
    }else{
      show.density2 <- show.density
    }
  }
  
  # Set up density plot
  if(show.density1 | show.density2){
    # Set up density plot
    if(is.null(.density.line.color)){
      if(show.density1 & show.density2){
        # If both, set colors to both
        .density.line.color <- c(.sig.colors[1], .sig2.colors[1])
      }else{
        # If only one (necessarily one is TRUE in this bracket), set color accordingly
        .density.line.color <- ifelse(show.density1, .sig.colors[1], .sig.colors[2])
      }
    } # if(is.null(.density.line.color)){
    
    if(is.null(.density.fill.color)){
      .density.fill.color <- adjust.transparency(.density.line.color, alpha = .3)
    }
    
    .density.height <- (.density.height.proportion * (nrow(.cluster.times.df) + .5)) / (1 - .density.height.proportion)
    
    # Make sure x-values are in a list
    if(is.null(.time.series.x)){stop("Must provide .time.series.x for density plots.")}
    if(class(.time.series.x) != "list"){.time.series.x <- list(.time.series.x)}
    
    # If time.series.x are sample labels, convert to times
    for(.x.loop in 1:length(.time.series.x)){
      if(class(.time.series.x[[.x.loop]]) == "character"){
        .time.series.x[[.x.loop]] <- time.convert(.time.series.x[[.x.loop]], "sample.labels", "times", input.sampling.rate = .sampling.rate)
      } # if(class(.time.series.x[[.x.loop]]) == "character"){
    } # .x.loop
    
    # Store max vals for ts1 and ts2 density plot for scaling them relative to each other
    .max.val.for.scaling <- list()
    
    # Get density
    if(.density.variable == 'mean.time.series'){
      if(show.density1 & (! show.density2)){
        .density.y <- list(apply(do.call(rbind, .time.series.y), 2, mean))
        .density.y <- lapply(.density.y, function(x){smoothing(x, n.samples.pre = .density.smoothing.samples)})
        .max.val.for.scaling[[1]] <- max(unlist(.density.y), na.rm = TRUE)
      }
      if((! show.density1) & show.density2){
        .density.y <- list(apply(do.call(rbind, .time.series2.y), 2, mean))
        .density.y <- lapply(.density.y, function(x){smoothing(x, n.samples.pre = .density.smoothing.samples)})
        .max.val.for.scaling[[2]] <- max(unlist(.density.y), na.rm = TRUE)
      }
      if(show.density1 & show.density2){
        .density.y <- lapply(list(.time.series.y, .time.series2.y), function(x){apply(do.call(rbind, x), 2, mean)})
        .density.y <- lapply(.density.y, function(x){smoothing(x, n.samples.pre = .density.smoothing.samples)})
        .max.val.for.scaling <- lapply(.density.y, function(x){max(unlist(x), na.rm = TRUE)})
      }
      .density.x <- list(.time.series.x[[1]], .time.series.x[[1]])
      
      # Get rid of NAs from smoothing
      for(k in 1:length(.density.y)){
        .remove.indices <- which(is.na(.density.y[[k]]))
        .density.x[[k]] <- .density.x[[k]][-.remove.indices]
        .density.y[[k]] <- .density.y[[k]][-.remove.indices]
      }; rm(k)
    } # if(.density.variable == 'mean.time.series'){
    if(.density.variable == 'significance'){
      
      .density.x.limits <- time.convert(.x.limits, "times", "samples", .sampling.rate)
      .density.x.vals.samples <- .density.x.limits[1]:.density.x.limits[2]
      .density.x <- .density.y <- list()
      
      # Get Density 1
      if(show.density1){
        .density.vals.samples1 <- lapply(.sig.windows, function(x){
          x <- get.significant.windows.inverse(x,
                                               .samples = .density.x.vals.samples,
                                               .sampling.rate = .sampling.rate, 
                                               output.class = "vector")
          x <- names(which(x == 1))
          x <- time.convert(x, "sample.labels", "samples")
          return(x)
        })
        if(length(unlist(.density.vals.samples1)) < 2){
          message("WARNING: Fewer than 2 significant samples in .sig.windows (TS1), not enough to do density plot; setting 'show.density1' to FALSE.")
          show.density1 <- FALSE
        }else{
          .density <- density(unlist(.density.vals.samples1), bw = .density.bandwidth)
          .density.x[[1]] <- time.convert(.density$x, "samples", "times", .sampling.rate)
          .density.y[[1]] <- .density$y
          .max.val.for.scaling[[1]] <- max(table(unlist(.density.vals.samples1)))
        }
      } # if(show.density1){
      
      # Get Density2
      if(show.density2){
        .density.vals.samples2 <- lapply(.sig.windows2, function(x){
          x <- get.significant.windows.inverse(x,
                                               .samples = .density.x.vals.samples,
                                               .sampling.rate = .sampling.rate, 
                                               output.class = "vector")
          x <- names(which(x == 1))
          x <- time.convert(x, "sample.labels", "samples")
          return(x)
        })
        if(length(unlist(.density.vals.samples2)) < 2){
          message("Warning: Fewer than 2 significant samples in .sig.windows (TS2), not enough to do density plot; setting 'show.density2' to FALSE.")
          show.density2 <- FALSE
        }else{
          .density2 <- density(unlist(.density.vals.samples2), bw = .density.bandwidth)
          .density.x[[length(.density.x) + 1]] <- time.convert(.density2$x, "samples", "times", .sampling.rate)
          .density.y[[length(.density.y) + 1]] <- .density2$y
          .max.val.for.scaling[[2]] <- max(table(unlist(.density.vals.samples2)))
        }
      } # if(show.density2)
    } # if(.density.variable == 'significance'){
  } # if(show.density1 | show.density2){
  
  # Scale density if still plotting it
  if(show.density1 | show.density2){
    .density.y <- lapply(.density.y, function(x){
      x / max(x) * .density.height * .density.height.scale
    })
  } # if(show.density1 | show.density2){
  if(show.density1 & show.density2){
    if(.max.val.for.scaling[[1]] > .max.val.for.scaling[[2]]){
      .density.y[[2]] <- .density.y[[2]] * .max.val.for.scaling[[2]] / .max.val.for.scaling[[1]]
    }else{
      .density.y[[1]] <- .density.y[[1]] * .max.val.for.scaling[[1]] / .max.val.for.scaling[[2]]
    }
  } # if(show.density1 & show.density2){
    
  # Set up plot area
  if(is.null(.margin)){
    .margin <- c(ifelse(.x.label == '', 2.5, 5.5), 
                 ifelse(.y.label == '', 1.5, 5.5), 
                 ifelse(.title == '', 1, 5), 
                 1.5)
    .margin <- .margin * .zoom
  }else{.margin <- .margin * .zoom}
  if((!is.null(.row.labels)) & show.row.labels){
    # Make room for row labels on right if adding them
    .margin[3] <- 5 * .zoom}
  par(mar = .margin,
      bg = .background)
  
  plot(y = NULL,
       x = NULL,
       xlim = .x.limits,
       ylim = c(.5, nrow(.cluster.times.df) + .5 + ifelse(show.density1 | show.density2, .density.height, 0)),
       bty = 'n',
       xlab = '',
       ylab = '',
       yaxt = 'n',
       xaxt = 'n')
  title(.title,
        cex.main = text.size.big * .zoom,
        col.main = ifelse(.theme == 'black', 'white', 'black'))
  title(ylab = .y.label,
        cex.lab = text.size.big * .zoom,
        line = 3.75 * .zoom,
        col.lab = ifelse(.theme == 'black', 'white', 'black'))
  title(xlab = .x.label,
        cex.lab = text.size.big * .zoom,
        line = 3.5 * .zoom,
        col.lab = ifelse(.theme == 'black', 'white', 'black'))
  
  # Plot t = 0 line
  if(show.t0){
    abline(v = 0,
           lty = 2,
           col = ifelse(.theme == 'white', 'black', 'white'),
           lwd = 2 * .zoom)
  } # if(show.t0){
  
  # Plot density
  if(show.density1 | show.density2){
    
    # Space evenly between x-axis and lower end of plot
    .x.axis.y.val <- par("usr")[3]
    .density.y <- lapply(.density.y, function(.y){.y + (.x.axis.y.val / 2)})
    
    for(k in 1:length(.density.y)){
      polygon(x = c(.density.x[[k]], rev(.density.x[[k]])),
              y = c(.density.y[[k]], rep(min(.density.y[[k]]), times = length(.density.y[[k]]))),
              col = .density.fill.color[k],
              border = NA)
      lines(y = .density.y[[k]],
            x = .density.x[[k]],
            lwd = 2 * .zoom,
            col = .density.line.color[k])
    }; rm(k)
    
  } # if(show.density)
  
  # Loop thru clusters
  if(nrow(.cluster.times.df) > 0){
    for(.group.loop in 1:nrow(.cluster.times.df)){
      # .group.loop = 1
      current.cluster <- rownames(.cluster.times.df)[.group.loop]
      
      ### Plot guidelines
      if(plot.guidelines){
        abline(h = .group.loop + ifelse(show.density1 | show.density2, .density.height, 0),
               lty = 2,
               lwd = 1 * .zoom,
               col = 'grey')  
      } # if(plot.guidelines){
      
      ### Set up time series plot 1
      if(! is.null(.time.series.y)){
        # Convert y-values to a list if it's not already -- so you can loop thru list and plot lines
        if(class(.time.series.y) != "list"){
          stop('ERROR: .time.series.y should be a list with names corresponding to rownames(.cluster.times.df)')
        }
        if(length(.time.series.y) != length(.sig.windows)){
          stop('ERROR: .time.series.y should have the same number of time series as there are .sig.windows (with the same labels, which are the rownames of .cluster.times.df)')
        }
        
        # Scale
        all.ts.ys <- unlist(.time.series.y)
        if(! is.null(.time.series2.y)){
          all.ts.ys <- c(all.ts.ys, unlist(.time.series2.y))
        }
        max.ts.y <- quantile(all.ts.ys, .99, na.rm = TRUE)
        .time.series.y <- lapply(.time.series.y, function(x){
          x <- x / max.ts.y / 2.5 * .time.series.scale.factor
        })
        
        # Make sure there are as many colors are there are time series
        if(length(.time.series.color) < length(.time.series.y)){
          .time.series.color <- rep(.time.series.color, length.out = length(.time.series.y))
        }
        
        # Make sure there are as many line width values as there are y-values
        if(is.null(.time.series.lwd)){.time.series.lwd <- 100^(1/length(.time.series.y)) - .3}
        if(length(.time.series.lwd) < length(.time.series.y)){
          .time.series.lwd <- rep(.time.series.lwd, length.out = length(.time.series.y))
        }
        # Make sure no line thickness is greater than 2.5
        .time.series.lwd <- sapply(.time.series.lwd, function(x){min(x, 3)})
      } # if(! is.null(.time.series.y)){
      
      ### Set up time series plot 2
      if(! is.null(.time.series2.y)){
        # Convert y-values to a list if it's not already -- so you can loop thru list and plot lines
        if(class(.time.series2.y) != "list"){
          stop('ERROR: .time.series2.y should be a list with names corresponding to rownames(.cluster.times.df)')
        }
        if(length(.time.series2.y) != length(.sig.windows2)){
          stop('ERROR: .time.series2.y should have the same number of time series as there are .sig.windows2 (with the same labels, which are the rownames of .cluster.times.df)')
        }
        
        # Scale
        all.ts2.ys <- unlist(.time.series2.y)
        if(! is.null(.time.series2.y)){
          all.ts2.ys <- c(all.ts2.ys, unlist(.time.series2.y))
        }
        max.ts2.y <- quantile(all.ts2.ys, .99, na.rm = TRUE)
        .time.series2.y <- lapply(.time.series2.y, function(x){
          x <- x / max.ts2.y / 2.5 * .time.series.scale.factor
        })
        
        # Make sure x-values are in a list
        if(class(.time.series2.x) != "list"){
          .time.series2.x <- list(.time.series2.x)}
        
        # If time.series.x are sample labels, convert to times
        for(.x.loop in 1:length(.time.series2.x)){
          if(class(.time.series2.x[[.x.loop]]) == "character"){
            .time.series2.x[[.x.loop]] <- time.convert(.time.series2.x[[.x.loop]], "sample.labels", "times", input.sampling.rate = .sampling.rate)
          } # if(class(.time.series2.x[[.x.loop]]) == "character"){
        } # .x.loop
        
        # Make sure there are as many lists of x-values as there are y-values
        if((length(.time.series2.y) > 1) & (length(.time.series2.x) == 1)){
          for(i in 2:length(.y.values)){
            .time.series2.x[[i]] <- .time.series2.x[[1]]
          }; rm(i)
        }
        
        # Make sure there are as many colors are there are time series
        if(length(.time.series2.color) < length(.time.series2.y)){
          .time.series2.color <- rep(.time.series2.color, length.out = length(.time.series2.y))
        }
        
        # Make sure there are as many line width values as there are y-values
        if(is.null(.time.series2.lwd)){.time.series2.lwd <- 100^(1/length(.time.series2.y)) - .3}
        if(length(.time.series2.lwd) < length(.time.series2.y)){
          .time.series2.lwd <- rep(.time.series2.lwd, length.out = length(.time.series2.y))
        }
        # Make sure no line thickness is greater than 2.5
        .time.series2.lwd <- sapply(.time.series2.lwd, function(x){min(x, 3)})
      } # if(! is.null(.time.series2.y)){
      
      ### Plot significant windows and time series
      if(offset.sig.windows){
        .sig.windows.y.offset <- ifelse(is.null(.sig.windows2), 0, .05)  
      }else{
        .sig.windows.y.offset <- 0
      }
      if(! is.null(.sig.windows2[[current.cluster]])){
        
        ## Sig bars/highlights
        if(nrow(.sig.windows2[[current.cluster]]) > 0){ # if there are any significant windows for this cluster
          for(sig.loop in 1:nrow(.sig.windows2[[current.cluster]])){
            
            # Sig bars
            if(show.sig.bars){
              # Sig windows bars
              segments(x0 = .sig.windows2[[current.cluster]][sig.loop,'start.time'],
                       x1 = .sig.windows2[[current.cluster]][sig.loop,'end.time'],
                       y0 = .group.loop + .sig.windows.y.offset + ifelse(show.density1 | show.density2, .density.height, 0),
                       col = .sig2.colors[.group.loop],
                       lwd = 4 * .zoom * .cluster.line.width.zoom)
            } # if(show.sig.bars){  
            
            # Sig highlights
            if(show.sig.highlights){
              # Get x-values for highlight lines
              .highlight.sample.range <- time.convert(unlist(.sig.windows2[[current.cluster]][sig.loop,c('start.time','end.time')]),'times', 'samples', .sampling.rate)
              .highlight.sample.labels <- time.convert(.highlight.sample.range[1]:.highlight.sample.range[2], 'samples', 'sample.labels', .sampling.rate)
              .highlight.indices <- which(time.convert(.time.series2.x[[current.cluster]], 'times', 'sample.labels', .sampling.rate) %in% .highlight.sample.labels)
              
              # Sig windows highlights
              .sig.highlights2.lwd <- max(4 * .zoom * .cluster.line.width.zoom, 3 * max(.time.series2.lwd))
              lines(x = .time.series2.x[[current.cluster]][.highlight.indices],
                    y = .group.loop + .time.series2.y[[current.cluster]][.highlight.indices] + ifelse(show.density1 | show.density2, .density.height, 0),
                    col = .sig2.colors[.group.loop],
                    lwd = .sig.highlights2.lwd)
              
              # Clean up 
              rm(.highlight.sample.range, .highlight.sample.labels, .highlight.indices)
              
            } # if(show.sig.highlights){  
          } # sig.loop  
        } # if there are any significant windows for this cluster
        
        ## Plot time series 2
        if(! is.null(.time.series2.y)){
          lines(x = .time.series2.x[[current.cluster]],
                y = .time.series2.y[[current.cluster]] + .group.loop + ifelse(show.density1 | show.density2, .density.height, 0),
                lwd = .time.series2.lwd,
                col = .time.series2.color)
        } # if(! is.null(.time.series2.y)){
      } # if this cluster isn't null
      if(! is.null(.sig.windows[[current.cluster]])){
        
        ## Sig bars/highlights
        if(nrow(.sig.windows[[current.cluster]]) > 0){ # if there are any significant windows for this cluster
          for(sig.loop in 1:nrow(.sig.windows[[current.cluster]])){
            
            # Sig bars
            if(show.sig.bars){
              # Sig windows bars
              segments(x0 = .sig.windows[[current.cluster]][sig.loop,'start.time'],
                       x1 = .sig.windows[[current.cluster]][sig.loop,'end.time'],
                       y0 = .group.loop + .sig.windows.y.offset + ifelse(show.density1 | show.density2, .density.height, 0),
                       col = .sig.colors[.group.loop],
                       lwd = 4 * .zoom * .cluster.line.width.zoom)
            } # if(show.sig.bars){  
            
            # Sig highlights
            if(show.sig.highlights){
              # Get x-values for highlight lines
              .highlight.sample.range <- time.convert(unlist(.sig.windows[[current.cluster]][sig.loop,c('start.time','end.time')]),'times', 'samples', .sampling.rate)
              .highlight.sample.labels <- time.convert(.highlight.sample.range[1]:.highlight.sample.range[2], 'samples', 'sample.labels', .sampling.rate)
              .highlight.indices <- which(time.convert(.time.series.x[[current.cluster]], 'times', 'sample.labels', .sampling.rate) %in% .highlight.sample.labels)
              
              # Sig windows highlights
              .sig.highlights.lwd <- max(4 * .zoom * .cluster.line.width.zoom, 3 * max(.time.series.lwd))
              lines(x = .time.series.x[[current.cluster]][.highlight.indices],
                    y = .group.loop + .time.series.y[[current.cluster]][.highlight.indices] + ifelse(show.density1 | show.density2, .density.height, 0),
                    col = .sig.colors[.group.loop],
                    lwd = .sig.highlights.lwd)
              
              # Clean up 
              rm(.highlight.sample.range, .highlight.sample.labels, .highlight.indices)
              
            } # if(show.sig.highlights){  
          } # sig.loop  
        } # if there are any significant windows for this cluster
        
        ## Plot time series 1
        if(! is.null(.time.series.y)){
          lines(x = .time.series.x[[current.cluster]],
                y = .time.series.y[[current.cluster]] + .group.loop + ifelse(show.density1 | show.density2, .density.height, 0),
                lwd = .time.series.lwd,
                col = .time.series.color)
        } # if(! is.null(.time.series.y)){
      } # if this cluster isn't null
      if(show.train.times){
        arrows(x0 = .cluster.times.df[.group.loop,'start.time'],
               x1 = .cluster.times.df[.group.loop,'end.time'],
               y0 = .group.loop + ifelse(show.density1 | show.density2, .density.height, 0),
               lwd = 2 * .zoom * .train.line.width.zoom,
               col = .train.time.color,
               code = 3,
               angle = 90,
               length = .train.time.cap.length)
      } # .group.loop
    }# if(show.train.times){
  } # if(nrow(.cluster.times.df) > 0){
  
  # Add axis
  if(is.null(.x.ticks)){.x.ticks <- sort(unique(c(.x.limits, seq(.x.limits[1], .x.limits[2], by = 500))))}
  if(is.null(.x.tick.labels)){.x.tick.labels <- as.character(.x.ticks)}
  axis(side = 1,
       at = .x.ticks,
       labels = .x.tick.labels,
       las = 0,
       tck = -.025 * .zoom, # length of tick
       padj = .6 * .zoom, # distance between tick and label
       lwd = 1.5 * .zoom,
       lwd.ticks = 1.5 * .zoom,
       cex.axis = text.size.med * .zoom,
       col = ifelse(.theme == 'black', 'white', 'black'),
       col.axis = ifelse(.theme == 'black', 'white', 'black'))
  
  # Add row labels
  if((! is.null(.row.labels)) & show.row.labels){
    if(length(.row.labels) > 0){
      mtext(text = .row.labels,
            at = c(1:nrow(.cluster.times.df)) + ifelse(show.density1 | show.density2, .density.height, 0),
            las = 2,
            line = .5 * .zoom,
            #pos = 4
            side = 4,
            outer = FALSE,
            cex = (text.size.small - .2) * .zoom,
            col = 'grey')
    } # if(length(.row.labels) > 0){
  } # if((! is.null(.row.labels)) & show.row.labels){
  
}# plot.cluster.classification.rasters()