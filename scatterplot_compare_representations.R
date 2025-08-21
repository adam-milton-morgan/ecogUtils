### Plot sentence vs naming representations
### Winter 2023 (!!!!!!)
### adam.milton.morgan@gmail.com

library('pals')

scatterplot.compare.representations <- 
 function(
   .x.times,
   .y.times,
   .colors = rep(rgb(0,0,0), times = length(.x.times)),
   .x.y.limits = NULL,
   .title = '',
   .x.label = 'picture naming time (ms)',
   .y.label = 'sentence production\ntime (ms)',
   .time.lock = 'locked_to_production_onset',
   .theme = c('black','white')[2],
   .lwd = 4,
   .lty = 1,
   .zoom = 2,
   show.data = TRUE,
   show.which = NULL,
   show.t0 = TRUE,
   show.diagonal = TRUE,
   add = FALSE
    ){
    
    # Colors
    #source(paste0(path,'/analysis/R/functions/adjust_transparency.R'))
    #source(paste0(path,'/analysis/R/functions/smoothing.R'))
    library('pals')
    
    # Text sizes
    text.size.big <- 1.8
    text.size.med <- 1.6
    text.size.small <- 1.4
    
    # X-limits and tick marks
    if(is.null(.x.y.limits)){
      all.data <- unlist(c(.x.times, .y.times))
      .x.y.limits <- c(min(all.data), max(all.data))
    } # if(is.null(.x.limits)){
    
    # Make sure there are as many colors as there are y-values
    if(is.null(.colors)){
      .colors <- cubicl(length(.x.times))
    }
    if(length(.colors) < length(.x.times)){
      .colors <- rep(.colors, length.out = length(.x.times))
    }
    
    # Make sure there are as many line width values as there are y-values
    if(length(.lwd) < length(.x.times)){
      .lwd <- rep(.lwd, length.out = length(.x.times))
    }
    
    # Make sure there are as many line type values as there are y-values
    if(length(.lty) < length(.x.times)){
      .lty <- rep(.lty, length.out = length(.x.times))
    }
    
    # Plot area
    par(mar = c(5,5.5,5,1) * .zoom,
        bg = .theme)
    
    # Set up plot
    if(!add){
      par(pty = 's') # square
      plot(y = NULL,
           x = NULL,
           ylim = .x.y.limits,
           xlim = .x.y.limits,
           asp = 1,
           bty = 'n',
           xlab = '',
           ylab = '',
           xaxt = 'n',
           yaxt = 'n')
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
    } # if(!add)
    
    # Plot t=0 line
    if(show.t0){
      abline(v = 0,
             col = ifelse(.theme == 'black', 'white', 'black'),
             lwd = 1.5 * .zoom,
             lty = 2)  
      abline(h = 0,
             col = ifelse(.theme == 'black', 'white', 'black'),
             lwd = 1.5 * .zoom,
             lty = 2)  
    } # if(show.t0){
    
    # Plot diagonal lines
    if(show.diagonal){
      abline(a = 0, 
             b = 1,
             col = ifelse(.theme == 'black', 'white', 'black'),
             lwd = 1.5 * .zoom,
             lty = 2)
    }
    
    # Plot data
    if(show.data){
      # Which time series in list of input time series to show? If not specified, show all
      if(is.null(show.which)){
        show.which <- 1:length(.x.times)
      }

      # Plot data
      for(i in show.which){ # i = 1
        # i = i + 1
        
        points(x = .x.times[i],
              y = .y.times[i],
              col = .colors[i],
              pch = 20,
              cex = 2 * .zoom)  
        #readline('[enter]')
      }; rm(i)
    } # if(show.data){
    
    # Axes
    if(!add){
      axis(side = 1,
           #at = x.ticks,
           #labels = x.ticks,
           las = 0,
           tck = -.025 * .zoom, # length of tick
           padj = .6 * .zoom, # distance between tick and label
           lwd = 1.5 * .zoom,
           lwd.ticks = 1.5 * .zoom,
           cex.axis = text.size.med * .zoom,
           col = ifelse(.theme == 'black', 'white', 'black'),
           col.axis = ifelse(.theme == 'black', 'white', 'black'))
      axis(side = 2,
           #at = seq(from = .16, to = .26, by = .02),
           las = 0,
           tck = -.025 * .zoom, # length of tick
           padj = -.5 * .zoom, # distance between tick and label
           lwd = 1.5 * .zoom,
           lwd.ticks = 1.5 * .zoom,
           cex.axis = text.size.med * .zoom,
           las = 0,
           col = ifelse(.theme == 'black', 'white', 'black'),
           col.axis = ifelse(.theme == 'black', 'white', 'black'))
    } # if(!add){
  }


