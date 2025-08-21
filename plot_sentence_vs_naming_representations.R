### Plot sentence vs naming representations
### Winter 2023 (!!!!!!)
### adam.milton.morgan@gmail.com



# Fake data
n.patients = 10
n.obs = 3
fake.data <- data.frame('patient' = rep(paste0('NY',1:n.patients), each = n.obs),
                        'pn.start' = sample(-500:200, size = n.patients * n.obs))
fake.data$pn.end <- fake.data$pn.start + sample(20:150, size = n.patients * n.obs)
fake.data$sp.start <- fake.data$pn.start + rnorm(n = n.patients * n.obs, mean = 100, sd = 50)
fake.data$sp.end <- fake.data$sp.start + abs((fake.data$pn.end - fake.data$pn.start) + rnorm(n = n.patients * n.obs, mean = -20, sd = 10))
plot(fake.data$pn.end - fake.data$pn.start, fake.data$sp.end - fake.data$sp.start)

#plot.sentence.vs.naming.representations <- 
 # function(
   .pn.start.times = fake.data$pn.start
   .pn.end.times = fake.data$pn.end
   .sp.start.times = fake.data$sp.start
   .sp.end.times = fake.data$sp.end
   .colors = c(rep(rgb(64, 195, 160, maxColorValue = 255), times = 4), 
               rep(rgb(228, 208, 90, maxColorValue = 255), times = n.patients * n.obs - 4))
   .x.y.limits = NULL
   .title = ''
   .x.label = 'picture naming time (ms)'
   .y.label = 'sentence production\ntime (ms)'
   .time.lock = c('locked_to_production_onset','locked_to_stimulus_onset')[1]
   #.error.bars = NULL
   .theme = c('black','white')[2]
   #.sig.color = ifelse(.theme == 'black', rgb(.5,.5,.5,1), rgb(.75,.75,.75,1))
   .lwd = 4
   .lty = 1
   .zoom = 2
   show.data = TRUE
   #show.sig.bars = TRUE
   #show.sig.highlights = FALSE
   #show.error = TRUE
   #show.which = NULL
   show.t0 = TRUE
   show.diagonal = TRUE
   #.horizontal.line.at = NA
   add = FALSE
  #  ){
    
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
      all.data <- unlist(c(.pn.start.times, .pn.end.times, .sp.start.times, .sp.end.times))
      .x.y.limits <- c(min(all.data), max(all.data))
    } # if(is.null(.x.limits)){
    
    # Make sure there are as many colors as there are y-values
    if(is.null(.colors)){
      .colors <- cubicl(1500)[round(.pn.start.times) - 
                                c('locked_to_production_onset' = -1000,
                                  'locked_to_stimulus_onset' = 0)[.time.lock]]
    }
    if(length(.colors) < length(.pn.start.times)){
      .colors <- rep(.colors, length.out = length(.pn.start.times))
    }
    
    # Make sure there are as many line width values as there are y-values
    if(length(.lwd) < length(.pn.start.times)){
      .lwd <- rep(.lwd, length.out = length(.pn.start.times))
    }
    
    # Make sure there are as many line type values as there are y-values
    if(length(.lty) < length(.pn.start.times)){
      .lty <- rep(.lty, length.out = length(.pn.start.times))
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
        show.which <- 1:length(.pn.start.times)
      }

      # Plot data
      for(i in show.which){ # i = 1
        # i = i + 1
        message('i=',i,'; .pn.start=',round(.pn.start.times[i]),'; pn.end=',round(.pn.end.times[i]),'; sp.start=',round(.sp.start.times[i]),'; sp.end=',round(.sp.end.times[i]))
        segments(x0 = .pn.start.times[i],
              x1 = .pn.end.times[i],
              y0 = .sp.start.times[i],
              y1 = .sp.end.times[i],
              col = .colors[i],
              lty = .lty[i],
              lwd = .lwd[i] * .zoom)  
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
  #}


