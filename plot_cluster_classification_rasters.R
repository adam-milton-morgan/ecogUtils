### Plot PN classification accuracy significant windows and training window
### Spring 2023
### adam.milton.morgan@gmail.com

### Plot parameters
# Font sizes
text.size.big <- 1.8
text.size.med <- 1.6
text.size.small <- 1.4


### Plot
plot.cluster.classification.rasters <- function(
    .cluster.names,
    .sig.windows,
    .sig.windows2 = NULL,
    .title = 'Significant PN Classification Windows',
    .y.label = 'Clusters',
    .x.label = 'time from speech onset (ms)',
    .row.labels = NULL,
    .sig.colors = colors_bright['purple','hex'],
    .sig2.colors = colors_bright['green','hex'],
    .zoom = 1,
    .theme = 'white',
    plot.guidelines = TRUE,
    .cluster.line.width.zoom = 1,
    offset.sig.windows = FALSE
){

  # Colors
  colors_bright <- read.csv(paste0(path,
                                   'analysis/R/color palettes/output/theme_',.theme,'/rainbow_bright.csv'), 
                            row.names = 1)
  
  # Sort by median train time
  .cluster.names$middle.time <- apply(.cluster.names[,c('start.time.prod.locked', 'end.time.prod.locked')], 1, mean)
  cluster.middle.time.order <- order(.cluster.names$middle.time)
  .cluster.names <- .cluster.names[cluster.middle.time.order,]
  
  # Get enough colors for each line
  if(length(.sig.colors) < nrow(.cluster.names)){
    .sig.colors <- rep(.sig.colors, length.out = nrow(.cluster.names))
  }
  .sig.colors <- .sig.colors[cluster.middle.time.order]
  if(length(.sig2.colors) < nrow(.cluster.names)){
    .sig2.colors <- rep(.sig2.colors, length.out = nrow(.cluster.names))
  }
  .sig2.colors <- .sig2.colors[cluster.middle.time.order]
  
  # Set up plot area
  if(is.null(.row.labels)){
    par(mar = .zoom * (c(5,5,4,2) + .1))  
  }else{
    par(mar = .zoom * (c(5,5,4,8) + .1))
  }
  
  plot(y = NULL,
       x = NULL,
       xlim = c(-1000, 500),
       ylim = c(1, nrow(.cluster.names)),
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
  axis(side = 1,
       at = c(-1000,-500,0,500),
       labels = c(-1000,-500,0,500),
       las = 0,
       tck = -.025 * .zoom, # length of tick
       padj = .6 * .zoom, # distance between tick and label
       lwd = 1.5 * .zoom,
       lwd.ticks = 1.5 * .zoom,
       cex.axis = text.size.med * .zoom,
       col = ifelse(.theme == 'black', 'white', 'black'),
       col.axis = ifelse(.theme == 'black', 'white', 'black'))
  
  # Loop thru clusters
  for(cluster.loop in 1:nrow(.cluster.names)){
    # cluster.loop = 2
    current.cluster <- rownames(.cluster.names)[cluster.loop]
    # message(current.cluster,': ',cluster.names[cluster.loop,'start.time'],' to ',cluster.names[cluster.loop,'end.time'],'ms')
    # print(.sig.windows[[current.cluster]][,1:2])
    if(plot.guidelines){
      abline(h = cluster.loop,
             lty = 2,
             lwd = 1 * .zoom,
             col = 'grey')  
    }
    
    # Plot significant windows
    if(offset.sig.windows){
      .sig.windows.y.offset <- ifelse(is.null(.sig.windows2), 0, .05)  
    }
    if(! is.null(.sig.windows2[[current.cluster]])){
      if(nrow(.sig.windows2[[current.cluster]]) > 0){ # if there are any significant windows for this cluster
        for(sig.loop in 1:nrow(.sig.windows2[[current.cluster]])){ # sig.loop = 1
          segments(x0 = .sig.windows2[[current.cluster]][sig.loop,'start.time'],
                   x1 = .sig.windows2[[current.cluster]][sig.loop,'end.time'],
                   y0 = cluster.loop - .sig.windows.y.offset,
                   col = .sig2.colors[cluster.loop],
                   lwd = 4 * .zoom * .cluster.line.width.zoom)
        } # sig.loop  
      } # if there are any significant windows for this cluster  
    } # if this cluster isn't null
    if(! is.null(.sig.windows[[current.cluster]])){
      if(nrow(.sig.windows[[current.cluster]]) > 0){ # if there are any significant windows for this cluster
        for(sig.loop in 1:nrow(.sig.windows[[current.cluster]])){
          segments(x0 = .sig.windows[[current.cluster]][sig.loop,'start.time'],
                   x1 = .sig.windows[[current.cluster]][sig.loop,'end.time'],
                   y0 = cluster.loop + .sig.windows.y.offset,
                   col = .sig.colors[cluster.loop],
                   lwd = 4 * .zoom * .cluster.line.width.zoom)
        } # sig.loop  
      } # if there are any significant windows for this cluster
    } # if this cluster isn't null
    segments(x0 = .cluster.names[cluster.loop,'start.time.prod.locked'],
             x1 = .cluster.names[cluster.loop,'end.time.prod.locked'],
             y0 = cluster.loop,
             lwd = 2 * .zoom * .cluster.line.width.zoom,
             col = colors_bright['orange','hex'])
  } # cluster.loop
  
  # Add row labels
  if(! is.null(.row.labels)){
    mtext(text = .row.labels,
          at = 1:nrow(.cluster.names),
          las = 2,
          line = .5 * .zoom,
          #pos = 4
          side = 4,
          outer = FALSE,
          cex = (text.size.small - .2) * .zoom,
          col = 'grey')
  }
  
  # Plot t = 0 line
  abline(v = 0,
         lty = 2,
         col = 'black',
         lwd = 2 * .zoom)
}# plot.cluster.classification.rasters()