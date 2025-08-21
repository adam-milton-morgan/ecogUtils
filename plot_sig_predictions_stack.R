### Plot PN classification accuracy significant windows and training window
### Spring 2023
### adam.milton.morgan@gmail.com

### Plot parameters
# Font sizes
text.size.big <- 1.8
text.size.med <- 1.6
text.size.small <- 1.4


### Plot
plot.sig.predictions.stack <- function(
    .cluster.times.df,
    .sig.windows,
    .sig.windows2 = NULL,
    .title = 'Significant Prediction Windows',
    .y.label = 'Clusters',
    .x.label = 'time from speech onset (ms)',
    .row.labels = NULL,
    .sig.colors = NULL,
    .sig2.colors = NULL,
    .train.time.color = NULL,
    .train.time.cap.length = 0,
    .zoom = 1,
    .theme = 'white',
    plot.guidelines = TRUE,
    show.row.labels = TRUE,
    .cluster.line.width.zoom = 1,
    offset.sig.windows = FALSE,
    .omit.clusters.with.no.sig = TRUE,
    .background = .theme,
    show.t0 = TRUE
){
  
  # Colors
  colors_bright <- read.csv(paste0(path,
                                   'analysis/R/color palettes/output/theme_',.theme,'/rainbow_bright.csv'), 
                            row.names = 1)
  if(is.null(.sig.colors)){.sig.colors <- colors_bright['purple','hex']}
  if(is.null(.sig2.colors)){.sig2.colors <- colors_bright['green','hex']}
  if(is.null(.train.time.color)){.train.time.color <- colors_bright['orange','hex']}
  
  # Omit clusters where there are no sig windows
  if(! is.null(.row.labels)){names(.row.labels) <- rownames(.cluster.times.df)}
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
  
  # Get enough colors for each line
  if(length(.sig.colors) < nrow(.cluster.times.df)){
    .sig.colors <- rep(.sig.colors, length.out = nrow(.cluster.times.df))
  }
  .sig.colors <- .sig.colors[cluster.middle.time.order]
  if(length(.sig2.colors) < nrow(.cluster.times.df)){
    .sig2.colors <- rep(.sig2.colors, length.out = nrow(.cluster.times.df))
  }
  .sig2.colors <- .sig2.colors[cluster.middle.time.order]
  
  # Set up plot area
  if((!is.null(.row.labels)) & show.row.labels){
    par(mar = .zoom * (c(5,5,4,8) + .1),
        bg = .background)
  }else{
    par(mar = .zoom * (c(5,5,4,2) + .1),
        bg = .background)  
  }
  
  plot(y = NULL,
       x = NULL,
       xlim = c(-1000, 500),
       ylim = c(.75, nrow(.cluster.times.df) + .25),
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
  
  # Plot t = 0 line
  if(show.t0){
  abline(v = 0,
         lty = 2,
         col = ifelse(.theme == 'white', 'black', 'white'),
         lwd = 2 * .zoom)
  } # if(show.t0){
  
  # Loop thru clusters
  if(nrow(.cluster.times.df) > 0){
  for(.group.loop in 1:nrow(.cluster.times.df)){
    # .group.loop = 1
    current.cluster <- rownames(.cluster.times.df)[.group.loop]
    # message(current.cluster,': ',cluster.names[.group.loop,'start.time'],' to ',cluster.names[.group.loop,'end.time'],'ms')
    # print(.sig.windows[[current.cluster]][,1:2])
    if(plot.guidelines){
      abline(h = .group.loop,
             lty = 2,
             lwd = 1 * .zoom,
             col = 'grey')  
    } # if(plot.guidelines){
    
    # Plot significant windows
    if(offset.sig.windows){
      .sig.windows.y.offset <- ifelse(is.null(.sig.windows2), 0, .05)  
    }else{
      .sig.windows.y.offset <- 0
    }
    if(! is.null(.sig.windows2[[current.cluster]])){
      if(nrow(.sig.windows2[[current.cluster]]) > 0){ # if there are any significant windows for this cluster
        for(sig.loop in 1:nrow(.sig.windows2[[current.cluster]])){ # sig.loop = 1
          segments(x0 = .sig.windows2[[current.cluster]][sig.loop,'start.time'],
                   x1 = .sig.windows2[[current.cluster]][sig.loop,'end.time'],
                   y0 = .group.loop - .sig.windows.y.offset,
                   col = .sig2.colors[.group.loop],
                   lwd = 4 * .zoom * .cluster.line.width.zoom)
        } # sig.loop  
      } # if there are any significant windows for this cluster  
    } # if this cluster isn't null
    if(! is.null(.sig.windows[[current.cluster]])){
      if(nrow(.sig.windows[[current.cluster]]) > 0){ # if there are any significant windows for this cluster
        for(sig.loop in 1:nrow(.sig.windows[[current.cluster]])){
          segments(x0 = .sig.windows[[current.cluster]][sig.loop,'start.time'],
                   x1 = .sig.windows[[current.cluster]][sig.loop,'end.time'],
                   y0 = .group.loop + .sig.windows.y.offset,
                   col = .sig.colors[.group.loop],
                   lwd = 4 * .zoom * .cluster.line.width.zoom)
        } # sig.loop  
      } # if there are any significant windows for this cluster
    } # if this cluster isn't null
    arrows(x0 = .cluster.times.df[.group.loop,'start.time'],
             x1 = .cluster.times.df[.group.loop,'end.time'],
             y0 = .group.loop,
             lwd = 2 * .zoom * .cluster.line.width.zoom,
             col = .train.time.color,
           code = 3,
           angle = 90,
           length = .train.time.cap.length)
  } # .group.loop
  } # if(nrow(.cluster.times.df) > 0){
    
  # Add row labels
  if((! is.null(.row.labels)) & show.row.labels){
    if(length(.row.labels) > 0){
    mtext(text = .row.labels,
          at = 1:nrow(.cluster.times.df),
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