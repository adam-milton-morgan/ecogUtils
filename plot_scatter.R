### Scatter plots
### Summer 2024
### adam.milton.morgan@gmail.com

plot.scatter <- 
  function(
    .y.values = NULL,
    .x.values = NULL,
    .y.limits = NULL,
    .x.limits = NULL,
    .title = '',
    .y.label = '',
    .x.label = '',
    .dot.color = NULL,
    .dot.transparency = 1, 
    .dot.size = 1.4,
    .y.ticks = NULL,
    .y.tick.labels = NULL,
    .x.ticks = NULL,
    .x.tick.labels = NULL,
    .theme = c('white','black')[1],
    .background = NULL,
    .margin = NULL,
    .lms.to.plot = NULL, # List of LM objects
    .lm.colors = NULL,
    .horizontal.line.at = NULL,
    .vertical.line.at = NULL,
    show.diagonal = FALSE,
    show.x.axis = TRUE,
    show.y.axis = TRUE,
    show.lms = TRUE,
    .x.tick.labels.vertical = FALSE,
    .add = FALSE,
    .zoom = 1
  ){
    
    source(paste0(path,'/analysis/R/functions/adjust_transparency.R'))
    
    if(is.null(.background)){.background <- .theme}
    if(is.null(.y.limits)){.y.limits <- range(unlist(.y.values))}
    if(is.null(.x.limits)){.x.limits <- range(unlist(.x.values))}
    if(is.null(.dot.color)){.dot.color <- rgb(.5,.5,.5,1)}
    .dot.color <- adjust.transparency(.dot.color, alpha = .dot.transparency)
    if(is.null(.margin)){
      .margin <- rep(NA, times = 4)
      .margin[3] <- ifelse(.title != '', 5.5, 3.5)
      .margin[2] <- .margin[4] <- ifelse(.y.label != '', 5.5, 3.5)
      .margin[1] <- ifelse(.x.label != '', 5.5, 3.5)
    }
    
    par(bg = .background,
        mar = .margin * .zoom)
    
    if(! .add){
      plot(x = NULL,
           y = NULL,
           xlim = .x.limits,
           ylim = .y.limits,
           bty = 'n',
           xaxt = 'n',
           yaxt = 'n',
           main = .title,
           xlab = .x.label,
           ylab = .y.label)
    }
    
    if(! is.null(.lms.to.plot)){
      if(! "list" %in% class(.lms.to.plot)){
        .lms.to.plot <- list(.lms.to.plot)
      }
      if(is.null(.lm.colors)){.lm.colors <- rgb(.5,.5,.5)}
      if(show.lms){
        for(lm.loop in 1:length(.lms.to.plot)){
          abline(.lms.to.plot[[lm.loop]],
                 lwd = 3 * .zoom,
                 col = .lm.colors[lm.loop])
        }; rm(lm.loop)
      }; rm(show.lms)
    } # if(! is.null(.lms.to.plot)){
    
    if(show.diagonal){
      abline(a = 0, 
             b = 1, 
             lty = 2,
             col = ifelse(.theme == 'black', 'white', 'black'),
             lwd = 1.5 * .zoom)
    } # if(show.diagonal){
    if(! is.null(.horizontal.line.at)){
      abline(h = .horizontal.line.at,
             lty = 2,
             lwd = 1.5 * .zoom,
             col = ifelse(.theme == 'black', 'white', 'black'))
    } # if(! is.null(.horizontal.line.at)){
    if(! is.null(.vertical.line.at)){
      abline(h = .vertical.line.at,
             lty = 2,
             lwd = 1.5 * .zoom,
             col = ifelse(.theme == 'black', 'white', 'black'))
    } # if(! is.null(.vertical.line.at)){
    
    ### Plot data
    points(x = .x.values,
           y = .y.values,
           xlab = '',
           xlim = .x.limits,
           ylim = .y.limits,
           ylab = '',
           pch = 20,
           cex = .dot.size * .zoom,
           col = .dot.color,
           bty = 'n',
           xaxt = 'n',
           yaxt = 'n')
    
    ### Axes
    if(show.x.axis){
      if(is.null(.x.ticks)){
        if(! is.null(.x.tick.labels)){message("Disregarding '.x.tick.labels' because '.x.ticks' not specified.")}
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
             col = ifelse(.theme == 'black', 'white', 'black'),
             col.axis = ifelse(.theme == 'black', 'white', 'black'))
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
             col = ifelse(.theme == 'black', 'white', 'black'),
             col.axis = ifelse(.theme == 'black', 'white', 'black'))
      } # if(is.null(.y.ticks)){}else{
    } # if(show.y.axis){
  } # plot.scatter()


