### Get classification accuracies for each sample (t_train = t_test)
### Winter 2021
### adam.milton.morgan@gmail.com

accuracy.plot <- 
  function(acc.df,
           dv = 'accuracy',
           lock.loop = c('locked_to_production_onset','locked_to_stimulus_onset')[2],
           sig.windows,
           add = FALSE,
           shade.region = NA, # c(100,200)
           shade.label = NA,
           shade.color = NA,
           scale = 1,
           title = paste0(patient,": Noun Classification"),
           subtitle = "",
           show.acc = TRUE,
           smooth = FALSE,
           half.smooth.window.time = 50,
           show.acc.ci = FALSE,
           show.shuff = TRUE,
           show.sig = TRUE,
           acc.color = "#780085",
           additional.dvs = c(),
           additional.dv.colors = c('black'),
           shuff.color = 'grey',
           show.theoretical.chance = FALSE,
           theoretical.chance = 1/6){
    
    # Colors
    source(paste0(path,'/analysis/R/functions/adjust_transparency.R'))
    source(paste0(path,'/analysis/R/functions/smoothing.R'))
    library('pals')
    colors <- cubicl(10)
    
    
    # Text sizes
    text.size.big <- 1.8
    text.size.med <- 1.6
    text.size.small <- 1.4
    
    # X-limits and tick marks
    if(lock.loop == 'locked_to_production_onset'){
      x.limits <- c(-1000, 500)
      x.ticks <- c(-1000, -500, 0, 500)
    }
    if(lock.loop == 'locked_to_stimulus_onset'){
      x.limits <- c(-500, 1000)
      x.ticks <- c(-500, 0, 500, 1000)
    }
    
    # Do smoothing
    if(smooth){
      # Smooth accuracies
      if(show.acc){
        acc.df[,dv] <- smoothing(acc.df[,dv],
                                 n.samples.pre = floor(half.smooth.window.time * 512 / 1000),
                                 n.samples.post = floor(half.smooth.window.time * 512 / 1000))
        if(length(additional.dvs) > 0){
          for(i in 1:length(additional.dvs)){
            acc.df[,additional.dvs[i]] <- smoothing(acc.df[,additional.dvs[i]],
                                                    n.samples.pre = floor(half.smooth.window.time * 512 / 1000),
                                                    n.samples.post = floor(half.smooth.window.time * 512 / 1000))
          }
        }
      } # if(show.acc)
      
      # Smooth shuffle distribution
      if(show.shuff){
        acc.df$shuff.upper.95ci <- 
          smoothing(acc.df$shuff.upper.95ci,
                    n.samples.pre = floor(half.smooth.window.time * 512 / 1000),
                    n.samples.post = floor(half.smooth.window.time * 512 / 1000))
        acc.df$shuff.lower.95ci <- 
          smoothing(acc.df$shuff.lower.95ci,
                    n.samples.pre = floor(half.smooth.window.time * 512 / 1000),
                    n.samples.post = floor(half.smooth.window.time * 512 / 1000))
        acc.df$shuff.mean <- 
          smoothing(acc.df$shuff.mean,
                    n.samples.pre = floor(half.smooth.window.time * 512 / 1000),
                    n.samples.post = floor(half.smooth.window.time * 512 / 1000))
        acc.df <- droplevels(subset(acc.df, ! is.na(shuff.mean)))
      } # if(show.shuff)
    } # if(smooth)
    
    # Calculate Y-limits: absolute max and min across plots within patient
    all.y.vals <- c(acc.df[,dv], acc.df$upper.95ci, acc.df$shuff.upper.95ci, acc.df$lower.95ci, acc.df$shuff.lower.95ci)
    if(length(additional.dvs) > 0){
      for(i in 1:length(additional.dvs)){
        all.y.vals <- c(all.y.vals, acc.df[,additional.dvs[i]])
      }
    }
    y.maxes <- max(all.y.vals, na.rm = TRUE)
    y.mins <- min(all.y.vals, na.rm = TRUE)
    
    # Store Y-limits
    y.limits <- c(min(.16, min(y.mins))[1], max(y.maxes)[1])
    names(y.limits) <- c("min", "max")
    y.range <- as.numeric(y.limits['max'] - y.limits['min'])
    
    # Plot area
    par(mar = c(5,5.5,5,1) * scale)
    
    # Set up plot
    if(!add){
      plot(NULL,
           ylim = y.limits,
           xlim = x.limits,
           main = paste0(title,
                         ifelse(is.na(subtitle),"",paste0('\n',subtitle))),
           cex.main = text.size.big * scale,
           bty = 'n',
           xlab = '',
           ylab = '',
           xaxt = 'n',
           yaxt = 'n')
      title(ylab = 'accuracy',
            cex.lab = text.size.big * scale,
            line = 3.75 * scale)
      title(xlab = 'time (ms)',
            cex.lab = text.size.big * scale,
            line = 3.5 * scale)
    } # if(!add)
    
    # Shade region
    if(! is.na(shade.region[1])){
      if(is.na(shade.color)){shade.color <- colors[4]}
      polygon(x = c(shade.region, rev(shade.region)),
              y = c(y.limits[1] - y.range, y.limits[1] - y.range,
                    y.limits[2] + y.range, y.limits[2] + y.range),
              border = FALSE,
              col = adjust.transparency(shade.color, alpha = 120))
      if(! is.na(shade.label)){
        text(x = shade.region[2],
             y = y.limits[2] - (y.range / 20),
             labels = shade.label,
             cex = 1.6 * scale,
             col = shade.color)
      }
    }
    
    # Significant windows
    if(show.sig){
      for(sig.loop in 1:length(sig.windows)){
        segments(x0 = sig.windows[[sig.loop]]['start.time'],
                 x1 = sig.windows[[sig.loop]]['end.time'],
                 y0 = y.limits['min'] + (y.range / 20),
                 col = adjust.transparency(acc.color, alpha = 140),
                 lwd = 3.5 * scale)
      }; rm(sig.loop)
    }
    
    if(show.shuff){
      
      # Shuffle 95% confidence interval
      polygon(x = c(acc.df$time, rev(acc.df$time)),
              y = c(acc.df$shuff.upper.95ci, rev(acc.df$shuff.lower.95ci)),
              border = FALSE,
              #col = colors[3])
              col = adjust.transparency(shuff.color, alpha = 180))
      
      # Shuffle mean (smoothed)
      lines(x = acc.df$time,
            y = acc.df$shuff.mean,
            col = shuff.color,
            lwd = 3 * scale)
    }
    
    # Plot t=0 line
    abline(v = 0,
           col = 'black',
           lwd = 1.5 * scale,
           lty = 2)
    
    # Plot theoretical chance line
    if(show.theoretical.chance){
      abline(h = theoretical.chance, 
             col = 'black',
             lwd = 1.5 * scale,
             lty = 2)  
    }
    
    
    # Plot accuracy
    if(show.acc){
      if(show.acc.ci){
        polygon(x = c(acc.df$time, rev(acc.df$time)),
                y = c(get.confidence.interval(acc.df[,dv], acc.df[,paste0('sd.',dv)], 'upper'), 
                      rev(get.confidence.interval(acc.df[,dv], acc.df[,paste0('sd.',dv)], 'lower'))),
                border = FALSE,
                col = adjust.transparency(acc.color,
                                          alpha = 180))
      }
      # Plot any additonal DVs first (so behind)
      if(length(additional.dvs) > 0){
        for(i in 1:length(additional.dvs)){
          lines(y = acc.df[,additional.dvs[i]],
                x = acc.df$time,
                col = additional.dv.colors[i],
                lwd = 3 * scale)
        }
      }
      
      # Plot DV
      lines(y = acc.df[,dv],
            x = acc.df$time,
            col = acc.color,
            lwd = 3 * scale)
    }
    
    # Axes
    if(!add){
      axis(side = 1,
           at = x.ticks,
           labels = x.ticks,
           las = 0,
           tck = -.012 * scale, # length of tick
           padj = .6 * scale, # distance between tick and label
           lwd = 1.5 * scale,
           cex.axis = text.size.med * scale)
      axis(side = 2,
           #at = seq(from = .16, to = .26, by = .02),
           las = 0,
           tck = -.012 * scale, # length of tick
           padj = -.5 * scale, # distance between tick and label
           lwd = 1.5 * scale,
           cex.axis = text.size.med * scale,
           las = 0)
    }
  }