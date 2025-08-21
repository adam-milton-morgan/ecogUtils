### Plot a stack of time series (similar to erpimage in EEGLAB and viz.plot_epochs_image() in MNE)
### Summer 2021
### adam.milton.morgan@gmail.com
### Based on a script by Mark Gardener 2015

stack.plot <- 
  function(elec.data, # dataframe for one elec/feature; rows = trials; cols = metadata and samples
           data.columns = time.convert(-1024:512, "samples", "sample.labels"), # names of columns with brain data in them
           plot.title = '',
           sort.by = c('production_latency',NA)[1], # name of column to sort by - should be a previously-ordered factor or numeric
           plot.sort.by.line = TRUE,
           plot.sort.by.col = 'black',
           plot.sort.by.neg, # -1 for locked_to_production_onset, 1 for stimulus_onset
           time.ticks = list("locked_to_stimulus_onset" = c(-1000,-500,0,500,1000,1500,2000),
                             "locked_to_production_onset" = c(-2000,-1500,-1000,-500,0,500,1000))[[2]],
           squash.outliers.beyond.quantiles = c(.05,.95), # set values beyond these percentiles in the dist to that percentile
           color.bar.label = "HGP (z)",
           row.id.label = "trials",
           plot.0.line = TRUE,
           plot.0.col = 'white',
           zoom = 1){ # time.lock = 'locked_to_production_onset'
    
    # Functions
    library("reshape2")
    library("data.table")
    library("ggplot2")
    library("viridis")
    
    # Function to convert between samples, sample labels, and times
    source(paste0(path,'/analysis/R/functions/time_convert.R'))
    
    # Font sizes
    text.size.big = 20
    text.size.med = 18
    text.size.small = 16
    
    # Sort by order of production latency
    if(! is.na(sort.by)){elec.data <- elec.data[order(elec.data[,sort.by]),]}
    # Give unique labels to each trial, ordered by sort.by
    elec.data$row.id <- paste0("row.id_",1:nrow(elec.data))
    elec.data$row.id <- factor(elec.data$row.id,
                               levels = paste0("row.id_",1:nrow(elec.data)))
    
    # Get rid of extraneous columns
    keep.cols <- c('row.id')
    if(!is.na(sort.by)){keep.cols <- c(sort.by, keep.cols)}
    elec.data <- elec.data[,c(keep.cols, data.columns)]
    
    # Reshape wide data -> long
    elec.data <- melt(setDT(elec.data),
                      id.vars = keep.cols,
                      variable.name = "sample.label",
                      value.name = "plot.vals")
    
    # Set up times 
    elec.data$time.real <- time.convert(elec.data$sample.label, "sample.labels", "samples") * 1000 / 512
    
    # Swap sign of production latency if plotted on left
    # elec.data[,sort.by] <- plot.sort.by.neg * elec.data$production_latency
    
    # Limit to data from event window
    time.tick.samples <- time.convert(time.ticks, "times","samples")
    
    # Set positive outliers to whatever the 97.5th %ile value is
    elec.data$plot.vals[which(elec.data$plot.vals > quantile(elec.data$plot.vals, squash.outliers.beyond.quantiles[2]))] <- 
      quantile(elec.data$plot.vals, squash.outliers.beyond.quantiles[2])
    elec.data$plot.vals[which(elec.data$plot.vals < quantile(elec.data$plot.vals, squash.outliers.beyond.quantiles[1]))] <- 
      quantile(elec.data$plot.vals, squash.outliers.beyond.quantiles[1])
    
    p <- ggplot(data = elec.data,
                aes(x = time.real,
                    y = row.id,
                    fill = plot.vals)) +
      geom_tile() + 
      scale_x_continuous(name = "time (ms)",
                         breaks = time.ticks,
                         labels = time.ticks,
                         expand = c(0, 0),
                         limits = c(min(time.ticks), max(time.ticks))) + 
      scale_y_discrete(name = row.id.label,
                       breaks = c(),
                       labels = "",
                       expand = c(0, 0)) + 
      # scale_fill_distiller(palette = "magma",
                           # breaks = as.integer(c(ceiling(min(elec.data$plot.vals)), 0, floor(max(elec.data$plot.vals)))),
                           # #labels = as.character(as.integer(c(ceiling(min(elec.data$plot.vals)), 0, floor(max(elec.data$plot.vals))))),
                           # limits = c(min(elec.data$plot.vals), max(elec.data$plot.vals)),
                           # guide = guide_colorbar(title = color.bar.label,
                           #                        barwidth = 1 * zoom,
                           #                        barheight = 6 * zoom)) +
      scale_fill_viridis(option = "magma",
                         breaks = as.integer(c(ceiling(min(elec.data$plot.vals)), 0, floor(max(elec.data$plot.vals)))),
                           limits = c(min(elec.data$plot.vals), max(elec.data$plot.vals)),
                         # limits = c(max(abs(min(elec.data$plot.vals)), abs(max(elec.data$plot.vals)))) * c(-1, 1),
                           guide = guide_colorbar(title = color.bar.label,
                                                  barwidth = 1 * zoom,
                                                  barheight = 6 * zoom)) +
      #theme_classic() + # removes background grid and top/right plot borders
      ggtitle(plot.title) + 
      theme(axis.text.x = element_text(size = text.size.small * zoom),
            axis.title = element_text(size = text.size.med * zoom),
            plot.title = element_text(size = text.size.big * zoom),
            legend.text = element_text(size = text.size.small * zoom),
            legend.title = element_text(size = text.size.med * zoom),
            # Get rid of background and grid and border
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    if(plot.sort.by.line){
      p <- p + geom_line(aes(x = plot.sort.by.neg * time.convert(production_latency, "samples", "times"),
                             y = as.numeric(gsub("row.id_","",row.id))),
                         linewidth = 1 * zoom,
                         color = plot.sort.by.col)
    }
    if(plot.0.line){
      p <- p + geom_line(aes(x = 0,
                             y = as.numeric(gsub("row.id_","",row.id))),
                         linewidth = .5 * zoom,
                         color = plot.0.col,
                         linetype = "dotted")
    }
    
    return(p)
  }

