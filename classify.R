### Get classification accuracies for each sample (t_train = t_test)
### Winter 2021
### adam.milton.morgan@gmail.com

classify <- function(data = sample.data, # data = sample.data[1:4]
                     n.rcv.folds = 10,
                     n.rcv.repeats = 3,
                     data.shuffled = FALSE,
                     seed = NA,
                     save.text = NA,
                     save.dir = NA, # ideally: '/Users/adam/Dropbox/Research/ChickenSyntax/analysis/R/naming classifier/[specific analysis folder]/results/data/'
                     save.subdir = NA, # any additional embedding folders to add, e.g., 'test'
                     features.to.use = use.these.features[['all']],
                     features.to.zero.out.at.test = c()){
  
  # Packages
  library('reshape2')
  library('doParallel')
  library('foreach')
  library('caret')
  
  # If current region has >= 1 elecs and isn't NA, classify
  features.to.zero.out.at.test <- 
    features.to.zero.out.at.test[which(! is.na(features.to.zero.out.at.test))] # remove NAs
  if(!all(features.to.zero.out.at.test %in% features.to.use)){
       message("Classification '",save.text,"' skipped because something is wrong with the feature specification.")
     }else{
    # Set seed if provided
    if(! is.na(seed)){set.seed(seed)}
    
       # Make combination function for foreach output
       comb <- function(x, ...) {
         lapply(seq_along(x),
                function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
       }
       
       ## Set up parallel processing
       # Close any old parallel backends
       unregister_dopar()
       # Set up parallel workers in case caret wants to use it
       n.cores <- parallel::detectCores() - n.cores.to.subtract
       cl <- makeCluster(n.cores, type="FORK")
       registerDoParallel(cl)
       
    ## Loop through samples and classify
    classifier.output.list <- 
      foreach(sample.loop = 1:length(names(data)), 
              .combine='comb', 
              .multicombine=TRUE,
              .init=list(list(), list(), list())) %dopar% { 
      # sample.loop = 1
      current.sample = names(data)[sample.loop]
      current.data = data[[current.sample]]
      
      ## Get rid of all columns except Y and X
      current.data = current.data[,c("word",
                                     paste0(patient,"_",features.to.use))]
      current.data$word <- as.character(current.data$word)
      
      # Set up repeated (3 times) k-Fold (10) validation
      train.folds <- createMultiFolds(y = current.data$word,
                                      k = n.rcv.folds,
                                      times = n.rcv.repeats)
      
      # Initialize prediction storage
      current.predictions <- list()
      current.importances <- list()
      
      # Loop thru repeated cross validation folds
      for(rcv.loop in 1:length(train.folds)){ # rcv.loop = 1 # for troubleshooting
        # Define train and test sets
        current.train.data = current.data[train.folds[[rcv.loop]],]
        current.test.data = current.data[-train.folds[[rcv.loop]],]
        
        # If zeroing out some electrodes in test data, do that
        if(length(features.to.zero.out.at.test) > 0){
          current.test.data[,paste0(patient,'_',features.to.zero.out.at.test)] <- 0
        }
        
        # If data.shuffled, shuffle labels
        if(data.shuffled){
          current.train.data$word <- 
            base::sample(current.train.data$word,
                         size = nrow(current.train.data),
                         replace = FALSE)
        }
      
        # Begin suppress output
        sink("/dev/null") 
        
        # Train
        current.classifier = train(word ~ .,
                                   data = current.train.data,
                                   method = "multinom",
                                   preProcess = c("center","scale"),
                                   tuneGrid = multinom.hps,
                                   trControl = trainControl(method = "none",
                                                            allowParallel = FALSE),
                                   verbose = FALSE)
        
        # End suppress output
        sink()
        
        # Test
        current.predictions[[rcv.loop]] = predict(current.classifier, 
                                                  newdata = current.test.data,
                                                  type = "prob")
        
        # Store results
        current.predictions[[rcv.loop]]$winner = predict(current.classifier,
                                                         newdata = current.test.data,
                                                         type = "raw")
        current.predictions[[rcv.loop]]$actual = current.test.data$word
        
        # Store variable importances
        current.importances[[rcv.loop]] = data.frame(t(varImp(current.classifier, scale = FALSE)$importance))
      }
      
      ## Get summary stats
      # Variable importance
        variable.importance <- data.frame('sample.label' = current.sample,
                                          t(colMeans(bind_rows(current.importances))))
      
      # Accuracy
      current.predictions <- bind_rows(current.predictions)
      current.predictions$accuracy <- with(current.predictions,
                                           as.numeric(winner == actual))
      classifier.accuracies = data.frame('sample.label' = current.sample,
                                         'accuracy' = mean(current.predictions$accuracy))
      
      # Prediction probabilities
        current.predictions$winner <- current.predictions$accuracy <- NULL
        current.predictions <- reshape2::melt(current.predictions,
                                    id.vars = 'actual',
                                    measure.vars = words,
                                    variable.name = 'model',
                                    value.name = 'probability')
        current.predictions <- data.frame(with(current.predictions, 
                                               tapply(probability, 
                                                      list(actual, model), 
                                                      mean)))
        current.predictions$actual <- rownames(current.predictions)
        current.predictions$sample.label <- current.sample
        current.predictions <- current.predictions[,c('sample.label','actual',words)]
        
        # Add to big list
        classifier.predictions <- current.predictions
      
      # Clean up
      rm(current.classifier, current.data, current.sample)
      message(patient,": Loop ",sample.loop," of ",length(data)," completed.")
      
      # Combine output dataframes into a list
      classifier.output <- list()
      classifier.output[['classifier.accuracies']] <- classifier.accuracies
        classifier.output[['variable.importance']] <- variable.importance
        classifier.output[['classifier.predictions']] <- classifier.predictions  
      
      return(classifier.output)
    } # End sample.loop
    
    # End parallel processing
    stopCluster(cl)
    
    ## Clean up output
    classifier.accuracies <- bind_rows(classifier.output.list[[1]])
    variable.importance <- bind_rows(classifier.output.list[[2]])
    classifier.predictions <- bind_rows(classifier.output.list[[3]])
      
    # Add time columns
    classifier.accuracies$time <- time.convert(classifier.accuracies$sample.label, "sample.labels", "times")
    classifier.predictions$time <- time.convert(classifier.predictions$sample.label, "sample.labels", "times")
    variable.importance$time <- time.convert(variable.importance$sample.label, "sample.labels", "times")
    
    ## Set up saving
    # Create save directory path
    if(is.na(save.dir)){
      save.dir <- paste0(path,
                         'analysis/R/naming classifier/save_path_not_provided/',
                         patient,'/')  
    }else{
      save.dir <- gsub('//','/',paste0(save.dir,'/'))
    }
    # Create save subdirectory path
    if(is.na(save.subdir)){
      save.subdir <- ifelse(data.shuffled, 'shuffled/', 'real/')
    }else{
      save.subdir <- gsub('//','/',paste0(save.subdir,'/'))
    }
    save.subdir <- paste0(patient,'/',save.subdir)
    # Create directory
    suppressWarnings(dir.create(paste0(save.dir, save.subdir), 
                                recursive = TRUE))
    
    # Save
    save.time <- Sys.time() # so as not to accidentally write over old files
    
    # Save accuracies
    suppressWarnings(dir.create(paste0(save.dir,save.subdir,'accuracies/'),
                                recursive = TRUE))
    write.csv(classifier.accuracies,
              paste0(save.dir,
                     save.subdir,
                     'accuracies/',
                     patient,
                     '_accuracies',
                     ifelse(data.shuffled, '_shuffledData_', '_realData_'),
                     'multinom_',length(features.to.use),'Elecs_',
                     ifelse(is.na(save.text),"",paste0(save.text,"_")),
                     save.time,'.csv'),
              row.names=FALSE, quote=FALSE)
    
    # Set up saving features
    suppressWarnings(dir.create(paste0(save.dir,save.subdir,'features/'),
                                recursive = TRUE))
    # Make dataframe
    features.df <- data.frame('feature' = features.to.use)
    
    # 0'ed out at test?
    features.df$zero.out.at.test <- 0
    rownames(features.df) <- features.df$feature
    features.df[features.to.zero.out.at.test,'zero.out.at.test'] <- 1
    
    # Add localizations
    features.df$region <- localizations[features.df$feature,'roi_electrodes']
    
    # Save
    write.csv(features.df,
              paste0(save.dir,
                     save.subdir,
                     'features/',
                     patient,
                     '_features used_',
                     ifelse(is.na(save.text),"",paste0(save.text,"_")),
                     save.time,'.csv'),
              row.names=FALSE, quote=FALSE)
    
    # If real data, variable importance and predictions
      suppressWarnings(dir.create(paste0(save.dir,save.subdir,'variable importance/'),
                                  recursive = TRUE))
      write.csv(variable.importance,
                paste0(save.dir,
                       save.subdir,
                       'variable importance/',
                       patient,
                       '_variable importance',
                       ifelse(data.shuffled, '_shuffledData_', '_realData_'),
                       'multinom_',length(features.to.use),'Elecs_',
                       ifelse(is.na(save.text),"",paste0(save.text,"_")),
                       save.time,'.csv'),
                row.names=FALSE, quote=FALSE)
      suppressWarnings(dir.create(paste0(save.dir,save.subdir,'prediction probabilities/'),
                                  recursive = TRUE))
      write.csv(classifier.predictions,
                paste0(save.dir,
                       save.subdir,
                       'prediction probabilities/',
                       patient,
                       '_prediction probabilities',
                       ifelse(data.shuffled, '_shuffledData_', '_realData_'),
                       'multinom_',length(features.to.use),'Elecs_',
                       ifelse(is.na(save.text),"",paste0(save.text,"_")),
                       save.time,'.csv'),
                row.names=FALSE, quote=FALSE)
    
    # Output
    return(classifier.accuracies)
  }
}