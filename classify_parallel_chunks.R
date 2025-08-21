### Get classification accuracies for each sample (t_train = t_test)
### Winter 2021
### adam.milton.morgan@gmail.com

classify.parallel.chunks <- 
  function(data = sample.data,
           chunk.size = 20,
           n.rcv.folds = 10,
           n.rcv.repeats = 3,
           data.shuffled = FALSE,
           store.variable.importance = TRUE,
           store.prediction.probabilities = TRUE,
           seed = NA,
           save.text = NA,
           save.dir = NA, # ideally: '/Users/adam/Dropbox/Research/ChickenSyntax/analysis/R/naming classifier/[specific analysis folder]/results/data/'
           save.subdir = NA, # any additional embedding folders to add, e.g., 'test'
           features.to.use = use.these.features[['all']],
           features.to.zero.out.at.test = c(),
           feature.type = c("elecs","PCs")[1],
           n.metadata.cols = n.lx.cols){
    
    # Packages
    library('reshape2')
    library('doParallel') # for parallel processing
    library('foreach') # for parallel for loops
    # Close any old parallel backends
    source(paste0(path,'analysis/R/functions/unregister_dopar.R'))
    
    # If current region has >= 1 elecs and isn't NA, classify
    features.to.zero.out.at.test <- 
      features.to.zero.out.at.test[which(! is.na(features.to.zero.out.at.test))] # remove NAs
    if((! all(is.na(features.to.use))) &
       (length(features.to.use) - length(features.to.zero.out.at.test) >= 1) &
       all(features.to.zero.out.at.test %in% features.to.use)){
      
      # Set seed if provided
      if(! is.na(seed)){set.seed(seed)}
      
      ## Initialize storage
      # Accuracies
      classifier.accuracies = data.frame(matrix(nrow=length(names(data)), ncol=2))
      colnames(classifier.accuracies) = c('sample.label','accuracy')
      
      ## Initialize storage for optional metadata
      # Variable importance
      variable.importance <- data.frame(matrix(nrow=length(names(data)), 
                                               ncol=length(features.to.use)))
      colnames(variable.importance) <- paste0(patient,'_',features.to.use)
      rownames(variable.importance) <- names(data)
      
      ## Storage warnings
      if(data.shuffled){
        if(store.variable.importance){
          message("Warning: store.variable.importance and data.shuffled are both TRUE. Unnecessary and storage intensive. Consider not storing.")}
        if(store.prediction.probabilities){
          message("Warning: store.prediction.probabilities and data.shuffled are both TRUE. Unnecessary and storage intensive. Consider not storing.")}
      }else{
        if(! store.variable.importance){
          message("Heads up: data.shuffled is FALSE and store.variable.importance is FALSE. Consider storing for real data analyses.")}
        if(! store.prediction.probabilities){
          message("Heads up: data.shuffled is FALSE and store.prediction.probabilities is FALSE. Consider storing for real data analyses.")}
      }
      
      # Predictions
      classifier.predictions <- list()  
      
      # Function for combining foreach outputs
      combine.foreach.outputs <- function(x, ...) {
        lapply(seq_along(x),
               function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
      }
      
      ## Set up parallel processing
      # Close any old parallel backends
      unregister_dopar()
      # Set up parallel workers in case caret wants to use it
      if(is.null(n.cores)){
        n.cores <- ceiling(parallel::detectCores() * (2/3))
      }
      cl <- makeCluster(n.cores, type="FORK")
      registerDoParallel(cl)
      
      ## Loop through samples and classify
      output <- 
        foreach(sample.loop = 1:length(names(data)),
              .combine = 'combine.foreach.outputs',
              .multicombine = TRUE,
              .init = list(list(), list(), list()),
              #.final = function(x) setNames(x, names(data)),
              .options.nws = list(chunkSize = chunk.size)) %dopar% { # sample.loop = 1
        current.sample = names(data)[sample.loop]
        current.data = data[[current.sample]]
        
        ## Get rid of all columns except Y and X
        # Get rid of linguistic columns
        current.data = current.data[,-which(names(current.data)[1:n.metadata.cols]!='word')]
        current.data$word <- as.character(current.data$word)
        
        # Just the electrodes specified
        current.data = current.data[,c("word",
                                       paste0(patient,"_",features.to.use))]
        
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
        if(store.variable.importance){
          current.importances <- bind_rows(current.importances)
          current.importances <- cbind(data.frame('sample.label' = current.sample),
                                       data.frame(t(colMeans(current.importances))))
        }
        
        # Accuracy
        current.predictions <- bind_rows(current.predictions)
        current.predictions$accuracy <- with(current.predictions,
                                             as.numeric(winner == actual))
        classifier.accuracies[sample.loop,] =
          c(current.sample, mean(current.predictions$accuracy))
        
        # Prediction probabilities
        if(store.prediction.probabilities){
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
          rownames(current.predictions) <- NULL
        }
        
        # Clean up
        rm(current.classifier, current.data, current.sample)
        message(patient,": Loop ",sample.loop," of ",length(data)," completed (with ",getDoParWorkers()," workers in parallel backend).")
        
        return(list(classifier.accuracies[sample.loop,],
                    current.importances,
                    current.predictions))
      }; rm(sample.loop)
      
      # End parallel processing
      stopCluster(cl)
      beep()
      
      output
      ## Organize data
      classifier.accuracies <- bind_rows(output[[1]])
      
      
      # Accuracies
      classifier.accuracies$time <- time.convert(classifier.accuracies$sample.label, "sample.labels", "times")
      
      # Prediction probabilities
      if(store.prediction.probabilities){
        classifier.predictions <- bind_rows(output[[3]])
        classifier.predictions$time <- time.convert(classifier.predictions$sample.label, "sample.labels", "times")
      }
      
      if(store.variable.importance){
        variable.importance <- bind_rows(output[[2]])
        rownames(variable.importance) <- variable.importance$sample.label
        variable.importance$time <- time.convert(variable.importance$sample.label, "sample.labels", "times")
      }
      
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
      if(! is.na(save.subdir)){
        save.subdir <- paste0(patient,'/',save.subdir)
      }else{
        save.subdir <- paste0(patient,'/')
      }
      save.subdir <- gsub('//','/',paste0(save.subdir,'/'))
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
      if(feature.type == "elecs"){
        features.df$region <- localizations[features.df$feature,'roi_electrodes']
      }
      
      # Save
      write.csv(features.df,
                paste0(save.dir,
                       save.subdir,
                       'features/',
                       patient,
                       '_features used_',
                       ifelse(data.shuffled, '_shuffledData_', '_realData_'),
                       ifelse(is.na(save.text),"",paste0(save.text,"_")),
                       save.time,'.csv'),
                row.names=FALSE, quote=FALSE)
      
      # If real data, variable importance and predictions
      if(store.variable.importance){
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
      }
      if(store.prediction.probabilities){
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
      }
      
      # Output
      return(classifier.accuracies)
    }
    else{
      message("Classification '",
              ifelse(is.na(save.text),'[no name]',save.text),
              "' skipped because something is wrong with the feature specification.")
    }
  }