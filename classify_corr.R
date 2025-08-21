### Get classification accuracies for each sample (t_train = t_test)
### Winter 2021
### adam.milton.morgan@gmail.com

classify.corr <- 
  function(data = sample.data, # data = sample.data[1:4]
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
  library('stringr')
  library('weights')
  
  # If current region has >= 1 elecs and isn't NA, classify
  features.to.zero.out.at.test <- 
    features.to.zero.out.at.test[which(! is.na(features.to.zero.out.at.test))] # remove NAs
  if(!all(features.to.zero.out.at.test %in% features.to.use)){
       message("Classification '",save.text,"' skipped because something is wrong with the feature specification.")
     }else{
    
       ## Set up
       # Set seed if provided
       if(! is.na(seed)){set.seed(seed)}
       
       # Create template dataframe for predictions
       pred.df.template <- data.frame('actual' = NA,
                                      'chicken' = NA,
                                      'dog' = NA,
                                      'dracula' = NA,
                                      'frankenstein' = NA,
                                      'ninja' = NA,
                                      'nurse' = NA,
                                      'winner' = NA)
       
       # Get all unique pairs of words to compare (there must be a simpler way to do this... janky af)
       word.pairs <- c()
       comparisons <- expand.grid('word1' = words, 'word2' = words)
       comparisons <- comparisons[with(comparisons, word1 != word2),]
       comparisons[,1] <- as.character(comparisons[,1])
       comparisons[,2] <- as.character(comparisons[,2])
       for(i in 1:nrow(comparisons)){
         # i = 1
         temp <- sort(c(comparisons[i,1], comparisons[i,2]))
         word.pairs <- c(word.pairs, paste0(temp[1],"_",temp[2]))
       }; rm(i, temp)
       word.pairs <- unique(word.pairs)
       comparisons <- data.frame(str_split_fixed(word.pairs, "_", 2))
       colnames(comparisons) <- c('word1','word2')
       
       # Make combination function for foreach output
       comb <- function(x, ...){
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
      
        # Divide up train data by word
        current.train.data.by.word <- list()
        for(word.loop in words){
          current.train.data.by.word[[word.loop]] <- current.train.data[current.train.data$word == word.loop,-1]
        }; rm(word.loop)
        
        # Define profiles (vector of means) for each word
        current.trained.profiles <- lapply(current.train.data.by.word, colMeans)
        
        ## Calculate weights for correlation terms: average t-value from a bunch of pairwise t-tests per electrode
        # Initialize dataframe for weights
        elec.weights <- data.frame('elec' = paste0(patient,"_",features.to.use))
        elec.weights$weight <- NA
        rownames(elec.weights) <- elec.weights$elec
        
        # Loop thru elecs
        for(elec.loop in paste0(patient,"_",features.to.use)){
          # elec.loop = paste0(patient,"_",features.to.use)[1]
          # Initialize storage for t-values
          current.cor.weights <- c()
          
          # Get t-values for every pairwise t-test
          for(comparison.loop in 1:nrow(comparisons)){
            # comparison.loop = 1
            current.cor.weights <-
              c(current.cor.weights,  
                t.test(current.train.data.by.word[[comparisons$word1[comparison.loop]]][,elec.loop],
                   current.train.data.by.word[[comparisons$word2[comparison.loop]]][,elec.loop])$statistic)
          }; rm(comparison.loop)
          
          ## Make a single weight out of t-values (should all be absolute valued)
          # squaring them didn't work
          #Selec.weights[elec.loop,'weight'] <- mean(current.cor.weights ^ 2)
          # just the mean?
          elec.weights[elec.loop,'weight'] <- mean(abs(current.cor.weights))
          # max?
          #elec.weights[elec.loop,'weight'] <- max(abs(current.cor.weights))
          # maybe next try max and median?
          #elec.weights[elec.loop,'weight'] <- median(abs(current.cor.weights))
        }; rm(elec.loop, current.cor.weights)
        
        ## Test
        # Initialize storage
        current.rcv.predictions <- list()
        for(test.trial.loop in 1:nrow(current.test.data)){ # test.trial.loop = 1
          current.rcv.predictions[[test.trial.loop]] <- pred.df.template
          current.rcv.predictions[[test.trial.loop]]$actual <- current.test.data$word[test.trial.loop]
          for(word.loop in names(current.trained.profiles)){ # word.loop = names(current.trained.profiles)[1]
            current.rcv.predictions[[test.trial.loop]][,word.loop] <-
              wtd.cors(x = current.trained.profiles[[word.loop]], 
                      y = unlist(current.test.data[test.trial.loop,c(-1)]),
                      weight = elec.weights$weight)
          }; rm(word.loop)
          meta.cols <- which(! colnames(current.rcv.predictions[[test.trial.loop]]) %in% words)
          current.rcv.predictions[[test.trial.loop]]$winner <- 
            names(which(unlist(current.rcv.predictions[[test.trial.loop]][,-meta.cols]) == 
                          max(unlist(current.rcv.predictions[[test.trial.loop]][,-meta.cols]))))
        }; rm(test.trial.loop, meta.cols)
        current.predictions[[rcv.loop]] <- bind_rows(current.rcv.predictions)
      }
      
      # Clean up
      current.predictions <- bind_rows(current.predictions)
      
      # Accuracy
      current.predictions$accuracy <- with(current.predictions,
                                           as.numeric(winner == actual))
      classifier.accuracies = data.frame('sample.label' = current.sample,
                                         'accuracy' = mean(current.predictions$accuracy))
      
      # Variable important (null for now)
      variable.importance <- classifier.accuracies # placeholder while getting script running
      variable.importance$accuracy <- NA
      
      # Prediction probabilities
      current.predictions$winner <- current.predictions$accuracy <- NULL
      classifier.predictions <- reshape2::melt(current.predictions,
                                               id.vars = 'actual',
                                               measure.vars = words,
                                               variable.name = 'model',
                                               value.name = 'probability')
      classifier.predictions <- data.frame(with(classifier.predictions, 
                                                tapply(probability, 
                                                       list(actual, model), 
                                                       mean)))
      classifier.predictions$actual <- rownames(classifier.predictions)
      classifier.predictions$sample.label <- current.sample
      classifier.predictions <- classifier.predictions[,c('sample.label','actual',words)]

      # Clean up
      rm(current.trained.profiles, current.data, current.sample, current.predictions)
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