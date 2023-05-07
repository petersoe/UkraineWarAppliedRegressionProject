initialize <- function() {
  libraries <- c("MASS","reshape2","boot","glmnet","gridExtra","scales","progress","zoo","memoise","doParallel","foreach","ggplot2","dplyr", "changepoint", "tidyr", "MASS", "olsrr", "car",'gvlma','sjPlot','car','generalhoslem',"corrplot")
  for(lib in libraries)
  {
    suppressPackageStartupMessages(
      if (!require(lib, character.only = TRUE)) {
        print(paste(lib, "is not installed, attempting to install now..."))
        install.packages(lib)
        if (!require(lib, character.only = TRUE)) {
          stop(paste("Failed to install", lib))
        }
        print(paste(lib, "has been installed!"))
      } else {
        print(paste(lib, "is already installed"))
      }
    )
  }
  set.seed(123)
}

#this function is to import data sets as aggregates for use with some visualizations
getCumulativeData <- function() {
  # Read in the data
  totalEquipLost <- read.csv("russia_losses_equipment.csv")
  totalEquipLost$date <- as.POSIXct(totalEquipLost$date, format = "%m/%d/%Y")
  totalPersonelLost <- read.csv("russia_losses_personnel.csv")
  totalPersonelLost$date <- as.POSIXct(totalPersonelLost$date, format = "%m/%d/%Y")
  civHarm <- read.csv("ukr-civharm-2023-03-25.csv")
  majorEvents <- read.csv("MajorEventsVariable.csv")
  majorEvents$date <- as.POSIXct(majorEvents$date, format = "%m/%d/%Y")
  offensives <- read.csv("OffensivesVariable.csv")
  offensives$date <- as.POSIXct(offensives$date, format = "%m/%d/%Y")
  
  #structure civHarm into civHarmFreq and save dates for calculating when NA should be replaced with 0
  civHarmCumulative <- data.frame(table(civHarm$date))
  colnames(civHarmCumulative) <- c("date", "civHarmFreqCumulative")
  civHarmCumulative$date <- as.POSIXct(civHarmCumulative$date, format = "%m/%d/%Y")
  civHarmCumulative <- civHarmCumulative %>% arrange(date) %>% mutate(civHarmFreqCumulative = cumsum(civHarmFreqCumulative)) #arrange by date before Cumulative sum, so the Cumulative is over the proper time
  min_date <- min(civHarmCumulative$date) # Store this information for later
  max_date <- max(civHarmCumulative$date)
  
  # Merge the data frames
  merged_Cumulative_data <- merge(x = totalEquipLost, y = dplyr::select(totalPersonelLost, -day), by = "date", all = TRUE) %>%
    merge(y = majorEvents, by = "date", all = TRUE) %>%
    merge(y = offensives, by = "date", all = TRUE) %>%
    merge(y = civHarmCumulative, by = "date", all = TRUE) %>%
    # Replace missing civHarmFreqCumulative values last value for dates inside of the range
    mutate(civHarmFreqCumulative = ifelse(date >= min_date & date <= max_date, 
                                          na.locf(civHarmFreqCumulative, na.rm = FALSE), 
                                          civHarmFreqCumulative))
  # Replace missing values in majorEvents and Offensives with FALSE
  merged_Cumulative_data$isOffensive[is.na(merged_Cumulative_data$isOffensive)] <- FALSE
  merged_Cumulative_data$majorEvent[is.na(merged_Cumulative_data$majorEvent)] <- FALSE
  
  #get rid of first two days which suffer from a lot of NA values due to misalignment of data sources on when data started
  merged_Cumulative_data = merged_Cumulative_data[2:nrow(merged_Cumulative_data),]
  merged_Cumulative_data = dplyr::select(merged_Cumulative_data, date, day, civHarmFreqCumulative, aircraft:isOffensive)
  
  #ended up deciding not to use POW. Plot can be made for it, so add it back in if you want to see the plots here
  merged_Cumulative_data = merged_Cumulative_data[,-which(names(merged_Cumulative_data)=="POW")]
  
  #get rid of any after the last date of civilianHarm, I don't use any information past the last date of that column
  max_date_index <- which(merged_Cumulative_data$date==max_date)
  merged_Cumulative_data = merged_Cumulative_data[1:max_date_index,]
  
  return(merged_Cumulative_data)
}

#main import function
importData <- function() {
  # Read in the data
  totalEquipLost <- read.csv("russia_losses_equipment.csv")
  totalEquipLost$date <- as.POSIXct(totalEquipLost$date, format = "%m/%d/%Y")
  totalPersonelLost <- read.csv("russia_losses_personnel.csv")
  totalPersonelLost$date <- as.POSIXct(totalPersonelLost$date, format = "%m/%d/%Y")
  civHarm <- read.csv("ukr-civharm-2023-03-25.csv")
  majorEvents <- read.csv("MajorEventsVariable.csv")
  majorEvents$date <- as.POSIXct(majorEvents$date, format = "%m/%d/%Y")
  offensives <- read.csv("OffensivesVariable.csv")
  offensives$date <- as.POSIXct(offensives$date, format = "%m/%d/%Y")
  
  # Calculate equipment lost per day
  equipLost <- totalEquipLost %>% 
    mutate(across(!c(date, day), 
                  ~ ifelse(!is.na(.x) & !is.na(lag(.x, order_by = day)), 
                           .x - lag(.x, order_by = day), 
                           NA)))
  
  
  
  # Calculate personnel lost per day
  personelLost <- totalPersonelLost %>% 
    mutate(across(!c(date, day), 
                  ~ ifelse(!is.na(.x) & !is.na(lag(.x, order_by = day)), 
                           .x - lag(.x, order_by = day), 
                           NA)))
  
  #structure civHarm into civHarmFreq and save dates for calculating when NA should be replaced with 0
  civHarmFreq <- data.frame(table(civHarm$date))
  colnames(civHarmFreq) <- c("date", "CivAttackFreq")
  civHarmFreq$date <- as.POSIXct(civHarmFreq$date, format = "%m/%d/%Y") #this couldn't be done earlier. There were errors only fixed by moving this after the creation of the frequency tables
  min_date <- min(civHarmFreq$date) # Store this information for later
  max_date <- max(civHarmFreq$date)
  
  # Merge the data frames
  merged_data <- merge(x = equipLost, y = dplyr::select(personelLost, -"day"), by = "date", all = TRUE) %>%
    merge(y = majorEvents, by = "date", all = TRUE) %>%
    merge(y = offensives, by = "date", all = TRUE) %>%
    merge(y = civHarmFreq, by = "date", all = TRUE) %>%
    # Replace missing civHarmFreq values with 0 for dates inside of the range
    mutate(CivAttackFreq = ifelse(date >= min_date & date <= max_date, replace(CivAttackFreq, is.na(CivAttackFreq), 0), CivAttackFreq))
  
  # Replace missing values in majorEvents and Offensives with FALSE
  merged_data$isOffensive[is.na(merged_data$isOffensive)] <- FALSE
  merged_data$majorEvent[is.na(merged_data$majorEvent)] <- FALSE
  
  #get rid of first two days which suffer from a lot of NA values due to misalignment of data sources on when data started
  merged_data = merged_data[3:nrow(merged_data),]
  merged_data = dplyr::select(merged_data, date,day,CivAttackFreq,aircraft:isOffensive)
  
  #ended up deciding not to use POW. Plot can be made for it, so add it back in if you want to see the plots here
  merged_data = merged_data[,-which(names(merged_data)=="POW")]
  
  #get rid of any data after last date of civilianHarm, I don't use any information past the last date of that column
  max_date_index <- which(merged_data$date==max_date)
  merged_data = merged_data[1:max_date_index,]
  
  return(merged_data)
}

# This function is structured in a way that works with the function argument for rollapply().
# This function specifically to be used in the rollapply() within the createBoolDataOnThreshold function. 
# It checks whether the last value in the data is above the mean plus the threshold times the standard deviation
is_increase_above_sd_threshod = function (data, threshold,threshold_days,partial1) {
  if(any(is.na(data)))
  {
    #i'm torn about this, maybe i could try to figure out a way to have the first days look forward instead of back? That seems like a lot of work for just the beginning of the data though
    #for now i've decided the first 15 days can go even if partial is declared, but i'm keeping anything past that
    if(partial1)
    {
      data = na.omit(data)
      if(length(data)<15)
      {
        return(NA)
      }
    }
  }
  if(length(data)>=15 || length(data)>=threshold_days)
  {
    m = mean(data) # calculate the mean of the data
    sd = sd(data)
    if(data[length(data)] >= m+threshold*sd) # check if the last value in the data is greater than the threshold
    {
      return(TRUE) # return true if it is greater than the threshold
    }
    else
    {
      return(FALSE) # return false if it is not greater than the threshold
    }
  }
  else
  {
    return(NA)
  }
}

# This function outputs the a copy of the data provided, but alters all columns not included in the "columns_to_exclude" argument.
createBoolDataOnThreshold = function(data,moving_average_days,threshold_sd,columns_to_exclude=NULL,partial=TRUE)
{
  result = data # initialize result with the input data
  for(column in names(data)) # loop through each column in the data
  {
    if (!(column %in% columns_to_exclude)) # check if the column should be excluded
    {
      column_index = which(names(data) == column) # get the index of the current column
      result[,column_index] = zoo::rollapply(result[,column_index], width=moving_average_days, FUN=is_increase_above_sd_threshod, threshold=threshold_sd,threshold_days=moving_average_days,align="right", fill=NA,partial=partial,partial1=partial) # apply the is_increase_above_sd_threshod function to the current column for a specified number of days
    }
  }
  if(moving_average_days > 15)
  {
    result = result[15:nrow(result),] # if the days are more than 15 I make a local moving average for those days, so too much data isn't lost.
  }
  else
  {
    result = result[moving_average_days:nrow(result),]
  }
  return(result) # return the boolean data
}

# This function is structured in a way that works with the function argument for rollapply().
# This function specifically to be used in the rollapply() within the checkIfAtLeastOneTrue() function. 
# This function checks whether a given row contains at least one non-NA and true value
check_true_sub_function = function(data, columns_to_exclude=NULL)
{
  for (row in data) # loop through each row in the data
  {
    if(!is.na(row) && row) # check if the current row is not NA and contains a true value
    {
      return(TRUE) # return true if the condition is met
    } 
  }
  return(FALSE) # return false if the condition is not met
}

# This function checks if at least one true value exists in the specified columns for a given number of days
checkIfAtLeastOneTrue = function(data,threshold_days,columns_to_exclude=NULL)
{
  result = data # initialize result with the input data
  for(column in names(data)) # loop through each column in the data
  {
    if (!(column %in% columns_to_exclude)) # check if the column should be excluded
    {
      column_index = which(names(data) == column) # get the index of the current column
      deviation = sd(data[,column_index]) # calculate the standard deviation of the current column
      result[,column_index] = rollapply(result[,column_index], width=threshold_days, FUN=check_true_sub_function, align="right", fill=NA) # apply the check_true_sub_function to the current column for a specified number of days
    }
  }
  result = result[threshold_days:nrow(result),] # remove the first threshold_days rows from the result
  return(result) # return the result
}

addMajorDateLines = function()
{
  abline(v = 45,col="red") #2022-04-09 end of initial invasion day 45 #fix later to bring back now that you got rid of days, maybe add days back in
  abline(v=187,col="red") #2022-08-29 start of counteroffensive day 187
  abline(v=262,col="red") #	2022-11-12 end of counteroffensive day 262
}

countMajorLosses = function(data){
  row_sums <- rowSums(data == TRUE)
  return (row_sums)
}

makeMajorLoss = function(num_threshold,data){
  row_sums <- rowSums(data == TRUE)
  return (row_sums>num_threshold)
}

get_didBigSpikeOccurOverLastWeek = function(threshold_sd, threshold_days, moving_average_days,majorLossThreshold,data)
{
  data_bool <- createBoolDataOnThreshold(data, moving_average_days = moving_average_days,
                                         threshold_sd = threshold_sd, columns_to_exclude = c("date","day","isOffensive","majorEvent"))
  didBigSpikeOccurOverLastWeek <- checkIfAtLeastOneTrue(data_bool, threshold_days, c("date","day","isOffensive","CivAttackFreq"))
  didBigSpikeOccurOverLastWeek$MajorLossCount = countMajorLosses(didBigSpikeOccurOverLastWeek[setdiff(names(data_bool),  c("date", "day", "CivAttackFreq", "isOffensive"))])
  didBigSpikeOccurOverLastWeek$MajorLoss = didBigSpikeOccurOverLastWeek$MajorLossCount > majorLossThreshold
  didBigSpikeOccurOverLastWeek <- didBigSpikeOccurOverLastWeek[,-1:-2] # remove date and day
  return(didBigSpikeOccurOverLastWeek)
}

# Define a function that returns the AIC or BIC given arguments. Will return the model if no metric is specified
get_model_or_metric <- function(type,threshold_sd, threshold_days, moving_average_days,majorLossThreshold,minNumberOfCivAttacks,data,metric="None") {
 
  didBigSpikeOccurOverLastWeek = get_didBigSpikeOccurOverLastWeek(threshold_sd, threshold_days, moving_average_days,majorLossThreshold,data)
  if(sum(didBigSpikeOccurOverLastWeek$CivAttackFreq) < minNumberOfCivAttacks)
  {
    #if there isn't enough TRUE values for civilian spikes, we aren't interested in that model
    return(Inf)
  }
    if(type=="step")
    {
      tryCatch({
        null = capture.output(glm_model <- step(glm(as.integer(CivAttackFreq) ~ ., data = didBigSpikeOccurOverLastWeek,family = "binomial"), direction = "both", trace = 0, silent=TRUE))
      }, error = function(e) {
        message(paste0("Error in step(glm(CivAttackFreq ~ ., data = didBigSpikeOccurOverLastWeek)): ", e$message))
        return(Inf)})
    } else if( type =="lasso")
    {
      tryCatch({
        #check if big loss occurred over threshold of days
        cv.fit <- cv.glmnet(as.matrix(didBigSpikeOccurOverLastWeek[, !names(didBigSpikeOccurOverLastWeek) %in% c("date", "day","CivAttackFreq")]),didBigSpikeOccurOverLastWeek[["CivAttackFreq"]], family="binomial",alpha = 1)
        best.lambda <- cv.fit$lambda.min
        
        # Fit LASSO model with the best lambda value
        lasso.fit <- glmnet(as.matrix(didBigSpikeOccurOverLastWeek[, !names(didBigSpikeOccurOverLastWeek) %in% c("date", "day","CivAttackFreq")]),didBigSpikeOccurOverLastWeek[["CivAttackFreq"]],family="binomial", alpha = 1, lambda = best.lambda)
        selected = row.names(coef(lasso.fit,s=best.lambda))[which(coef(lasso.fit, s = best.lambda) != 0)]
      }, error = function(e) {
        message(paste0("Error in generating Lasso fit: ", e$message))
        return(Inf)})
      tryCatch({
        # Fit the final model with the only the selected variables used in the LASSO regression
        if(length(selected)!=1)
        {
          glm_model = glm(CivAttackFreq ~ ., data = didBigSpikeOccurOverLastWeek[, c("CivAttackFreq", selected[-1])],family="binomial")
        } else {
          glm_model = glm(CivAttackFreq ~ 1, data = didBigSpikeOccurOverLastWeek,family="binomial")
        }
      }, error = function(e) {
        message(paste0("Error in generating binary logistic regression: ", e$message))
        return(Inf)})
    }
    if (metric=="aic")
    {
      return(AIC(glm_model))
    } else if(metric == "bic"){
      return(BIC(glm_model))
    }
    return(glm_model)

}


checkAssumptions = function(model)
{
  #glm of bianary logistic, and polr is for ordinal binary logistic
  #trafo didn't work very well for my purposes.
  #major assumptions of binary logistic regression:
  #1) the response variable is always TRUE or FALSE ---> this is true for my data
  #2) There should exist no outliers in the data
  #3) There should not be a high degree of multicollinearity amoung the different explantory variables, this can be determined with Jaccard coefficient.
  
  
  #assess if there are outliers
  try(predicted_labels <- predict(model))
  
  print(outlierTest(model))
  cooks_d <- cooks.distance(model)
  high_leverage = cooks_d > 4/nrow(predicted_labels)
  if(length(high_leverage)==0)
  {
    cat("\nno outliers with high leverage found\n")
  } else
  {
    cat("outliers with high leverage:\n")
    print(model$model[which(high_leverage)])
  }
  
  
  #given the very low preportion of True to False, VIF does not have to be as high to indicate multi-collinearity as it normally would. However, these values are still low and indicate no multi-collinearity
  tryCatch({
    cat("VIF\n")
    vif(model)
  }, error = function(e) {
    message("Error calculating VIF: the model contains fewer than 2 terms. There can't be multi-collinearity")
  })
}


get_all_models = function(type,metric, total_step_calls, threshold_sd_range, threshold_days_range, moving_average_days_range, majorLossThreshold_range, minNumberOfCivAttacks, data) {
  if( type == "lasso"||type=="step")
  {
    if (metric == "aic" || metric == "bic") {
      start = Sys.time()
      # Set up parallel backend and register it with foreach
      #it wasn't worth the time trying to figure out how to get a progress % setup for a multithread, so i just output a very rough estimate of time      estimatedMinutes = floor(total_step_calls*0.08081424801 / 60)
      estimatedSeconds = round((total_step_calls*0.08081424801) %% 60)
      if(!(total_step_calls>1))
      {
        cat("Only 1 set of arguments passed for calculations. You need to have total_step_calls be at least 2.\n")
      }
      if(estimatedMinutes==0)
      {
        cat("rough Estimate on Time for Calculation: ",estimatedSeconds, "Seconds", " + 8 seconds for initializing\n")
      } else {
        cat("rough Estimate on Time for Calculation: ",estimatedMinutes, " minutes and ", estimatedSeconds+8, " seconds\n")
      }
      
      cl <- makeCluster(detectCores())
      registerDoParallel(cl)
      
      null = capture.output(clusterEvalQ(cl, {
        library(memoise)
        library(doParallel)
        library(foreach)
        library(ggplot2)
        library(dplyr)
        library(changepoint)
        library(tidyr)
        library(MASS)
        library(olsrr)
        library(car)
        library(gvlma)
        library(sjPlot)
        library(generalhoslem)
        library(glmnet)
        library(corrplot)
        source("practicalAssignmentCustomTools.R")
      }))
      
      
      # Loop through all combinations of parameter values and find the one with the lowest AIC/BIC value
      #results should be combined from all the threads after it is finished and should have a list of the parameters and AIC/BIC for each iteration after the cluster is done
      step_calls <- expand.grid(threshold_sd_range, threshold_days_range, moving_average_days_range,majorLossThreshold_range)
      step_calls <- step_calls[step_calls[, 2] < step_calls[, 3], ] #not particularly necessary if you are setting the ranges correctly. This isn't computationally necessary, but this is a case that shouldn't happen. The threshold of days should be relatively low compared to the days used to calculate the moving average.
      results <- foreach(i = 1:nrow(step_calls), .combine = rbind) %dopar% {
        metric_value <- get_model_or_metric(type,step_calls[i, 1], step_calls[i, 2], step_calls[i, 3],step_calls[i, 4],minNumberOfCivAttacks, data,metric)
        c(step_calls[i, ], metric_value) #this function will return INF if the model has less than a set number of TRUE values for civAttack
      }
      colnames(results) <- c("threshold_sd", "threshold_days", "moving_average_days","majorLossThreshold", metric)
      write.csv(results, sprintf("results%s%s%s",type,toupper(metric),".csv"), row.names = FALSE)
      stopCluster(cl)
      end = Sys.time()
      time_to_complete = as.numeric(end-start)
      minutes = floor(time_to_complete / 60)
      seconds = round(time_to_complete %% 60)
      cat("Time Taken for Calculation: ", minutes, " minutes and ", seconds, " seconds\n")
      return(results)
    } else {
    cat("Please specify whether to run BIC or AIC.\n")
    }
  }else {
    cat("Please specify whether to run step or lasso.\n")
  }
}
