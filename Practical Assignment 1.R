#Is there a significant pattern of Russian retaliation towards Ukrainian civilians after a spike in Russian Losses or a significant Ukrainian success?

#initialize by importing/installing libraries and data
source("practicalAssignmentCustomTools.R"); source("practicalAssignmentCustomTools.R") ##bug: doesn't show the functions in my enviornment in Rstudio unless i run the command twice
initialize()
data = importData()
cuml_data = getCumulativeData()
attach(data) #important that you only attach this after you import both sets of data, many columns shared by Cumulative, and R doesn't keep data variable local to this file
runBIC = TRUE #flag that determines if the long process of performing step regressions on all combinations of hyperparameters. If set to FALSE, loads results from backup. 
runAIC = TRUE #flag that determines if the long process of performing step regressions on all combinations of hyperparameters. If set to FALSE, loads results from backup. 

threshold_sd_range <- seq(2, 4, by = 0.1)
threshold_days_range <- seq(7, 14, by = 1)
moving_average_days_range <- seq(25,35, by = 5)
total_step_calls = length(threshold_sd_range)*length(threshold_days_range)*length(moving_average_days_range)

# 
# ######## plots##############
# # set up plot with four panels for the quantitative variables
p_list <- list()
for (col in colnames(data)[-1:-2][-12:-13]) {
  null = capture.output(p <- ggplot(data, aes(date, !!sym(col))) +
    geom_col(fill = "black") +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    labs(title = ifelse(col == "civattackfreq" | col == "pow",
                        ifelse(col == "pow", "pows per day", "civattack per day"),
                        sprintf("loss per day of %s", col))) +
    scale_x_datetime(date_labels = ifelse(col == "pow","%d %b %y" ,"%b %y"), breaks = ifelse(col == "pow",date_breaks("3 weeks") ,date_breaks("6 months"))) +
    geom_smooth(formula="y~x",method = "loess", colour = "blue",alpha=0.7, level = 0.99) +
    theme(text = element_text(size = 15)) +
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-04-09")), color = "red") + # 2022-04-09 end of initial invasion day 45
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-08-29")), color = "red") + # 2022-08-29 start of counteroffensive day 187
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-11-12")), color = "red") +  # 2022-11-12 end of counteroffensive day 262
    theme(plot.title = element_text(hjust = 0.5,size=15))+
    xlab("") +
    ylab(""))
  p_list[[col]] <- p
}
output_list <- list()
for (i in seq(1, length(p_list), 4)) {
  if (length(p_list) - i >= 3) {
    suppressWarnings(output_list[[length(output_list) + 1]] <- grid.arrange(grobs = p_list[i:(i + 3)], nrow = 2, ncol = 2))
  } else {
    suppressWarnings(output_list[[length(output_list) + 1]] <- grid.arrange(grobs = p_list[i:length(p_list)], nrow = 2, ncol = 2))
  }
}


#now for the cumulative data
p_list <- list()
for (col in colnames(cuml_data)[-1:-2][-12:-13]) {
  null = capture.output(p <- ggplot(cuml_data, aes(cuml_data$date, !!sym(col))) +
    geom_line(fill = "black") +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    labs(title = ifelse(col == "civharmkfreqcumulative" | col == "pow",
                        ifelse(col == "pow", "pows cumulative", "civattacks cumulative"),
                        sprintf("cumulative loss of %s", col))) +
    scale_x_datetime(date_labels = ifelse(col == "pow","%d %b %y" ,"%b %y"), breaks = ifelse(col == "pow",date_breaks("6 months") ,date_breaks("6 months"))) +
    theme(text = element_text(size = 15)) +
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-04-09")), color = "red") + # 2022-04-09 end of initial invasion day 45
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-08-29")), color = "red") + # 2022-08-29 start of counteroffensive day 187
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-11-12")), color = "red") +  # 2022-11-12 end of counteroffensive day 262
    theme(plot.title = element_text(hjust = 0.5,size=15))+
    xlab("") +
    ylab(""))
  p_list[[col]] <- p
}
output_list <- list()
for (i in seq(1, length(p_list), 4)) {
  if (length(p_list) - i >= 3) {
    suppressWarnings(output_list[[length(output_list) + 1]] <- grid.arrange(grobs = p_list[i:(i + 3)], nrow = 2, ncol = 2))
  } else {
    suppressWarnings(output_list[[length(output_list) + 1]] <- grid.arrange(grobs = p_list[i:length(p_list)], nrow = 2, ncol = 2))
  }
}
########## exploring a changepoint analyis ################
# Define the list to store the plots


# Loop through each column of data
p_list <- list()
for (col in colnames(data)[-1:-2][-12:-13]) {
  
  # Identify changepoints
  suppressWarnings(cpt <- cpt.mean(data[[col]], method = "BinSeg", minseglen = 1))
  df = data.frame(date =data$date)
  df$means = rep(NA,nrow(df))
  indexes = c(1,cpt@cpts,length(data[[col]]))
  for (i in 1:length(indexes)) {
    # Calculate the mean of the values between the current index and the next index (or the end of the data)
    if (i < length(indexes)) {
      start_index <- indexes[i]
      end_index <- indexes[i + 1] - 1
    } else {
      start_index <- indexes[i]
      end_index <- length(data[[col]])
    }
    mean_value <- mean(data[[col]][start_index:end_index])
    # Set the values between the indexes to the mean
    df$means[start_index:end_index] <- mean_value
  }
  df[[col]]=data[[col]]
  # Create ggplot object
  null = capture.output(p <- ggplot(df, aes(date, !!sym(col))) +
    theme_classic() +
    geom_line() +
    geom_line(aes(date, means), color = "red",size=1) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    scale_x_datetime(date_labels = ifelse(col == "POW","%d %b %y" ,"%b %y"), breaks = ifelse(col == "POW",date_breaks("3 weeks") ,date_breaks("6 months"))) +
    labs(title=paste0("Change point analysis of ", col)) +
    theme(text = element_text(size = 15)) +
    theme(plot.title = element_text(hjust = 0.5,size=15)) +
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-04-09")), color = "blue",size=0.8) +
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-08-29")), color = "blue",size=0.8) +
    geom_vline(xintercept = as.numeric(as.POSIXct("2022-11-12")), color = "blue",size=0.8) +
    xlab("") +
    ylab(""))
  # Add the plot to the list
  p_list[[col]] <- p
}
output_list <- list()
for (i in seq(1, length(p_list), 4)) {
  if (length(p_list) - i >= 3) {
    suppressWarnings(output_list[[length(output_list) + 1]] <- grid.arrange(grobs = p_list[i:(i + 3)], nrow = 2, ncol = 2))
  } else {
    suppressWarnings(output_list[[length(output_list) + 1]] <- grid.arrange(grobs = p_list[i:length(p_list)], nrow = 2, ncol = 2))
  }
}

########## exploring regressions ################

#from the plots above, i saw i could not identify shocks from just using an average change. The variation changed over time. The mean from which the variation should be observed from also changed over time.
#I identified that major shifts in the offensives could be used to split up the data. However, this wasn't a perfect choice, it didn't line up perfectly with the offensive dates, and many other variables unaccounted for could be affecting variablity and mean over time. Furthermore, gradual changes in mean and variation won't be captured by a changepoint analysis. 
#A moving average and sd was more effective at identifying shocks in this case.

if (runBIC)
{
  # Set up parallel backend and register it with foreach
  #my 8700k got about 16.29 iterations done per minute. The parameters will change that, and CPUs are different etc.
  #it wasn't worth the time trying to figure out how to get a progress % setup for a multithread, so i just output a very rough estimate of time
  estimatedMinutes = floor(total_step_calls/16.293/60)
  if(!(total_step_calls>1))
  {
    cat("Only 1 set of arguments passed for calculations. You need to have total_step_calls be at least 2")
  }
  if(estimatedMinutes==0)
  {
    estimatedSeconds = floor(total_step_calls/16.293)
    cat("rough Estimate on Time for Calculation: ",estimatedSeconds, "Seconds")
  } else {
    cat("rough Estimate on Time for Calculation: ",estimatedMinutes, "Minutes")
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
    library(corrplot)
    source("practicalAssignmentCustomTools.R")
  }))
  
  
  # Loop through all combinations of parameter values and find the one with the lowest AIC/BIC value
  #results should be combined from all the threads after it is finished and should have a list of the parameters and AIC/BIC for each iteration after the cluster is done
  step_calls <- expand.grid(threshold_sd_range, threshold_days_range, moving_average_days_range)
  step_calls <- step_calls[step_calls[, 2] < step_calls[, 3], ] #not particularly necessary if you are setting the ranges correctly. This isn't computationally necessary, but this is a case that shouldn't happen. The threshold of days should be relatively low compared to the days used to calculate the moving average.
  resultsBIC <- foreach(i = 1:nrow(step_calls), .combine = rbind) %dopar% {
    bic <- get_bic(step_calls[i, 1], step_calls[i, 2], step_calls[i, 3], data)
    c(step_calls[i, ], bic)
  }
  colnames(resultsBIC) <- c("threshold_sd", "threshold_days", "moving_average_days", "bic")
  stopCluster(cl)
  write.csv(resultsBIC, "resultsBIC.csv", row.names = FALSE)
} else
{
  resultsBIC <- read.csv("resultsBIC.csv")
}
  
  
  # Find
  # Initialize variables to store the best parameters and the lowest AIC/BIC value found so far
  best_threshold_sd_BIC <- NA
  best_threshold_days_BIC <- NA
  best_moving_average_days_BIC <- NA
  lowest_bic <- Inf
  
  
  
  best_result_BIC <- resultsBIC[which.min(as.numeric(resultsBIC[,'bic'])),]
  best_threshold_sd_BIC <- best_result_BIC$threshold_sd
  best_threshold_days_BIC <- best_result_BIC$threshold_days
  best_moving_average_days_BIC <- best_result_BIC$moving_average_days
  lowest_bic <- best_result_BIC$bic
  
  
  #create Big loss variables
  threshold_sd = best_threshold_sd_BIC #number of standard deviations above moving average change that indicates a big loss for the day
  moving_average_days =  best_moving_average_days_BIC #number of days to include in the moving average for the standard deviation
  threshold_days = best_threshold_days_BIC #number of0 days until the 
  
  #check if big loss occurred over threshold of days
  data_bool = createBoolDataOnThreshold(data,moving_average_days=moving_average_days,threshold_sd = threshold_sd,columns_to_exclude = c("date","day","isOffensive","majorEvent"))
  didBigSpikeOccurOverLastWeek = checkIfAtLeastOneTrue(data_bool,threshold_days,c("date","day","isOffensive","CivAttackFreq"))
  didBigSpikeOccurOverLastWeek$MajorLossCount = countMajorLosses(didBigSpikeOccurOverLastWeek[setdiff(names(data_bool),  c("date", "day", "CivAttackFreq", "isOffensive"))])
  didBigSpikeOccurOverLastWeek = didBigSpikeOccurOverLastWeek[,-1:-2]
  
  finalModelBIC = step(glm(CivAttackFreq~.,data=didBigSpikeOccurOverLastWeek),trace=0,silent=TRUE)


##########do it all again for AIC #############
if(runAIC)
{
  # Set up parallel backend and register it with foreach
  #my 8700k got about 16.29 iterations done per minute. The parameters will change that, and CPUs are different etc.
  #it wasn't worth the time trying to figure out how to get a progress % setup for a multithread, so i just output a very rough estimate of time
  estimatedMinutes = floor(total_step_calls/16.293/60)
  if(!(total_step_calls>1))
  {
    cat("Only 1 set of arguments passed for calculations. You need to have total_step_calls be at least 2")
  }
  if(estimatedMinutes==0)
  {
    estimatedSeconds = floor(total_step_calls/16.293)
    cat("rough Estimate on Time for Calculation: ",estimatedSeconds, "Seconds")
  } else {
    cat("rough Estimate on Time for Calculation: ",estimatedMinutes, "Minutes")
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
    library(corrplot)
    source("practicalAssignmentCustomTools.R")
  }))
  
  
  # Loop through all combinations of parameter values and find the one with the lowest AIC/BIC value
  #results should be combined from all the threads after it is finished and should have a list of the parameters and AIC/BIC for each iteration after the cluster is done
  step_calls <- expand.grid(threshold_sd_range, threshold_days_range, moving_average_days_range)
  step_calls <- step_calls[step_calls[, 2] < step_calls[, 3], ] #not particularly necessary if you are setting the ranges correctly. This isn't computationally necessary, but this is a case that shouldn't happen. The threshold of days should be relatively low compared to the days used to calculate the moving average.
  resultsAIC <- foreach(i = 1:nrow(step_calls), .combine = rbind) %dopar% {
    aic <- get_aic(step_calls[i, 1], step_calls[i, 2], step_calls[i, 3], data)
    c(step_calls[i, ], aic)
  }
  colnames(resultsAIC) <- c("threshold_sd", "threshold_days", "moving_average_days", "aic")
  stopCluster(cl)
  write.csv(resultsBIC, "resultsAIC.csv", row.names = FALSE)
} else
{
  resultsAIC <- read.csv("resultsAIC.csv")
}


# Find
# Initialize variables to store the best parameters and the lowest AIC value found so far
best_threshold_sd_AIC <- NA
best_threshold_days_AIC <- NA
best_moving_average_days_AIC <- NA
lowest_aic <- Inf



best_result_AIC <-  resultsAIC[which.min(as.numeric(resultsAIC[,'aic'])),]
best_threshold_sd_AIC <- best_result_AIC$threshold_sd
best_threshold_days_AIC <- best_result_AIC$threshold_days
best_moving_average_days_AIC <- best_result_AIC$moving_average_days
lowest_aic <- best_result_AIC$aic


#create Big loss variables
threshold_sd = best_threshold_sd_AIC #number of standard deviations above moving average change that indicates a big loss for the day
moving_average_days =  best_moving_average_days_AIC #number of days to include in the moving average for the standard deviation
threshold_days = best_threshold_days_AIC #number of days until the 

#check if big loss occurred over threshold of days
data_bool = createBoolDataOnThreshold(data,moving_average_days=moving_average_days,threshold_sd = threshold_sd,columns_to_exclude = c("date","day","isOffensive","majorEvent"))
didBigSpikeOccurOverLastWeek = checkIfAtLeastOneTrue(data_bool,threshold_days,c("date","day","isOffensive","CivAttackFreq"))
didBigSpikeOccurOverLastWeek$MajorLossCount = countMajorLosses(didBigSpikeOccurOverLastWeek[setdiff(names(data_bool),  c("date", "day", "CivAttackFreq", "isOffensive"))])
didBigSpikeOccurOverLastWeek = didBigSpikeOccurOverLastWeek[,-1:-2]

finalModelAIC = step(glm(CivAttackFreq~.,data=didBigSpikeOccurOverLastWeek),trace=0,silent=TRUE)




#I want booleans here, because i'm not trying to predict civilian casualties with quantitative numbers.
#I want to identify if a shock ("a big loss or negative major event") to Russia results in 
#a shock increase to civilian attacks in the next week. Thus, I am converting my data to booleans to check if a big change occurred.
#The value is True for explanatory variables (except isOffensive) if a big change (set by threshold_sd around moving average) occurred over the last week (or threshold_days).


#glm of bianary logistic, and polr is for ordinal binary logistic
#trafo didn't work very well for my purposes.
#major assumptions of binary logistic regression:
  #1) the response variable is always TRUE or FALSE ---> this is true for my data
  #2) There should exist no outliers in the data
  #3) There should not be a high correlation or multicollinearity amoung the different explantory variables.
  #4) by selecting randomly from my data for the test and training data and removing the date variables, i have fulfilled random selection.


#assess if there are outliers
predicted_labels <- predict(finalModelBIC,data=didBigSpikeOccurOverLastWeek)
outlierTest(finalModelBIC)
cooks_d <- cooks.distance(finalModelBIC)
high_leverage = cooks_d > 4/nrow(didBigSpikeOccurOverLastWeek)
didBigSpikeOccurOverLastWeek[which(high_leverage), ]

#the points above have a lot of leverage, but they are not in fact outliers in this case. 
#There are few instances of naval.ships having significant losses in the last week and the fact that they 
#line up with civAttackFrequency is notable, and should have a large impact on my model


#given the very low preportion of True to False, VIF does not have to be as high to indicate multi-collinearity as it normally would. However, these values are still low and indicate no multi-collinearity
#vif(finalModelBIC)

# Compute correlation matrix
# correlation matrix also does not show much multi-collinearity

#FAILING HERE BUT STILL GOT THROUGH MOST OF THE STUFF :)
#cor_matrix <- cor((didBigSpikeOccurOverLastWeek),method = "pearson")
#melted_cor <- melt(cor_matrix)
#corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)



######other types of model generation, but not my final model ########


#lasso reduces the coefficient of less important variables. It also gets rid of a few non-important features. Fine for predicting the civilian attack numbers, but not good with the point of finding if rretaliation can be seen in the data.
#just quick exploration into maybe just predicitng the number of civilian attacks based on the losses for that day
# Perform cross-validation to select the best lambda value
cv.fit <- cv.glmnet(as.matrix(data[, !names(data) %in% c("date", "day","CivAttackFreq")]),data[["CivAttackFreq"]], alpha = 1)
best.lambda <- cv.fit$lambda.min

# Fit LASSO model with the best lambda value
lasso.fit <- glmnet(as.matrix(data[, !names(data) %in% c("date", "day","CivAttackFreq")]),data[["CivAttackFreq"]], alpha = 1, lambda = best.lambda)
coef(lasso.fit,s=best.lambda)
selected = row.names(coef(lasso.fit,s=best.lambda))
# Fit the final model with the selected variables
finalModel2 <- lm(CivAttackFreq ~ ., data = data[, c("CivAttackFreq", selected[-1])])

############## manual model ####################

#create Big loss variables
threshold_sd = 2 #number of standard deviations above moving average change that indicates a big loss for the day
moving_average_days =  30 #number of days to include in the moving average for the standard deviation
threshold_days = 7 #number of days until the 

#check if big loss occurred over threshold of days
data_bool = createBoolDataOnThreshold(data,moving_average_days=moving_average_days,threshold_sd = threshold_sd,columns_to_exclude = c("date","day","isOffensive","majorEvent"))
didBigSpikeOccurOverLastWeek = checkIfAtLeastOneTrue(data_bool,threshold_days,c("date","day","isOffensive","CivAttackFreq"))
didBigSpikeOccurOverLastWeek$MajorLossCount = countMajorLosses(didBigSpikeOccurOverLastWeek[setdiff(names(data_bool),  c("date", "day", "CivAttackFreq", "isOffensive"))])
didBigSpikeOccurOverLastWeek = didBigSpikeOccurOverLastWeek[,-1:-2]

model1 = glm(CivAttackFreq~majorEvent+naval.ship,data=didBigSpikeOccurOverLastWeek)
model2 = glm(CivAttackFreq~MajorLossCount+naval.ship,data=didBigSpikeOccurOverLastWeek)
model3 = glm(CivAttackFreq~majorEvent+MajorLossCount+naval.ship,data=didBigSpikeOccurOverLastWeek)
model4 = glm(CivAttackFreq~majorEvent+MajorLossCount,data=didBigSpikeOccurOverLastWeek)



###############final summaries##############
summary(mdoel1)
summary(mdoel2)
summary(mdoel3)
summary(mdoel4)
summary(finalModel2)
summary(finalModelAIC)
summary(finalModelBIC)

#chat-gpt make confusion matrix, use hosmer-lemeshow goodness of fit test, anova test, xtabs

#chat-gpt make a confusion maktrix for finalModelAIC here

#make recent major loss variable!!!  by a threshold of enough major losses accross other variables of interest
#interpret as having a recent event makes you __% more likely to have an increase in a spike in civilian attacks that day


#notes:
#given i made the major event list, it is too easy for me to wiggle around the events until i find one that is statistically significant.
#it would be nice if I could find some data that can help set a benchmark for determining what is considered a major event.


#Random forests are not good fit here. Although I explored them. There is no 
#test to determine if an explanatory variable is statistically significant in a random forest that i am aware of
#While I could potentially show that i am able to predict whether there will be an increases in civilian attacks,
#I don't want to just predict, but determine if there is a statistically significant relationship./