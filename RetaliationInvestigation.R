
#initialize by importing/installing libraries and data
source("RetaliationInvestigationCustomTools.R"); source("RetaliationInvestigationCustomTools.R") ##bug: doesn't show the functions in my environment in Rstudio unless i run the command twice
initialize()
data = importData()
cuml_data = getCumulativeData()
attach(data,warn.conflicts = FALSE) #important that you only attach this after you import both sets of data, many columns shared by Cumulative, and R doesn't keep data variable local to this file

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

#Hyperparameters for searching for best regressions

threshold_sd_range <- seq(1, 3, by = 0.1)
threshold_days_range <- seq(5, 10, by = 1)
moving_average_days_range <- seq(10,35, by = 5)
majorLossThreshold_range <- seq(1,3, by = 1)
total_step_calls = length(threshold_sd_range)*length(threshold_days_range)*length(moving_average_days_range)*length(majorLossThreshold_range)
minNumberOfCivAttacks = 15  #if you don't set this higher than 0 (good min is at least 30), the automation will make the response variable completely false, making the intercept a perfect predictor.
runStepBIC = TRUE  #flag that determines if the long process of performing step regressions on all combinations of hyper parameters. If set to FALSE, loads results from backup. 
runStepAIC = TRUE  #flag that determines if the long process of performing step regressions on all combinations of hyper parameters. If set to FALSE, loads results from backup. 
runLassoBIC = TRUE
runLassoAIC = TRUE

#get the best model based on aic and hyperparameter range with stepwise
if(runStepBIC)
{
  resultsStepBIC = get_all_models("step",
                                                  "bic",
                                                  total_step_calls,
                                                  threshold_sd_range,
                                                  threshold_days_range,
                                                  moving_average_days_range,
                                                  majorLossThreshold_range,
                                                  minNumberOfCivAttacks,
                                                  data)
} else {
  resultsStepBIC <- read.csv("resultsstepBIC.csv")
}
best_result_step_BIC <- resultsStepBIC[which.min(as.numeric(resultsStepBIC[,'bic'])),]
finalModelStepBIC = get_model_or_metric("step",
                                    best_result_step_BIC$threshold_sd,
                                    best_result_step_BIC$threshold_days,
                                    best_result_step_BIC$moving_average_days,
                                    best_result_step_BIC$majorLossThreshold,
                                    minNumberOfCivAttacks,
                                    data)



#get the best model based on aic and hyperparameter range with stepwise
if(runStepAIC)
{
  resultsStepAIC = get_all_models("step",
                                  "aic",
                                  total_step_calls,
                                  threshold_sd_range,
                                  threshold_days_range,
                                  moving_average_days_range,
                                  majorLossThreshold_range,
                                  minNumberOfCivAttacks,
                                  data)
} else {
  resultsStepAIC <- read.csv("resultsstepAIC.csv")
}
best_result_step_AIC <- resultsStepAIC[which.min(as.numeric(resultsStepAIC[,'aic'])),]
finalModelStepAIC = get_model_or_metric("step",
                                    best_result_step_AIC$threshold_sd,
                                    best_result_step_AIC$threshold_days,
                                    best_result_step_AIC$moving_average_days,
                                    best_result_step_AIC$majorLossThreshold,
                                    minNumberOfCivAttacks,
                                    data)


#get the best model based on aic and hyperparameter range with lasso
if(runLassoBIC)
{
  resultsLassoBIC = get_all_models("lasso",
                                    "bic",
                                    total_step_calls,
                                    threshold_sd_range,
                                    threshold_days_range,
                                    moving_average_days_range,
                                    majorLossThreshold_range,
                                    minNumberOfCivAttacks,
                                    data)
} else {
  resultsLassoBIC <- read.csv("resultslassoBIC.csv")
}
best_result_lasso_BIC <- resultsLassoBIC[which.min(as.numeric(resultsLassoBIC[,'bic'])),]
finalModelLassoBIC = get_model_or_metric("lasso",
                                          best_result_lasso_BIC$threshold_sd,
                                          best_result_lasso_BIC$threshold_days,
                                          best_result_lasso_BIC$moving_average_days,
                                          best_result_lasso_BIC$majorLossThreshold,
                                          minNumberOfCivAttacks,
                                          data
                                         )



#get the best model based on aic and hyperparameter range with lasso
if(runLassoAIC)
{
  resultsLassoAIC = get_all_models("lasso",
                                   "aic",
                                   total_step_calls,
                                   threshold_sd_range,
                                   threshold_days_range,
                                   moving_average_days_range,
                                   majorLossThreshold_range,
                                   minNumberOfCivAttacks,
                                   data)
  } else {
  resultsLassoAIC <- read.csv("resultslassoAIC.csv")
}
best_result_lasso_AIC <- resultsLassoAIC[which.min(as.numeric(resultsLassoAIC[,'aic'])),]
finalModelLassoAIC = get_model_or_metric("lasso",
                                    best_result_lasso_AIC$threshold_sd,
                                    best_result_lasso_AIC$threshold_days,
                                    best_result_lasso_AIC$moving_average_days,
                                    best_result_lasso_AIC$majorLossThreshold,
                                    minNumberOfCivAttacks,
                                    data)




############## manual models ####################
#this wasn't quite adequate to find the best pattern. There were too many possible hyperparameter combinations that a potential pattern could exist in.
#create Big loss variables
threshold_sd = 1.9 #number of standard deviations above moving average change that indicates a big loss for the day
moving_average_days =  35 #number of days to include in the moving average for the standard deviation
threshold_days = 7 #number of days until the 
majorLossThreshold = 2

#check if big loss occurred over threshold of days
didBigSpikeOccurOverLastWeek = get_didBigSpikeOccurOverLastWeek(threshold_sd, threshold_days, moving_average_days,majorLossThreshold,data)


model1 = glm(CivAttackFreq~majorEvent,data=didBigSpikeOccurOverLastWeek,family="binomial")
model2 = glm(CivAttackFreq~MajorLossCount,data=didBigSpikeOccurOverLastWeek,family="binomial")
model3 = glm(CivAttackFreq~MajorLoss,data=didBigSpikeOccurOverLastWeek,family="binomial")
model4 = glm(CivAttackFreq~majorEvent+MajorLossCount,data=didBigSpikeOccurOverLastWeek,family="binomial")
model5 = glm(CivAttackFreq~majorEvent+MajorLoss,data=didBigSpikeOccurOverLastWeek,family="binomial")

###############final summaries#############
cat("\n\tStepwise generated binary logistic regression models made with automatically selected hyperparemeters. Not discussed in paper due to lack of multi-collinearity check.\n")
checkAssumptions(finalModelStepAIC)
summary(finalModelStepAIC)
checkAssumptions(finalModelStepBIC)
summary(finalModelStepBIC)
cat("\n\t Manually generated final models.\n")
cat("\tFINAL MANUAL MODEL 1")
checkAssumptions(model1)
print(summary(model1))
cat("\n\tFINAL MANUAL MODEL 2")
checkAssumptions(model2)
print(summary(model2))
cat("\n\tFINAL MANUAL MODEL 3")
checkAssumptions(model3)
print(summary(model3))
cat("\n\tFINAL MANUAL MODEL 4")
checkAssumptions(model4)
print(summary(model4))
cat("\n\tFINAL MANUAL MODEL 5")
checkAssumptions(model5)
print(summary(model5))
cat("\nBinary logistic regression final models made with variable selection by LASSO with automatically tuned hyperparametrers.\n")
cat("Final Model Lasso BIC based:",
    "\n\tStd devs indication of spike: ",best_result_lasso_BIC$threshold_sd,
    "\n\tLag days to calculate the average and std dev for spikes: ",best_result_lasso_BIC$moving_average_days,
    "\n\tDays looked backwards to check if a major spike occurred: ",best_result_lasso_BIC$threshold_days,
    "\n\tNumber of simulataneous spikes accross all explanatory variables to determine the major event variable: ",best_result_lasso_BIC$majorLossThreshold,
    "\n\tMinimum number of spikes necessary in Civilian Attack Frequency for an acceptable model: ",minNumberOfCivAttacks)
checkAssumptions(finalModelLassoBIC)
print(summary(finalModelLassoBIC))
cat("Final Model Lasso AIC based:",
    "\n\tStd devs indication of spike: ",best_result_lasso_AIC$threshold_sd,
    "\n\tLag days to calculate the average and std dev for spikes: ",best_result_lasso_AIC$moving_average_days,
    "\n\tDays looked backwards to check if a major spike occurred: ",best_result_lasso_AIC$threshold_days,
    "\n\tNumber of simulataneous spikes accross all explanatory variables to determine the major event variable: ",best_result_lasso_AIC$majorLossThreshold,
    "\n\tMinimum number of spikes necessary in Civilian Attack Frequency for an acceptable model: ",minNumberOfCivAttacks)
checkAssumptions(finalModelLassoAIC)
print(summary(finalModelLassoAIC))
print("confidence intervals for odds ratios of coefficients")
print(exp(confint.default(finalModelLassoAIC)))
