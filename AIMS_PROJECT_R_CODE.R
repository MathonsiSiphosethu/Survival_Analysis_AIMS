# Load necessary libraries
rm(list = ls())
library(haven)
library(dplyr)
library(ggplot2)
library(lubridate)
library(survival)
library(survminer)
library(tidyr)
library(readr)
library(foreign)
library(discSurv)
library(mgcv)
library(extrafont)
library(gamlss)
library(rcompanion)
library(ggpubr)
library(multcomp)
library(knitr)
library(janitor)
library(tibble)
library(table1)
library(survAUC) 
library(survival)
library(rms)
options(scipen = 999) # Prevent scientific notation
# Load data
load("/var/autofs/misc/home/siphosethu/Documents/AIMS project_ Siphosethu/Data/survdat.rdata")
all5y <- dat
unique(all5y$wealthHH)
summary(all5y$wealthHH)
str(all5y$wealthHH)
# Remove rows with missing data
#all5y <- na.omit(all5y)
all5y$Period[all5y$period<= 2006]<-"P2002-2006"
all5y$Period[all5y$period> 2006 & dat$period<=2011]<-"P2007-2011"
all5y$Period[all5y$period> 2011]<-"P2012-2018"


table(all5y$Period)
colnames(all5y)[which(names(all5y) == "bcg")] <- "BCG_vaccination"
colnames(all5y)[which(names(all5y) == "igaMom")] <- "mom_engagement_inco"
colnames(all5y)[which(names(all5y) == "educMom")] <- "mom_education"


#unique(all5y$mom_education)

all5y$eventDeath <- factor(all5y$eventDeath, levels = c("0","1"), 
                               labels = c("Alive", "Dead"))
unique(all5y$wealthHH)


all5y$wealthHH <- factor(all5y$wealthHH, levels = c("Lowest","Highest","Middle","<NA>"), 
                                labels = c("Lowest","Highest","Middle","Lowest"))
all5y$wealthHH[is.na(all5y$wealthHH)] <- "Lowest"

all5y$BCG_vaccination <- factor(all5y$BCG_vaccination, levels = c("Yes","No","Dont know"), 
                           labels = c("Yes", "No","No"))


all5y$mom_engagement_inco <- factor(all5y$mom_engagement_inco, levels = c("Yes","No","Dont know"), 
                                labels = c("Yes", "No","Yes"))
all5y$mom_education <- factor(all5y$mom_education, levels = c("Primary","At least secondary","Dont know"), 
                                    labels = c("Primary", "At least secondary","At least secondary"))
all5y$mom_education[is.na(all5y$mom_education)] <- "At least secondary"

all5y$multiplebirth[is.na(all5y$multiplebirth)] <- "Multiple"


###################################################table 1 ####################################

table1(~Period|eventDeath,data = all5y)

table1(~momMaritalstatus +slumarea +genderChild +Period
        + mom_education + multiplebirth + mom_engagement_inco +
          BCG_vaccination|eventDeath,data = all5y,row.wise = TRUE)



#################################################################################################

#################################################################################################
all5y$eventDeath
# Reshape the data into long format
all5y_long <- all5y %>%
  dplyr::select(agefirstBirth, ageHH, hhsize, ) %>%
  pivot_longer(cols = c(agefirstBirth, ageHH, hhsize), 
               names_to = "Variable", 
               values_to = "Value")



# Rename the variables to more descriptive labels
all5y_long$Variable <- recode(all5y_long$Variable,
                              "agefirstBirth" = "Age at First Birth",
                              "ageHH" = "Age of Household Head",
                              "hhsize" = "Household Size")

#########################################################Kaplan-Meier survival plot###############################

# Define the survival object
surv_obj <- Surv(time = all5y$Time, event = all5y$eventDeath)

# Kaplan-Meier survival curve
model1 <- survfit(surv_obj ~ 1, data = all5y)

# Set margins (if needed)
par(mar = c(5, 4, 5, 2) + 0.1)  # Adjust top margin to make space for the caption

# Plot the survival curve with darker colors, thicker lines, and larger axis font sizes
plot(model1,
     conf.int = TRUE, 
     xlab = "Time until death (in months)", 
     ylab = "Survival probability", 
     ylim = c(0.955, 1), 
     col = "red",  # Set the color of the main plot line to red
     lwd = 2,      # Set line width for the survival estimate
     cex.lab = 1.3,  # Increase axis label font size
     cex.axis = 1.5  # Increase axis tick mark font size
)

# Add a legend with larger text
legend("topright", 
       legend = c("Lower 95% CI", "Survival estimate", "Upper 95% CI"), 
       lty = c(2, 1, 2),  # line types for each item
       col = c("red", "red", "red"),  # color of lines
       lwd = 1.7,  # line width
       cex = 2)  # Increase legend text size

# Add a bold title with larger text
title(main = "Kaplan-Meier Survival Curve", 
      line = 1, 
      font.main = 2, 
      cex.main = 3)  # Increase title text size


#######################################################################################################333
km_slum <- survfit(surv_obj ~ all5y$slumarea, data = all5y)

# Plot with vertical line at 4 years (48 months)
ggsurv <- ggsurvplot(km_slum, 
                     data = all5y,
                     ylim = c(0.95, 1), 
                     title = "Kaplan-Meier Curve by Slum Area", 
                     xlab = "Age (in Months)", 
                     ylab = "Survival Probability", 
                     risk.table = FALSE, 
                     conf.int = TRUE,
                     )

# Add vertical line at 48 months (4 years)
ggsurv$plot <- ggsurv$plot + 
  geom_vline(xintercept = 36, linetype = "dashed", color = "red", size = 0.9) + 
  annotate("text", x = 38, y = 0.97, label = "3 Years", color = "red", angle = 90, vjust = -0.5)

# Display the plot
ggsurv

##############################################################################################33
# Plot with vertical line at 3 years (36 months)
ggsurv <- ggsurvplot(km_slum, 
                     data = all5y,
                     ylim = c(0.95, 1), 
                     title = "Kaplan-Meier Curve by Slum Area", 
                     xlab = "Age (in Months)", 
                     ylab = "Survival Probability", 
                     risk.table = FALSE, 
                     conf.int = TRUE)

# Add vertical line at 36 months (3 years)
ggsurv$plot <- ggsurv$plot + 
  geom_vline(xintercept = 36, linetype = "dashed", color = "red", size = 0.9) + 
  annotate("text", x = 38, y = 0.97, label = "3 Years", color = "red", angle = 90, vjust = -0.5) +
  
  # Adjust legend and title text size
  theme(legend.text = element_text(size = 20),    # Adjust legend text size
        legend.title = element_text(size = 20),   # Adjust legend title size
        plot.title = element_text(size = 20, hjust = 0.5))   # Adjust plot title size and center it

# Display the plot
ggsurv






##############################################################################################
#gender

km_slum <- survfit(surv_obj ~all5y$genderChild, data = all5y)
ggsurvplot(km_slum, data = all5y,ylim = c(0.95, 1), title = "Kaplan-Meier Curve by Gender", 
           xlab = "Age (in Months)", ylab = "Survival Probability", risk.table = FALSE)
########################################################comaring K-M Plots#############################################
#Mothers marituls status

km_slum <- survfit(surv_obj ~ all5y$momMaritalstatus, data = all5y)

# Plot with vertical line at 4 years (48 months)
ggsurv <- ggsurvplot(km_slum, 
                     data = all5y,
                     ylim = c(0.95, 1), 
                     conf.int = TRUE, 
                     title = "Kaplan-Meier Curve by Mother's Marital Status", 
                     xlab = "Age (in Months)", 
                     ylab = "Survival Probability",
                     risk.table = FALSE)

# Add vertical line at 48 months (4 years)
ggsurv$plot <- ggsurv$plot + 
  geom_vline(xintercept = 48, linetype = "dashed", color = "red", size = 0.8) + 
  annotate("text", x = 50, y = 0.96, label = "4 Years", color = "red", angle = 90, vjust = -0.5) +
  
  # Adjust legend and title text size
  theme(legend.text = element_text(size = 20),    # Adjust legend text size
        legend.title = element_text(size = 20),   # Adjust legend title size
        plot.title = element_text(size = 20, hjust = 0.5))   # Adjust plot title size and center it

# Display the plot
ggsurv


######################################################################################################33

# wealth
km_slum <- survfit(surv_obj ~ all5y$wealthHH, data = all5y)

# Plot with vertical line at 3 years (36 months)
ggsurv <- ggsurvplot(km_slum, 
                     data = all5y,
                     ylim = c(0.96, 1), 
                     title = "Kaplan-Meier Curve by Wealth Status", 
                     xlab = "Age (in Months)", 
                     ylab = "Survival Probability")

# Add vertical line at 36 months (3 years)
ggsurv$plot <- ggsurv$plot + 
  geom_vline(xintercept = 36, linetype = "dashed", color = "red", size = 0.8) + 
  annotate("text", x = 38, y = 0.965, label = "3 Years", color = "red", angle = 90, vjust = -0.5) +
  
  # Adjust legend and title text size
  theme(legend.text = element_text(size = 20),    # Adjust legend text size
        legend.title = element_text(size = 20),   # Adjust legend title size
        plot.title = element_text(size = 20, hjust = 0.5))   # Adjust plot title size and center it

# Display the plot
ggsurv

###################################################################################333
# BCG vaccination
km_slum <- survfit(surv_obj ~ all5y$BCG_vaccination, data = all5y)

# Plot with vertical line at 4 years (48 months)
ggsurv <- ggsurvplot(km_slum, 
                     data = all5y,
                     ylim = c(0.92, 1), 
                     conf.int = TRUE, 
                     title = "Kaplan-Meier Curve by BCG Vaccination", 
                     xlab = "Age (in Months)", 
                     ylab = "Survival Probability")

# Add vertical line at 48 months (4 years)
ggsurv$plot <- ggsurv$plot + 
  geom_vline(xintercept = 48, linetype = "dashed", color = "red", size = 0.8) + 
  annotate("text", x = 50, y = 0.93, label = "4 Years", color = "red", angle = 90, vjust = -0.5) +
  
  # Adjust legend and title text size
  theme(legend.text = element_text(size = 20),    # Adjust legend text size
        legend.title = element_text(size = 20),   # Adjust legend title size
        plot.title = element_text(size = 20, hjust = 0.5))   # Adjust plot title size and center it

# Display the plot
ggsurv

####################################################################################3
km_slum <- survfit(surv_obj ~ all5y$multiplebirth, data = all5y)

# Plot with vertical line at 3 years (36 months)
ggsurv <- ggsurvplot(km_slum, 
                     data = all5y,
                     ylim = c(0.90, 1), 
                     conf.int = TRUE, 
                     title = "Kaplan-Meier Curve by Multiple Births", 
                     xlab = "Age (in Months)", 
                     ylab = "Survival Probability")

# Add vertical line at 36 months (3 years)
ggsurv$plot <- ggsurv$plot + 
  geom_vline(xintercept = 36, linetype = "dashed", color = "red", size = 0.8) + 
  annotate("text", x = 38, y = 0.91, label = "3 Years", color = "red", angle = 90, vjust = -0.5) +
  
  # Adjust legend and title text size
  theme(legend.text = element_text(size = 20),    # Adjust legend text size
        legend.title = element_text(size = 20),   # Adjust legend title size
        plot.title = element_text(size = 20, hjust = 0.5))   # Adjust plot title size and center it

# Display the plot
ggsurv

##########################################################################
km_slum <- survfit(surv_obj ~ all5y$mom_education, data = all5y)

# Plot with vertical line at 4 years (48 months)
ggsurv <- ggsurvplot(km_slum, 
                     data = all5y,
                     ylim = c(0.95, 1), 
                     conf.int = TRUE, 
                     title = "Kaplan-Meier Curve by Mother's Education", 
                     xlab = "Age (in Months)", 
                     ylab = "Survival Probability")

# Add vertical line at 48 months (4 years)
ggsurv$plot <- ggsurv$plot + 
  geom_vline(xintercept = 48, linetype = "dashed", color = "red", size = 0.8) + 
  annotate("text", x = 50, y = 0.97, label = "4 Years", color = "red", angle = 90, vjust = -0.5) +
  
  # Adjust legend and title text size
  theme(legend.text = element_text(size = 20),    # Adjust legend text size
        legend.title = element_text(size = 20),   # Adjust legend title size
        plot.title = element_text(size = 20, hjust = 0.5))   # Adjust plot title size and center it

# Display the plot
ggsurv

###########################################################################33
#Mother’s engagement in income generation activity

km_slum <- survfit(surv_obj ~all5y$mom_engagement_inco, data = all5y)
ggsurvplot(km_slum, data = all5y,ylim = c(0.95, 1),conf.int = TRUE, title = "Kaplan-Meier Curve by Mother’s engagement in
           income generation activity", 
           xlab = "Age (in Months)", ylab = "Survival Probability")
########################################################################3
# Fit the survival model
km_slum <- survfit(surv_obj ~ all5y$Period, data = all5y)

# Create the survival plot
ggsurv <- ggsurvplot(km_slum, 
                     data = all5y,
                     ylim = c(0.79, 1),
                     conf.int = TRUE, 
                     title = "Kaplan-Meier Curve by Different Time Periods", 
                     xlab = "Age (in Months)", 
                     ylab = "Survival Probability")

# Adjust legend and title text size
ggsurv$plot <- ggsurv$plot + 
  theme(legend.text = element_text(size = 20),    # Adjust legend text size
        legend.title = element_text(size = 20),   # Adjust legend title size
        plot.title = element_text(size = 20, hjust = 0.5))   # Adjust plot title size and center it

# Display the plot
ggsurv

#########################################################################fitting models######################
library(SurvMetrics)
library(survex)
library(timeROC)
library(randomForestSRC)
library(rpart)
library(stargazer)
library(merTools)
library(sjPlot)
library(rpart.plot)
# Split data into training and testing sets
set.seed(123)
n <- nrow(all5y)
n_train <- round(0.8 * n)  # 80% for training
train_indices <- sample(1:n, n_train)

train_data <- all5y[train_indices, ]
test_data <- all5y[-train_indices, ]

# Convert character variables to factors
train_data[] <- lapply(train_data, function(x) if (is.character(x)) as.factor(x) else x)
# Check for invalid times
invalid_times <- train_data[train_data$Time <= 0, ]
if (nrow(invalid_times) > 0) {
  cat("There are", nrow(invalid_times), "invalid times (<= 0). Removing them.\n")
  train_data <- train_data[train_data$Time > 0, ]  # Remove invalid times
}
# Cox proportional hazards model
cox_model <- coxph(Surv(Time, eventDeath) ~ momMaritalstatus +slumarea +genderChild+Period
                   + mom_education + multiplebirth + mom_engagement_inco +
                     BCG_vaccination+hhsize, data = train_data, x = TRUE)
summary(cox_model)



# Weibull model
weibull_model <- survreg(Surv(Time, eventDeath) ~  momMaritalstatus +slumarea +genderChild+Period
                         + mom_education + multiplebirth + mom_engagement_inco +
                           BCG_vaccination+hhsize, data = train_data, dist = "weibull")
summary(weibull_model)

# Calculate hazard ratios
hazard_ratios <- exp(coef(weibull_model))
print(hazard_ratios)

# Calculate 95% confidence intervals
conf_intervals <- exp(confint(weibull_model))
print(conf_intervals)



# Fit Survival Tree Model with further adjustments
surv_tree_model <- rpart(Surv(Time, eventDeath) ~ momMaritalstatus +slumarea +genderChild+Period
                         + mom_education + multiplebirth + mom_engagement_inco +
                           BCG_vaccination+hhsize,data =  train_data, method = "poisson",
                         control = rpart.control(minsplit = 3, cp = 0.000, minbucket = 4, maxdepth = 3))  # Further adjustments

summary(surv_tree_model)

# Define a color palette for the nodes
color_palette <- c("#E0F7FA", "#B2EBF2", "#80DEEA", "#4DD0E1", "#26C6DA", "#00BCD4")

rpart.plot(surv_tree_model)

# Plot the survival tree with improved aesthetics
rpart.plot(surv_tree_model, 
           type = 4,                  # Type of plot
           extra = 101,               # Extra details (event rate)
           fallen.leaves = TRUE,      # Keep leaves at the bottom
           main = "Survival Tree Model",  # Title
           box.palette = color_palette,   # Use the color palette
           shadow.col = "gray"       # Add shadow for depth
                         # Scale text size
          ,roundint =FALSE )                # Larger text and boxes


# Check for the number of branches
num_branches <- length(unique(surv_tree_model$frame$var[!is.na(surv_tree_model$frame$var)]))
cat("Number of branches in the tree:", num_branches, "\n")



# Fit Random Survival Forest model
rf_model <- rfsrc(Surv(Time, eventDeath) ~  momMaritalstatus +slumarea +genderChild+period
                  + mom_education + multiplebirth + mom_engagement_inco +
                    BCG_vaccination+hhsize, 
                  data = all5y, 
                  ntree = 500,    # Number of trees
                  nodesize = 5,   # Minimum node size
                  importance = TRUE)  # Calculate variable importance

# View model summary
print(rf_model)

plot(rf_model)
################################################################comparing models accuracy########################################
# Load required libraries
library(survival)
library(survAUC)
library(survivalROC)
library(rpart)
library(rpart.plot)
library(randomForestSRC)
# Cox proportional hazards model
cox_model <- coxph(Surv(Time, eventDeath) ~ momMaritalstatus + slumarea + genderChild + Period +
                     mom_education + multiplebirth + mom_engagement_inco +
                     BCG_vaccination + hhsize, data = train_data, x = TRUE)

# Weibull model
weibull_model <- survreg(Surv(Time, eventDeath) ~ momMaritalstatus + slumarea + genderChild + Period +
                           mom_education + multiplebirth + mom_engagement_inco +
                           BCG_vaccination + hhsize, data = train_data, dist = "weibull")

# Survival tree model
surv_tree_model <- rpart(Surv(Time, eventDeath) ~ momMaritalstatus + slumarea + genderChild + Period +
                           mom_education + multiplebirth + mom_engagement_inco +
                           BCG_vaccination + hhsize, data = train_data, method = "poisson",
                         control = rpart.control(minsplit = 3, cp = 0.000, minbucket = 4, maxdepth = 3))

# Calculate AUC for Cox model
cox_predictions <- predict(cox_model, newdata = train_data, type = "risk")
cox_auc <- survivalROC(Stime = train_data$Time, 
                       status = train_data$eventDeath, 
                       marker = cox_predictions, 
                       predict.time = 12,  # Adjust the time point as necessary
                       method = "KM")

# Calculate AUC for Weibull model
weibull_predictions <- predict(weibull_model, newdata = train_data, type = "response")
weibull_auc <- survivalROC(Stime = train_data$Time, 
                           status = train_data$eventDeath, 
                           marker = weibull_predictions, 
                           predict.time = 12,  # Adjust the time point as necessary
                           method = "KM")

# Calculate AUC for Survival Tree model
surv_tree_predictions <- predict(surv_tree_model, newdata = train_data, type = "vector")
surv_tree_auc <- survivalROC(Stime = train_data$Time, 
                             status = train_data$eventDeath, 
                             marker = surv_tree_predictions, 
                             predict.time = 12,  # Adjust the time point as necessary
                             method = "KM")

rf_predictions <- predict(rf_model, newdata = train_data, proximity = FALSE)$predicted
# Calculate AUC for the Random Forest model
rf_auc <- survivalROC(Stime = train_data$Time, 
                      status = train_data$eventDeath, 
                      marker = rf_predictions, 
                      predict.time = 12,   # Adjust the time point as necessary
                      method = "KM")
# Output AUC values
cat("Cox Model AUC:", cox_auc$AUC, "\n")
cat("Weibull Model AUC:", weibull_auc$AUC, "\n")
cat("Survival Tree Model AUC:", surv_tree_auc$AUC, "\n")
cat("Random Forest Model AUC:", rf_auc$AUC, "\n")

########################################################################################################333
predicted_survival <- predict(rf_model, newdata = test_data)$predicted

# Create a Surv object
surv_obj <- Surv(time = test_data$Time, event = test_data$eventDeath)

# Use concordance instead of survConcordance
c_index <- concordance(surv_obj ~ predicted_survival)

# Print or check the concordance index
print(c_index)


# Calculate the C-index
c_index <- survConcordance(surv_obj ~ predicted_survival)

# Display the C-index
cat("C-index for Random Survival Forest:", c_index$concordance, "\n")

# Plot the Random Survival Forest
plot(rf_model, main = "Random Survival Forest")

# Variable importance plot
var.select <- var.select(rf_model)


################################################################
# Predict risk scores
predicted_risk_cox <- predict(cox_model, newdata = test_data)

# Create a Surv object
surv_obj <- Surv(time = test_data$Time, event = test_data$eventDeath)

# Calculate C-index
c_index_cox <- survConcordance(surv_obj ~ predicted_risk_cox)

# Display the C-index
cat("C-index for Cox model:", c_index_cox$concordance, "\n")


##############################################################3

# Predict linear predictors (risk scores)
predicted_risk_weibull <- predict(weibull_model, newdata = test_data)

# Calculate C-index
c_index_weibull <- survConcordance(surv_obj ~ predicted_risk_weibull)

# Display the C-index
cat("C-index for Weibull model:", c_index_weibull$concordance, "\n")
##########################################################################################

# Predict risk scores
predicted_risk_tree <- predict(surv_tree_model, newdata = test_data, type = "vector")

# Calculate C-index
c_index_tree <- survConcordance(surv_obj ~ predicted_risk_tree)

# Display the C-index
cat("C-index for Survival Tree model:", c_index_tree$concordance, "\n")


#####################################################################################################
library(pec)
# Time points for prediction
times <- seq(1, 60, by = 1)  # example time points (can adjust)

# Calculate Brier score for the Cox PH model
brier_cox <- pec(list(CoxPH = cox_model),
                 formula = Surv(Time, eventDeath) ~ 1,
                 data = train_data, times = times)



# Display the Brier scores
print(brier_cox)

########################################################################################################



# Fit Weibull model
weibull_model <- survreg(Surv(Time, eventDeath) ~ momMaritalstatus + slumarea + genderChild + Period +
                           mom_education + multiplebirth + mom_engagement_inco +
                           BCG_vaccination + hhsize, data = train_data, dist = "weibull")

# Function to compute the survival function from the Weibull model
predict_weibull_surv <- function(model, newdata, times) {
  scale <- model$scale  # Weibull scale parameter
  lp <- predict(model, newdata = newdata, type = "linear")  # Linear predictor (log of survival times)
  shape <- 1 / scale  # Weibull shape parameter (inverse of scale)
  
  # Compute survival probabilities at each time point
  surv_probs <- sapply(times, function(t) {
    exp(-(t / exp(lp))^shape)  # Weibull survival function
  })
  
  return(surv_probs)
}

# Define time points to evaluate the survival probabilities
times <- seq(1, 60, by = 1)  # Example time points (adjust based on your data)

# Get predicted survival probabilities for each individual at each time point
pred_surv_weibull <- predict_weibull_surv(weibull_model, newdata = train_data, times = times)

# Now you can calculate the Brier score manually or use another method to evaluate it
# Load required library for the Brier score calculation
library(prodlim)

# Function to calculate the Brier score
calculate_brier <- function(surv_probs, observed_data, times) {
  n <- length(observed_data$Time)
  brier_scores <- numeric(length(times))
  
  for (i in seq_along(times)) {
    time_point <- times[i]
    
    # Censoring indicator: 1 if event occurred, 0 if censored
    event_indicator <- ifelse(observed_data$Time <= time_point & observed_data$eventDeath == 1, 1, 0)
    
    # Brier score at each time point
    brier_scores[i] <- mean((surv_probs[i, ] - event_indicator)^2)
  }
  
  return(mean(brier_scores))  # Return the integrated Brier score (average across all time points)
}

# Calculate Brier score for the Weibull model
brier_weibull <- calculate_brier(pred_surv_weibull, train_data, times)

# Print the Brier score
print(brier_weibull)

######################################################################################################33


library(randomForestSRC)

# Fit a Random Survival Forest model as an alternative
rsf_model <- rfsrc(Surv(Time, eventDeath) ~ momMaritalstatus + slumarea + genderChild + Period +
                     mom_education + multiplebirth + mom_engagement_inco +
                     BCG_vaccination + hhsize, data = train_data)

# Now you can calculate the Brier score using the random forest model
brier_rsf <- pec(list(RSF = rsf_model),
                 formula = Surv(Time, eventDeath) ~ 1,
                 data = train_data, times = times)
print(brier_rsf)
##########################################################################################################3
# Load required libraries
library(randomForestSRC)
library(pec)


# Calculate Brier score for the Random Forest model
brier_rf <- pec(object = list(RSF = rf_model),
                formula = Surv(Time, eventDeath) ~ 1,
                data = train_data,
                times = times)

# Print Brier score for Random Forest model
print(brier_rf)




#################################################################################################################

# Create a data frame with the metrics
model_metrics <- data.frame(
  Model = c("Cox Proportional Hazards", "Weibull", "Survival Tree", "Random Survival Forest"),
  C_index = c(0.6992, 0.3026, 0.6782, 0.7358),
  AUC = c(0.7889, 0.2493, 0.7675, 0.8341),
  IBS = c(0.022, 0.9268, 0.020, 0.017)
)

# Melt the data frame for ggplot
model_metrics_melted <- melt(model_metrics, id.vars = "Model", variable.name = "Metric", value.name = "Value")

# Create the bar plot with enhanced design
ggplot(model_metrics_melted, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7, color = "black") +
  labs(title = "Comparison of Survival Models",
       x = "Model",
       y = "Metric Value") +
  scale_fill_manual(values = c("C_index" = "#4C8BF5",  # Soft Blue
                               "AUC" = "#66CDAA",    # Soft Teal
                               "IBS" = "#F08080")) +   # Soft Red
  theme_minimal(base_size = 12) +  # Increase base font size for better readability
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.9, color = "black", size = 10),  # Darker color and larger font for model names
        legend.position = "top",  # Move legend to the top for better visibility
        legend.title = element_blank(),  # Remove legend title
        panel.grid.major.y = element_line(size = 0.5, color = "lightgrey"),  # Add light grid lines
        panel.grid.minor.y = element_blank()) +  # Remove minor grid lines
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  geom_text(aes(label = round(Value, 3)), position = position_dodge(0.7), vjust = -0.5, size = 4, color = "black")  # Add value labels above bars

############################################################END#############################################################33