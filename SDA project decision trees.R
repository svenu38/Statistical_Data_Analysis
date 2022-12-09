rm(list = ls())
white_dat <- read.csv("C:/Users/straw/Documents/winequality-white.csv", sep = ";")
attach(white_dat)

# lets see how many predictor/feature this data set has.

names(white_dat)
str(white_dat)
summary(white_dat)
##### Summary
##It seems that the dataset is very clean, with no missing data and clear structure. All variables are numeric. 
#The range of independent variables varies greatly, so when building the model I will normalize them to be within 
#the same range.
pairs.panel(white_dat)
#Next step I will check the pairwise relationship of each variable. As we can see from the following figure, 
#there is not a clear linear relationship between the quality variable and other covariants, indicating that 
# simple linear regression might not work. In addition, there is some collinearity between covariants. 
#These are against the assumption of a linear model.
# Dataset has 11 predictors along with 1 dependent/target variable "quality.
# All the predictors are numeric values. 
# summary statistics shows that most of the variable has wide range compared to the IQR, which may
# indicate spread in data and presence of outliers.
# Now the question arises what we will do with this data set.
# First we will see the distribution of data by making the histogram for all
#the variables.

par(mfrow = c(2,2),oma = c(1,1,0,0) + 0.1)

barplot(table(quality), col = c('blue', 'gray', 'slategray1', 'slategray4', 'darkgray'))
mtext("Quality", side=1, outer = F, line = 2, cex = 0.8)
hist(fixed.acidity, h = 0.5, col = 'slategray')
hist(volatile.acidity, h = 0.5, col = 'slategray3')
hist(citric.acid, h = 0.5, col = 'slategray4')
hist(residual.sugar, h =0.5, col = 'blue')
hist(chlorides, h =0.5, col = 'blue1')
hist(free.sulfur.dioxide, h =0.5, col = 'blue2')
hist(total.sulfur.dioxide, h =0.5, col = 'red')
hist(density, h =0.5, col = 'red1')
hist(pH, h =0.5, col = 'red2')
hist(sulphates, h =0.5, col = 'blue')
hist(alcohol, h =0.5, col = 'red')

# The bar plot of the our dependent variable shows that the most of the quality is distributed
# around 5 to 7, with some one 4 and 8 as well.

# Now we will try to see the data spread by making the boxplot of all the variables.

par(mfrow = c(1, 5), mar = c(2,2,1,1) +0.1)
boxplot(fixed.acidity, col = 'slategray3', pch = 18)
mtext("Fixed Acidity", cex = 0.8, side = 1, line = 2 )
boxplot(volatile.acidity, col = 'slategray', pch = 18)
mtext("Volatile Acidity", cex = 0.8, side = 1, line = 2)
boxplot(citric.acid, col = 'slategray', pch = 18)
mtext("Citric Acid", cex = 0.8, side = 1, line = 2)
boxplot(residual.sugar, col = 'slategray', pch = 18)
mtext("Residual Sugar", cex = 0.8, side = 1, line = 2)
boxplot(chlorides, col = 'slategray', pch = 18)
mtext("Chlorides", cex = 0.8, side = 1, line = 2)
boxplot(free.sulfur.dioxide, col = 'slategray', pch = 18)
mtext("Free sulfur dioxide", cex = 0.8, side = 1, line = 2)
boxplot(total.sulfur.dioxide, col = 'slategray', pch = 18)
mtext("total sulfur dioxide", cex = 0.8, side = 1, line = 2)
boxplot(density, col = 'slategray', pch = 18)
mtext("Density", cex = 0.8, side = 1, line = 2)
boxplot(pH, col = 'slategray', pch = 18)
mtext("pH", cex = 0.8, side = 1, line = 2)
boxplot(sulphates, col = 'slategray', pch = 18)
mtext("Sulphates", cex = 0.8, side = 1, line = 2)
boxplot(alcohol, col = 'slategray', pch = 18)
mtext("Alcohol", cex = 0.8, side = 1, line = 2)
## these the boxplot for every variable in the dataset . This gives us the mean and median
# spread of data along the graph.

  # By looking at these box plots we can say that the most of the predictors are spread along
  # way to above which confirm the outlier. if we remove these outlier than data will be more
  # symmetric. 
# fixed Acidity, Volatile Acidity and Citric acid have outliers. If we removed these outliers
# distribution of the variables can be symmetric.
# residual sugar has positively skewed distribution. 
# Alcohol has irregular shaped distribution but it doesn't have outliers.

##z_scores <- as.data.frame(sapply(white_dat, function(white_dat) (abs(white_dat-mean(white_dat))/sd(white_dat))))
##head(z_scores)
##no_outliers <- z_scores[!rowSums(z_scores>3), ]
##dim(no_outliers)
##dim(white_dat)
##here we calculated the data with no outliers.
##z_scores


white_outliers = c()
for ( i in 1:11 ) {
  stats = boxplot.stats(white_dat[[i]])$stats
  bottom_outlier_rows = which(white_dat[[i]] < stats[1])         # bottom whisker
  top_outlier_rows = which(white_dat[[i]] > stats[5])            # upper whisker
  white_outliers = c(white_outliers , top_outlier_rows[ !top_outlier_rows %in% white_outliers ] )
  white_outliers = c(white_outliers , bottom_outlier_rows[ !bottom_outlier_rows %in% white_outliers ] )
}
## stats	
## a vector of length 5, containing the extreme of the lower whisker, the lower 'hinge', the median, 
## the upper 'hinge' and the extreme of the upper whisker.
# this code will find outliers for each variables, the observations lie outside the 1.5*IQR as outliers


# Now we will use cook's distance for influential observations

linear_model <- lm(white_dat$quality~., data = white_dat)
cooksdistnace_white <- cooks.distance(linear_model)
plot(cooksdistnace_white, pch = "*", cex = 2, main = "influential obs by cooks distance")
abline(h = 4* mean(cooksdistnace_white, na.rm = T), col = "Gray")

some_var <- 4 * mean(cooksdistnace_white, na.rm = T)
str(some_var)

## Now let's have a look at the influential observations

head(white_dat[cooksdistnace_white> 4*mean(cooksdistnace_white, na.rm = T), ])

## This shows that row 99 and 252 has high residual sugar. row 99 and 254 has very low
## free.sulphur.dioxide.

## now we remove all outliers because regression model doesn't work with outliers in data

cooksoutliers <- as.numeric(rownames(white_dat[cooksdistnace_white> 4* mean(cooksdistnace_white, na.rm = T), ]))
cooksoutliers
### once try removing only outlier by cooksd
all_outliers <- c(white_outliers, cooksoutliers[!cooksoutliers %in% white_outliers])

cleandata_white <- white_dat[-all_outliers, ]
dim(cleandata_white)
dim(white_dat)
## now we're left with 3999 observations out of 4898 observations.
## now we will look again at the new set of histograms and check what effect removing outliers
## have on the white wine dataset.


par(mfrow = c(2,2))
hist(cleandata_white$quality, h = 0.5, col = 'slategray')
hist(cleandata_white$fixed.acidity, h = 0.5, col = 'slategray')
hist(cleandata_white$volatile.acidity, h = 0.5, col = 'slategray3')
hist(cleandata_white$citric.acid, h = 0.5, col = 'slategray4')
hist(cleandata_white$residual.sugar, h =0.5, col = 'blue')
hist(cleandata_white$chlorides, h =0.5, col = 'blue1')
hist(cleandata_white$free.sulfur.dioxide, h =0.5, col = 'blue2')
hist(cleandata_whitetotal.sulfur.dioxide, h =0.5, col = 'red')
hist(cleandata_white$density, h =0.5, col = 'red1')
hist(cleandata_white$pH, h =0.5, col = 'red2')
hist(cleandata_white$sulphates, h =0.5, col = 'blue')
hist(cleandata_white$alcohol, h =0.5, col = 'red')



# Now by looking the histogram of variables again we can see that distribution
# is normally distributed except for residual.sugar which is skewed to right

## now we will see the scattrplot to detremine if there is a linear correlation
# between our variables.

pairs(cleandata_white, 
      col = cleandata_white$quality,                     # for color of points numberwise
      pch = cleandata_white$quality)           # changes the shape of points numberwise quantity

#warnings()


## x <- rnorm(10) * 4
## y <- rnorm(10) * 5
## z <- rnorm(10) * 9
## dataframe <- data.frame(x,y,z)
## pairs(dataframe, col = c(5,5,2,1,1,6,4), pch = c(1,3,5,6,6,7,7,7))

head(cleandata_white)
library(tidyverse)
library(pander)
## CORRELATION
c <- cor(
  cleandata_white %>%
    # first we remove unwanted columns
    mutate(
      # now we translate quality to a number
      quality = as.numeric(quality)
    )
)
emphasize.strong.cells(which(abs(c) > .3 & c != 1, arr.ind = TRUE)) # emphasis on particular index.
pandoc.table(c) # table wise correlation of every predictor with each other.
install.packages("ggcorrplot")
library(ggcorrplot)
library(ggplot2)

ggcorrplot(c, hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank") # plot for correlation.



## Correlation is between 1 to -1 both have significant. Here values in orange shows significant correlation
## and values in dark blue shows negative correlations.

## This shows that there is strong correlation between residual.sugar/density and density/alcohol.
## Also it shows that alcohol is the variable with highest correlation with quality. That means
## with inc. in alcohol volume quality also increases.

### Now WE WILL DO THE SAME FOR RED WINE DATA.

## redwine_data <- read.csv("C:/Users/straw/Documents/winequality-red.csv", sep = ";")
## view(redwine_data)  # view the dataset in a new window
## 
## str(redwine_data)

# looking at the dataset it has 11 predictor and 1 target variable like the White wine dataset.
# All are continuous



###MULTIPLE Linear Regression


###Equation of Multiple Linear Regression is as follows:

###Y = B0 + B1X1 + B2X2 + .. + BnXn + E

##Now start with fitting the Regresssion

## lets split the data into train and test set.

set.seed(1111)
sample_80 <- round(nrow(cleandata_white)*0.8)
index<- sample(seq_len(sample_80), size = sample_80)

train_white <- cleandata_white[index, ] # training data 80% of the data is training
test_white <- cleandata_white[-index, ] # Rest is test data

## Now we will create the model by using multiple regression. 

wwfit <- lm(quality~., data = train_white) ## fitting the regression

summary(wwfit)
mean(wwfit$residuals^2)            # calculate the mean squared error

# from the summary function we in infer that most significat variables are fixed.acidity,
#volatile.acidity, residual.sugar, free.sulphur.dioxide, density, pH, sulphates and alocohol
# citric.acid and total.sulfur.dioxide is not significat in this model.
# MSE = 0.5352
# adjusted R-squared = 0.2793
plot(wwfit)

### Stepwise model selection for regression
library(MASS)
stepAIC(wwfit, direction = "both", trace = FALSE)
summary(step.model)

step.model_both <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
                        chlorides + free.sulfur.dioxide + density + pH + sulphates + 
                        alcohol, data = train_white) ## fitting both selection model


summary(step.model_both)
#plot(step.model_both)
## backward approach
step(wwfit, direction = "backward", trace = FALSE)

step.model_backward <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
                            chlorides + free.sulfur.dioxide + density + pH + sulphates + 
                            alcohol, data = train_white) # fit with backward selection

summary(step.model_backward)


## forward approach

step(wwfit, direction = "forward", trace = FALSE)


step.model_forward <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
  residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
  density + pH + sulphates + alcohol, data = train_white) # fit with forward selection

## only in forward selection all the predictor were selected as best fir for model.
## now we have applied all the resampling method. we will calculate the Rsquared for all the fit.
ad.r.sq.1 <- summary(step.model_both)$adj.r.squared  # adjustedr-squared for both selection
ad.r.sq.1
ad.r.sq.2 <- summary(step.model_backward)$adj.r.squared  # adjustedr-squared for backward selection
ad.r.sq.2
ad.r.sq.3 <- summary(step.model_forward)$adj.r.squared  # adjustedr-squared for forward selection
ad.r.sq.3
## adjustedR-squared for both selection is 0.2795494
## adjustedR-squared for both selection is 0.2795494
## adjustedR-squared for both selection is 0.279273


## Now we will use the 10 fold cross validation to estimate the average prediction error
library(caret)
library(leaps)
library(modelr)
train.control <- trainControl(method = "cv", number = 10)

step_model_cv <- train(quality~., data = train_white,
                       method = "leapBackward",  # backward selection
                       tuneGrid = data.frame(nvmax = 5:11), # tuning parameters
                       trControl = train.control)

step_model_cv$results
summary(step_model_cv$finalModel)

## linear_red_model <- train(quality~., data = train_white,
##                           method = "lm",
##                           trControl = train.control)
## 
## 
## linear_red_model
## we used nvmax from 5 to 11 to get the best model with 5 variables to 11 variables
## Density and residual.sugar is always selected for best fit by crossvalidation in backward.
## This model gives the best fit at 9 predictors. It is the point where model has the least
## RMSE. there it leaves out total.sulfur.dioxide and citric.acid.
## this result was also achieved by forward and backward selection before.
## RMSE and MAE are two different metrics measuring the prediction error of each model. Lower
## the RMSE and MAE the better the model.
## R-squared indicates the correlation between the observed outcomes values predicted by the model.
## Higher the R-squared better the model

#### Evident from the feature selection we don't obtain a model with higher preformance.
# coincidentally best so far is not found by any selection, but from manually including all the
# predictors for the model selection.
## hence for now best fit will be the model with all the available predictors i.e. 11 predictos.

ggplot(step_model_cv) # visualization of RMSE with the number of predictors.





## lasso and ridge
## we will use the lasso/ridge(regularized approaches) to model the quality of 
## white wine and compare the results with the results obtained in previous selction
## methods

## LASSO REGRESSION
##???i=1N(yi??????j=1pxij??j)2+?????j=1p|??j|
library(glmnet)
library(plotmo)
X <- model.matrix(quality~., train_white)[,-1] # matrix of predictor variables
Y <- train_white$quality

lasso_reg <- glmnet(X,Y, alpha = 1)
par(mfrow = c(1,1))
plot_glmnet(lasso_reg) 
# Almost all the coeficient are positive except for density. predictors that emerges
# first are the ones that explains the most of the result.

lambda_seq <- 10^seq(2, -2, by = -.01) # tuning parament for cv

# cross-validation for lasso

cvLasso_reg <- cv.glmnet(X,Y, alpha = 1, lambda = lambda_seq)
plot(cvLasso_reg)  
## This shows that as the MSE increase
## The intervals estimate variance of the loss metric (red dots). They're computed using CV.
## The vertical lines show the locations of ??min and ??1se.
##The numbers across the top are the number of nonzero coefficient estimates.
  
## let's train the lasso model
best_lambda <- cvLasso_reg$lambda.min
large_lambda <- cvLasso_reg$lambda.1se

pred_lasso = predict(lasso_reg, s = best_lambda, type = "coefficients", 
                     newx = model.matrix(quality~., test_white)[,-1]) # prediction of test set
pred_lasso
## removes fixed.acidity citrix.acid and total.sulphur.dioxide.


pred_lasso_large = predict(lasso_reg, s = large_lambda, type = "coefficients", 
                     newx = model.matrix(quality~., test_white)[,-1])

pred_lasso_large
## It removes the 4 predictor from the model and keeps only 7 predictors.
## which is very different from what we seen in forward, backward, and both
## sele ction.
## now let's try making the fit the with selected predictors by lasso.which is
## volatile.acidity, residual.sugar, chloride, free.sulphur.dioxide, pH, sulphates and alcohol

## Ridge Regression

ridgereg_white <- glmnet(X,Y, alpha = 0)
plot_glmnet(ridgereg_white)
# as evident from the plot that if lambda decreases the number of attributes that
# shows up also increases.

cvridgereg_white <- cv.glmnet(X,Y, alpha = 0, lambda = lambda_seq) # crossvalidation for ridge
plot(cvridgereg_white)
# this plot shows that mean squared error will remain low if the lambda value remain
# less than -2 in our 11 variable model.

cvridgereg_white$lambda.min   # min. lambda value
cvridgereg_white$lambda.1se   # max lambda value

pred_ridge_lammin <- predict(ridgereg_white, s = cvridgereg_white$lambda.min, type = "coefficients")
pred_ridge_lammin
pred_ridge_lammax <- predict(ridgereg_white, s = cvridgereg_white$lambda.1se, type = "coefficients")
pred_ridge_lammax
# it also give us about the same no. of predictors as 7. Now we will make a 
# using the predictors we got by using the lasso and ridge regression.


## sst <- sum((Y- mean(Y))^2)
## sse <- sum((pred_lasso - Y)^2)

## Now using all these regularized approaches we are down to 7 predictors instead of
## 9(backward selection). Now we will make the model by these 7 predictors namelyvolatile.acidity, 
## residual.sugar, chloride, free.sulphur.dioxide, pH, sulphates and alcohol

Model_lasso <- lm(quality~alcohol+pH+chlorides+free.sulfur.dioxide+volatile.acidity+residual.sugar+sulphates, train_white)
summary(Model_lasso)

MSE = mean(Model_lasso$residuals^2)
MSE  
adjusted.r.squared <- summary(Model_lasso)$adj.r.squared
adjusted.r.squared

## as we can see there is no improvement in adjusted R.squared by using hte model
## selected by Lasso/ridge regression. So we will move and use our previous model
## which have all the 11 predictors.

## PREDICTIONS

# model preformance on the train dataset:
y_pred <- wwfit$fitted.values
y_true <- train_white$quality
adjusted_r_squared <- summary(wwfit)$adj.r.squared
rmse <- RMSE(y_pred,y_true)
mae <- MAE(y_pred, y_true)
adjusted_r_squared
rmse
mae

## adjusted rsquared for the prediction on train dataset is
## Adjr.squared - 0.2793
## root mean squared error - 0.7316
## MAE is 0.5816

## prediction on test dataset

pred_test_lm <- predict(wwfit, newdata = test_white)
## preformance of model using test

rmse_test <- RMSE(pred_test_lm, test_white$quality)
mae_test <- MAE(pred_test_lm, test_white$quality)
rmse_test
mae_test


## rmse for test prediction is 0.7015
## Mae for test prediction is 0.5494

## plot of predicted using train dataset 
white_fit_back <- data.frame(qualitypred = step.model_backward$fitted.values,
                              qualityActual = train_white$quality)
ggplot(white_fit_back, aes(x = qualitypred, 
                           y = qualityActual)) + 
   geom_point(aes(color = as.factor(qualityActual)), show.legend = F) +
   geom_smooth(method = "lm", se = F) +
   labs(title = "predicted vs actual values using train dataset",
        x = "predicted quality",
        y = "Actual quality")

## plot of predicted with test dataset

white_predict_back <- data.frame(qualitypred = pred_test_lm,
                             qualityActual = test_white$quality)
ggplot(white_predict_back, aes(x = qualitypred, 
                           y = qualityActual)) + 
  geom_point(aes(color = as.factor(qualityActual)), show.legend = F) +
  geom_smooth(method = "lm", se = F) +
  labs(title = "predicted vs actual values using test dataset",
       x = "predicted quality",
       y = "Actual quality")

## conclusion on REGRESSION
## regression model is not good for predicting the wine quality as 
## prediction is not that well
## RMSE and MAE is still more signifies that our model doesn't predict the
## target variable well


## Now we will use another approach
# EVALUATION FUNCTION for Classification

Class_metric = function(prob, truth, plot = F, title = "") {
  # find cutoff with the best kappa
  cuts = seq(0.01, 0.99, by=0.01)
  kappa = c()
  for (cut in cuts){
    cat = as.factor(ifelse(prob >= cut, 1, 0))
    cm = confusionMatrix(cat, truth, positive = "1")
    kappa = c(kappa, cm$overall[["Kappa"]])
  }
  opt.cut = cuts[which.max(kappa)]
  
  # make predictions based on best kappa
  pred = as.factor(ifelse(prob >= opt.cut, 1, 0))
  confM = confusionMatrix(pred, truth, positive = "1")
  
  # calculate AUC
  roc = roc(as.vector(prob), as.factor(truth))
  auc = round(AUC::auc(roc),3)
  
  if (plot==T){
    # plot area under the curve
    par(mfrow = c(1,2), oma = c(0, 0, 2, 0))
    plot(roc, main = "AUC curve"); abline(0,1)
    text(0.8, 0.2, paste0("AUC = ", auc))
      
    # plot confusion matrix
    tab = table(truth, pred)
    plot(tab,
         xlab = "GroundTruth",
         ylab = "Predicted",
         main = "Confusion Matrix")
    text(0.9, 0.9, paste0('FN:', tab[2,1]))
    text(0.9, 0.05, paste0('TP:', tab[2,2]))
    text(0.1, 0.9, paste0('TN:', tab[1,1]))
    text(0.1, 0.05, paste0('FP:', tab[1,2]))
    mtext(paste0(title, " predicted vs. true using test set"), outer = TRUE)
    par(mfrow = c(1,1))
  }
  return(list(auc=auc, 
              confusionMatrix = confM))
}
    
## CLASSIFICATION ALGORITHM
## Here we will convert quality variable in binary variable, So that we can classify
## the wines, classification criteria will be the wines quality >= 6 will be good wine
## and less than 6 will be bad/average.
## Here we will make a new binary variable(y) called exquisite wine. so we will make a
## classification model to identify wines. 

Wine_df = white_dat %>% 
                    mutate(exquisite = ifelse(quality >= 6, "1", "0") %>% as.factor())

#str(Wine_df)

Wine_df = Wine_df[-12]  # dropping the quality column from the dataframe
  
dim(Wine_df)  # checking the shape of the data frame.
str(Wine_df)
table(Wine_df$exquisite) # to check the high and low quality values.

set.seed(123)
Index = createDataPartition(Wine_df$exquisite, p = 0.9, list = F)
Index
# print(floor(0.9*length(Wine_df$exquisite)))
x_train = Wine_df[Index, -12] %>% as.matrix()
dim(x_train)
view(x_train)
y_train = Wine_df[Index, 12]
table(y_train)

train_data <- data.frame(x_train, exquisite = y_train)  # training data

x_test <- Wine_df[-Index, -12] %>% as.matrix()
dim(x_test)

y_test <- Wine_df[-Index, 12]
table(y_test)
library(tidyr)

test_data <- data.frame(x_test, exquisite = y_test)  # test data

plot_data <- gather(train_data, key = "features", value = "value", 1:11)
ggplot(plot_data, aes(x = exquisite, y = value))+ geom_boxplot(aes(fill=exquisite))+
  coord_flip() + facet_wrap(~ features, ncol = 3, scales = "free") +
  labs(title = "visualizing the relationship between Y and other covarients",
       subtitle = "Note: wine quality >=6 is exquisite wine")

## This plot shows the relationship of variables with binary y variables. 
## exquisite wines seems to have smaller variations and outliers.

## principal components analysis

pca = prcomp(test_data[, -12])  # using test set for easy visualization.
summary(pca)

## This shows that PC1 and PC2 combined will make for more than 99 percent of variablity.
install.packages("AUC")
library(AUC)
library(pROC)
library(devtools)
library(ggfortify)
library(ggplot2)

autoplot(pca, data = test_data, colour = "exquisite")


## LOGISTIC REGRESSION

model.glm <- glm(exquisite ~., data = train_data, family = 'binomial')
summary(model.glm)

model.glm.prob <- predict(model.glm, data.frame(x_test), type = "response")
glm_model_eval <- Class_metric(model.glm.prob, y_test, plot= T, title = "Logistic Regression")
glm_model_eval

# AUC came out to be 0.767, which is above average, Accuracy of model is 69, sensitivity is
# 64, precision 85 and kappa of 37.
# Logistic regression had good accuracy so far.

## DECISION TREES

install.packages("C50")  # for classification trees.
library("C50")

model_dt <- C5.0(x_train, y_train)
model_dt
plot(model_dt) # not very pretty. right?

model_dt_pred <- predict(model_dt, x_test)
confusion_matrix <- confusionMatrix(model_dt_pred, y_test, positive = "1")
model.dt_eval <- list(auc = NA, confusionMatrix = confusion_matrix)
model.dt_eval

## trees boosting
model_dt_boost <- C5.0(x_train, y_train, trials = 10)
model_dt_boost
plot(model_dt_boost)

model_dtboost_pred <- predict(model_dt_boost, x_test)
confusion_matrix <-  confusionMatrix(model_dtboost_pred, y_test, positive = "1")
boost_eval <- list(auc = NA, confusionMatrix = confusion_matrix)
boost_eval

## boosting of Classification trees shoot our model accuray about 80 percent.
## with highest kappa recorder which is 0.546, Also precision is 0.75.

# 3. Random Forest
install.packages("randomForest")
library(randomForest)

model_rf <- randomForest(exquisite ~., data = train_data, ntree = 1000, mtry = sqrt(12))
model_rf

model_rf_prob <- predict(model_rf, x_test)
confusion_matrix <- confusionMatrix(model_rf_prob, y_test, positive = '1')
model_rf_eval <- list(auc = NA, confusionMatrix = confusion_matrix)
model_rf_eval

## as we can see the results Random forest is giving us the better results 
## So we will tune the hyper parameters in this case and see if we can improve
## kappa and accuracy.

install.packages("doParallel")
library(doParallel)
ct = trainControl(method = "repeatedcv", number = 10, repeats = 2)
grid_rf = expand.grid(.mtry = c(2, 3, 6))
set.seed(1)
cl = makePSOCKcluster(4)
registerDoParallel(cl)
tr.cvrfclass = train(exquisite~., data = train_data,
                     method = 'rf',
                     metric = "Kappa",
                     trControl = ct,
                     tuneGrid = grid_rf)
stopCluster(cl)
save(tr.cvrfclass, file = "~/wine_train_cvrfclass.RData")
load(file = "~/wine_train_cvrfclass.RData") 
getwd()
tr.cvrfclass

tr.cvrflass.pred <- predict(tr.cvrfclass, x_test)
confusion_matrix <- confusionMatrix(tr.cvrflass.pred, y_test, positive = "1")
tr.cvrfclass_eval <- list(auc = NA, confusionMatrix = confusion_matrix)
tr.cvrfclass_eval

## not much has improved by hyperparameter tuning of random forest.
## Tuning parameter was
glm = c(auc = glm_model_eval$auc, 
        glm_model_eval$confusionMatrix$overall, 
        glm_model_eval$confusionMatrix$byClass)
decision.tree = c(auc = model.dt_eval$auc, 
                  model.dt_eval$confusion_matrix$overall, 
                  model.dt_eval$confusion_matrix$byClass)
decision.tree.boost = c(auc = boost_eval$auc, 
                        boost_eval$confusion_matrix$overall, 
                        boost_eval$confusion_matrix$byClass)
rf = c(auc = model_rf_eval$auc, 
       model_rf_eval$confusion_matrix$overall, 
       model_rf_eval$confusion_matrix$byClass)
cv.rf = c(auc = tr.cvrfclass_eval$auc, 
          tr.cvrfclass_eval$confusion_matrix$overall, 
          tr.cvrfclass_eval$confusion_matrix$byClass)

all = cbind(glm, 
            decision.tree,decision.tree.boost,
            rf, cv.rf) %>% data.frame()

knitr::kable(all %>% round(3),
             caption = "comparing models")
install.packages("kableExtra")
library(kableExtra)
