#############################################
#                                           #
# Author:     Symphony Hopkins              #
# Date:       04/08/2023                    #
# Subject:    Project 4                     #
# Class:      DSCI 512                      #
# Section:    01W                           #         
# Instructor: Juan David Munoz              #
# File Name:  Project4_Hopkins_Symphony.R   #
#                                           #
#############################################

#1. Read the data set in Boston.csv Download Boston.csv
#   into R. Call the loaded data Boston. Make sure that 
#   you have the directory set to the correct location 
#   for the data.
#   Answer: See code.

#importing library
Boston <- read.csv("~/Documents/Maryville_University/DSCI_512/Week_4/Boston.csv")
View(Boston)

#2. The response is nox and the predictor is dis. Use the 
#   poly() function to fit a cubic polynomial regression 
#   to predict nox using dis. Report the regression output.
#   Answer: See code.

#creating cubic polynomial regression function
boston_pr <- lm(nox ~ poly(dis, 3), data=Boston)
summary(boston_pr)

#3. Your assistant data scientist, Tom Johnson, is considering
#   predicting nox using dis as a predictor. He proposes models
#   from degree 5, degree 4, and degree 3, and degree 2 polynomial
#   regression. Please perform cross-validation using caret 
#   package to select the optimal degree for the polynomial and 
#   justify your answer.
#   Answer: After performing cross validation, we concluded
#   that the optimal degree for Tom’s model would be 4 because it 
#   has the lowest RMSE (=0.0620871).

#importing library
library(caret)

#setting up cross validation parameters
train_control = trainControl(method='CV', number=10)

#setting seed for reproduciblity
set.seed(1)

#running cross validations
cv_d5 = train(nox ~ poly(dis, 5), data=Boston, 
              trControl=train_control, method='lm')
print(cv_d5)

cv_d4 = train(nox ~ poly(dis, 4), data=Boston, 
              trControl=train_control, method='lm')
print(cv_d4)

cv_d3 = train(nox ~ poly(dis, 3), data=Boston, 
              trControl=train_control, method='lm')
print(cv_d3)

cv_d2 = train(nox ~ poly(dis, 2), data=Boston, 
            trControl=train_control, method='lm')
print(cv_d2)

#4. Tom just took the DSCI 512. You recommend that he 
#   perform the following GAM analysis:
#4a. Predict nox using a smoothing spline of degree 3 
#    in dis and a smoothing spline of degree 2 in medv.
#    Answer: See code.

#importing library
library(gam)

#creating gam
bostom_gam_1 <- gam(nox ~ s(dis, 3) + s(medv, 2), data=Boston)
summary(bostom_gam_1)

#4b. Predict nox using a smoothing spline of degree 2 
#    in dis and a smoothing spline of degree 1 in medv.
#    Answer: See code.

#creating gam 
bostom_gam_2 <- gam(nox ~ s(dis, 2) + s(medv, 1), data=Boston)
summary(bostom_gam_2)

#4c. Perform anova analysis. Recommend the best model 
#    and justify your answer.
#    Answer: We recommend Model 2 as the best model
#    because it has the smallest p-value (this is indicated
#    by the asterisks as well in R).

#performing ANOVA
anova(bostom_gam_1, bostom_gam_2)

#End Assignment





