#STA 232A housing price 
#Zahra Aminiranjbar
##importing libraries
library(readr)
library(ggplot2)
library(knitr)
library(tidyverse)
library(caret)
library(leaps)
library(car)
library(mice)
library(scales)
library(RColorBrewer)
library(plotly)
library(nortest)
library(lmtest)
##Loading the data 
library("readxl")
housing <- read_excel("Real_estate_valuation.xlsx")
colnames(housing) = c('no','date','age','MRT','store','lat','long','Y')
head(housing)
#missing data
sum(is.na(housing))
#Removing the first col 
housing<-housing[,2:8]
housing$date<-housing$date-2012.666
#head(housing)
#pair wise comparison 
jpeg(file="scatter_plot_matrix.jpeg", width=15, height=15, unit="in", res=600)
pairs(housing, main = "Scatterplot Matrix", pch=19, lowe.panel=NULL)
dev.off()
#Correlation matrix
cor(housing)

#Normality 
jpeg(file="QQplot.jpeg", width=10, height=7, unit="in", res=600)
attach(mtcars)
par(mfrow = c(3, 3))
qqnorm(housing$date,main = "QQ-date", pch = 1, frame = FALSE)
qqline(housing$date, col = "steelblue", lwd = 2)
qqnorm(housing$age,main = "QQ-age", pch = 1, frame = FALSE)
qqline(housing$age, col = "steelblue", lwd = 2)
qqnorm(housing$MRT,main = "QQ-MRT", pch = 1, frame = FALSE)
qqline(housing$MRT, col = "steelblue", lwd = 2)
qqnorm(housing$store,main = "QQ-store", pch = 1, frame = FALSE)
qqline(housing$store, col = "steelblue", lwd = 2)
qqnorm(housing$lat,main = "QQ-lat", pch = 1, frame = FALSE)
qqline(housing$lat, col = "steelblue", lwd = 2)
qqnorm(housing$long,main = "QQ-long", pch = 1, frame = FALSE)
qqline(housing$long, col = "steelblue", lwd = 2)
qqnorm(housing$Y,main = "QQ-Y", pch = 1, frame = FALSE)
qqline(housing$Y, col = "steelblue", lwd = 2)
dev.off()


#correlation heatmap
#plotting Correlation in heat map 
library(reshape2)
cormat <- round(cor(housing),2)
jpeg(file="Correlation matrix heat map.jpeg", width=5, height=3, unit="in", res=600)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
dev.off()
#Summary of the data
summary(housing)
#plotting the location to see if there is pattern  
jpeg(file="Data map-Longtitude vs Latitude.jpeg", width=5, height=3, unit="in", res=600)
plot_map = ggplot(housing, 
                  aes(x = long, y = lat, color = Y,)) +
  geom_point(aes(size = MRT), alpha = 0.4) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Data Map - Longtitude vs Latitude") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired", labels = comma) +
  labs(color = "Housing value", size = "MRT")
plot_map
dev.off()

# Histgrams of the distributions -----------------
jpeg(file="Histograms.jpeg", width=7, height=7, unit="in", res=600)
attach(mtcars)
par(mfrow = c(3, 3))
hist(housing$date, breaks = 30, main = "Date", border="darkorange", col="dodgerblue")
hist(housing$age, breaks = 30, main = "Age", border="darkorange", col="dodgerblue")
hist(housing$MRT, breaks = 30, main = "MRT", border="darkorange", col="dodgerblue")
hist(housing$store, breaks = 30, main = "store", border="darkorange", col="dodgerblue")
hist(housing$lat, breaks = 30, main = "latitude", border="darkorange", col="dodgerblue")
hist(housing$long, breaks = 30, main = "longtitude", border="darkorange", col="dodgerblue")
hist(housing$Y, breaks = 30, main = "Housing price", border="darkorange", col="dodgerblue")
dev.off()

#Full model############################

#Simple LR without any change to the data 
library(dplyr)
housing <- read_excel("Real_estate_valuation.xlsx")
colnames(housing) = c('no','date','age','MRT','store','lat','long','Y')
housing<-housing[,2:8]
housing$date<-housing$date-2012.666
lm.full = lm(Y ~ ., data = housing)
summary(lm.full)

#Plot residuals
jpeg(file="Residuals.jpeg", width=7, height=7, unit="in", res=600)
attach(mtcars)
par(mfrow=c(3,3))
plot(lm.full$fitted.values,lm.full$residuals,xlab = 'fitted values',ylab = 'residuals',main = 'residual vs fittedvalues')
plot(housing$date,lm.full$residuals,xlab = 'date',ylab = 'residuals')
plot(housing$age,lm.full$residuals,xlab = 'age',ylab = 'residuals')
plot(housing$MRT,lm.full$residuals,xlab = 'MRT',ylab = 'residuals')
plot(housing$store,lm.full$residuals,xlab = 'store',ylab = 'residuals')
plot(housing$lat,lm.full$residuals,xlab = 'lat',ylab = 'residuals')
plot(housing$long,lm.full$residuals,xlab = 'long',ylab = 'residuals')
dev.off()
#boX plot and QQ plot
jpeg(file="Box plot and QQ plot.jpeg", width=10, height=7, unit="in", res=600)
par(mfrow=c(1,2))
boxplot(lm.full$residuals, main = "boxplot of residuals")
qqnorm(lm.full$residuals)
qqline(lm.full$residuals)
dev.off()
# Confidence intervals(simultaneou family wise CIs) Bonferroni
library(multcomp)
K = cbind(0, diag(6))
rownames(K) = names(coef(lm.full)) [-1]
lm_lh = glht(lm.full, K)
lm_bf = summary(lm_lh, test= adjusted(type = "bonferroni"))
confint(lm_bf, level = 0.95)

#repeat of the BIC and AIC
lm.sub4=regsubsets(Y ~ ., data=housing, nbest=3, method="exhaustive")
sub4.display = as.data.frame(summary(lm.sub4)$outmat)
sub4.display = cbind(sub4.display,round(summary(lm.sub4)$rsq,4),round(summary(lm.sub4)$adjr2,4),round(summary(lm.sub4)$cp,4),round(summary(lm.sub4)$bic,4))
varnames = c("date","age","MRT","store","lat","long")
names(sub4.display)=c(varnames,"R^2","adj R^2","Cp","BIC")
sub4.display

lm.dat = lm(Y ~.,data=housing)

## Forward selection-based stepwise regression with AIC 
lm.forward.AIC = step(lm.dat,k=2,direction="forward",test="F")

## Backward elimination-based stepwise regression with AIC criterion
lm.backward.AIC = step(lm.dat,k=2,direction="backward",test="F")

## Forward stepwise (forward selection and backward elimination) regression with AIC criterion
lm.both.AIC = step(lm.dat,k=2,direction="both",test="F")


#sub model############################
#Submodel : logY , log MRT and no longtitude 
housing <- read_excel("Real_estate_valuation.xlsx")
colnames(housing) = c('no','date','age','MRT','store','lat','long','Y')
housing<-housing[,2:8]
housing$date<-housing$date-2012.666
housing.sub<-housing[,c(1,2,3,4,5,7)]
housing.sub$MRT<-log(housing.sub$MRT)
housing.sub$Y<-log(housing.sub$Y)
set.seed(123)
training.samples <- housing.sub$Y %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- housing.sub[training.samples, ]
test.data <- housing.sub[-training.samples, ]
x <- model.matrix(Y~., train.data)[,-1]
y <- train.data$Y
# Build the model
model <- lm(Y ~., data = housing.sub)
# Summarize the model
summary(model)
sqrt(sum(model$residuals^2) / model$df)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
# (a) Prediction error, RMSE
RMSE(predictions, test.data$Y)
# (b) R-square
R2(predictions, test.data$Y)

#Simple LR on sub model 
set.seed(123)
lm.sub = lm(Y ~ ., data = housing.sub)
summary(lm.sub)

#Plot residuals
jpeg(file="Residuals_submodel.jpeg", width=10, height=7, unit="in", res=600)
attach(mtcars)
par(mfrow=c(2,3))
plot(lm.sub$fitted.values,lm.sub$residuals,xlab = 'fitted values',ylab = 'residuals',main = 'residual vs fittedvalues')
plot(housing.sub$date,lm.sub$residuals,xlab = 'date',ylab = 'residuals')
plot(housing.sub$age,lm.sub$residuals,xlab = 'age',ylab = 'residuals')
plot(housing.sub$MRT,lm.sub$residuals,xlab = 'MRT',ylab = 'residuals')
plot(housing.sub$store,lm.sub$residuals,xlab = 'store',ylab = 'residuals')
plot(housing.sub$lat,lm.sub$residuals,xlab = 'lat',ylab = 'residuals')
dev.off()
#boX plot and QQ plot
jpeg(file="Box plot and QQ plot_submodel.jpeg", width=10, height=7, unit="in", res=600)
par(mfrow=c(1,2))
boxplot(lm.sub$residuals, main = "boxplot of residuals")
qqnorm(lm.sub$residuals)
qqline(lm.sub$residuals)
dev.off()
# Confidence intervals(simultaneou family wise CIs) Bonferroni

#repeat of the BIC and AIC
lm.sub4=regsubsets(Y ~ ., data=housing.sub, nbest=3, method="exhaustive")
sub4.display = as.data.frame(summary(lm.sub4)$outmat)
sub4.display = cbind(sub4.display,round(summary(lm.sub4)$rsq,4),round(summary(lm.sub4)$adjr2,4),round(summary(lm.sub4)$cp,4),round(summary(lm.sub4)$bic,4))
varnames = c("date","age","MRT","store","lat")
names(sub4.display)=c(varnames,"R^2","adj R^2","Cp","BIC")
sub4.display

lm.dat = lm(Y ~.,data=housing.sub)
## Backward elimination-based stepwise regression with AIC criterion
lm.backward.AIC = step(lm.dat,k=2,direction="backward",test="F")


#submodel 2 ###########################
#Simple LR on sub model 
housing <- read_excel("Real_estate_valuation.xlsx")
colnames(housing) = c('no','date','age','MRT','store','lat','long','Y')
housing<-housing[,2:8]
housing$date<-round(sort(housing$date-2012.667),3)

housing.sub1<-housing[,c(2,3,4,5,7)]
housing.sub1$MRT<-log(housing.sub1$MRT)
housing.sub1$Y<-log(housing.sub1$Y)
lm.sub1 = lm(Y ~ ., data = housing.sub1)
summary(lm.sub1)



#Lets stick to model 1 ##########################
#LOOCV
library(tidyverse)
library(caret)
housing <- read_excel("Real_estate_valuation.xlsx")
colnames(housing) = c('no','date','age','MRT','store','lat','long','Y')
housing<-housing[,2:8]
housing$date<-housing$date-2012.666
housing.sub<-housing[,c(1,2,3,4,5,7)]
housing.sub$MRT<-log(housing.sub$MRT)
housing.sub$Y<-log(housing.sub$Y)
housing.sub[,c(1,2,3,4,5)]<-scale(housing.sub[,c(1,2,3,4,5)])

#LOOCV
train.control <- trainControl(method = "LOOCV")
model_LOOCV <- train(Y ~., data = housing.sub, method = "lm",
               trControl = train.control)
print(model_LOOCV)


#10 fold cross validation 
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
model_k <- train(Y ~., data = housing.sub, method = "lm",
                 trControl = train.control)
print(model_k)


#10 fold CV with repeat 
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
model_K_repeat <- train(Y ~., data = housing.sub, method = "lm",
                        trControl = train.control)
print(model_K_repeat)

#Bootstrap on the new model
train.control <- trainControl(method = "boot", number = 100)
model_boots <- train(Y ~., data = housing.sub, method = "lm",
                     trControl = train.control)
print(model_boots)

#Ridgr regression 
library(glmnet)
set.seed(123)
training.samples <- housing.sub$Y %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- housing.sub[training.samples, ]
test.data <- housing.sub[-training.samples, ]
x <- model.matrix(Y~., train.data)[,-1]
y <- train.data$Y
#alpha=0 for ridge regression 
init_model<-glmnet(x, y, alpha = 0, lambda = NULL , standardize = TRUE)
# Find the best lambda using cross-validation
set.seed(123) 
cv.lambda <- cv.glmnet(x, y, alpha = 0,lambda=exp(seq(-7,6,.1)))
best_lambda<-cv.lambda$lambda.min
plot(cv.lambda)
# Fit the final model on the training data
#model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(model)
# Make predictions on the test data
x.test <- model.matrix(Y ~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$Y),
  Rsquare = R2(predictions, test.data$Y)
)
plot(init_model, xvar = "lambda",label = TRUE)
#plot(cv.lambda$model.fit, 
#    "lambda", label=TRUE)


#decision tree
# Fit the model on the training set
library(tidyverse)
library(caret)
library(rpart)
housing <- read_excel("Real_estate_valuation.xlsx")
colnames(housing) = c('no','date','age','MRT','store','lat','long','Y')
housing<-housing[,2:8]
housing$date<-housing$date-2012.666
housing.sub<-housing[,c(1,2,3,4,5,7)]
housing.sub$MRT<-log(housing.sub$MRT)
housing.sub$Y<-log(housing.sub$Y)
set.seed(123)
training.samples <- housing.sub$Y %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- housing.sub[training.samples, ]
test.data <- housing.sub[-training.samples, ]
x <- model.matrix(Y~., train.data)[,-1]
y <- train.data$Y
set.seed(123)
model <- train(
  Y ~., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model error vs different values of
# cp (complexity parameter)
plot(model)
# Print the best tuning parameter cp that
# minimize the model RMSE
model$bestTune
# Plot the final tree model
jpeg(file="Tree model.jpeg", width=10, height=7, unit="in", res=600)
par(xpd = NA) # Avoid clipping the text in some device
plot(model$finalModel)
text(model$finalModel, digits = 3)
dev.off()
# Decision rules in the model
model$finalModel
# Make predictions on the test data
predictions <- model %>% predict(test.data)
head(predictions)
# Compute the prediction error RMSE
RMSE(predictions, test.data$Y)
R2(predictions, test.data$Y)





#random forest ########################
#random forest
# Fit the model on the training set
library(randomForest)

model_RF <- train(
  Y ~., data = train.data, method = "rf",
  trControl = trainControl("cv", number = 10),
)
# Best tuning parameter mtry
predictions <- model_RF %>% predict(test.data)
head(predictions)
# Compute the average prediction error RMSE
RMSE(predictions, test.data$Y)
R2(predictions, test.data$Y)
plot(model_RF$finalModel)


#PCA
housing <- read_excel("Real_estate_valuation.xlsx")
colnames(housing) = c('no','date','age','MRT','store','lat','long','Y')
housing<-housing[,2:8]
housing$Y<-log(housing$Y)
housing$MRT<-log(housing$MRT)
X = housing[,1:6]
Y = housing[,7]
housing.norm=as.data.frame(scale(housing))
X.norm <- housing.norm[,1:6]
data.pca <- prcomp(X.norm, center = TRUE, scale. = TRUE)
pcs <- as.data.frame(data.pca$x)
data.ols <- cbind(pcs, housing[,7])
colnames(data.ols)= c('PC1','PC2','PC3','PC4','PC5','PC6','Y')

### END 

