
##BRT to predict Wind damage occurrence in North Karelian forest stands##


library(dismo)
library(gbm)
library(caTools)


setwd("C:\\Users\\liangch\\Desktop\\MyDesktopFile\\phd_things\\SummerSchool2023\\GBM")

standsNK <- read.csv("./BRTWindFinland_data2.csv")


#Data wrangling

standsNK$X <- NULL
standsNK$mainsp <- as.factor(standsNK$mainsp)
standsNK$developmentc <- as.factor(standsNK$developmentc)
standsNK$fertilityc <- as.factor(standsNK$fertilityc)
standsNK$soiltype <- as.factor(standsNK$soiltype)


sample <- sample.split(standsNK$poly_ID, SplitRatio = .75)
train <- subset(standsNK, sample == TRUE)
test <- subset(standsNK, sample == FALSE)



#BRT 

BRT.model <- gbm(storm ~ dam_G + developmentc + dam_age + soiltype + dam_ratioHD +
                       mainsp, data = train, distribution = "bernoulli",
                     interaction.depth = 5, bag.fraction = 0.75, shrinkage = 0.01,
                     cv.folds = 10)

print(BRT.model) #100 trees
summary(BRT.model, plot = TRUE) #relative influence of the predictors: dam_G(26.78), developmentc(25.69), etc

BRT.model$cv.fitted #look at the fitted values, log odds scale


best.iter <- gbm.perf(BRT.model, method = "cv") #check performance using 10-fold cross validation
gbm.perf(BRT.model, method = "cv") #100



#Marginal dependence  

plot.gbm(BRT.model, i.var = 1, type = "response")
plot.gbm(BRT.model, i.var = 1, type = "link")
plot.gbm(BRT.model, i.var = 2, type = "response")

plot(BRT.model, i.var = 1, n.trees = best.iter)

plot(BRT.model, i.var = c("dam_G", "developmentc"), n.trees = best.iter, type = "response")
plot(BRT.model, i.var = c("dam_G", "dam_age"), n.trees = best.iter, type = "response")
plot(BRT.model, i.var = c("dam_G", "dam_age", "soiltype"), n.trees = best.iter, type = "response")



#Predicting wind probability using BRT

BRTpred_train <- predict(BRT.model, newdata = train, type = "response") #predicting with training data

head(data.frame("Actual" = train$storm, "PredictedProbability" = BRTpred_train))

train <- cbind(train, BRTpred_train)

write.csv(train, "train_predictions.csv") #export data to be joined in arcgis or qgis, to map predictions



BRTpred_test <- predict.gbm(BRT.model, newdata = test, type = "response") #predicting with test data

test <- cbind(test, BRTpred_test)

write.csv(train, "test_predictions.csv")


######rf

library(randomForest)

rf <- randomForest(storm ~ dam_G + developmentc + dam_age + soiltype + dam_ratioHD +
                                        mainsp, data = train,
                   importance = T,
                   ntree = 500,mtry =3,nodesize =1)

summary(rf)
