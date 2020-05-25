library(dplyr)
library(glmnet)
library(rpart)
library(randomForest)
library(tree)
library(e1071)
library(mctest)
library(caret)
library(GGally)
library(funModeling)
library(GGally)

data <- read.csv('C:/Users/varal/OneDrive - George Mason University/Desktop/MS/SYST 568/project/communities and crime unnormalized data set (2)/crimedata.csv')
# column names
attributes <- names(data)

# Replacing ? with NA in 
data[data == '?'] <- NA
str(data)

# Count of missing values
fNA_counts <- sapply(data, function(x) sum(is.na(x)))

# Handling missing values in community code
table(data$communityname)
# by observing the values we can say that there are only 2% records that have unique value. It means that all the data in communitycode
# is going to unique and there is no point of using this data to build a model
df <- subset(data,select = -c(communityCode))



# Removing columns that has 80% of data missing
df <- subset(df,select = -c(LemasSwornFT,LemasSwFTPerPop ,LemasSwFTFieldOps,LemasSwFTFieldPerPop,LemasTotalReq,LemasTotReqPerPop,PolicReqPerOffic,PolicPerPop,RacialMatchCommPol,PctPolicWhite, PctPolicBlack,
                            PctPolicHisp, PctPolicAsian,PctPolicMinor,OfficAssgnDrugUnits,NumKindsDrugsSeiz,PolicAveOTWorked,PolicCars,PolicOperBudg,LemasPctPolicOnPatr,LemasGangUnitDeploy,PolicBudgPerPop))

df <- subset(df,select = -c(countyCode))
# Count of missing values
NA_counts <- sapply(df, function(x) sum(is.na(x)))

# Handling Missing data in country code and community code
table(df$state)

# Handling missing data in Rapes attribute
df$rapes <- as.numeric(df$rapes)
hist(df$rapes)
rapes_group = data.frame()
x = data.frame()
y = data.frame()
rapes_group = aggregate(df$rapes,by=list(df$state), FUN = sum, na.rm = TRUE)
x = aggregate(rapes ~ state , data=df, function(sumna) {sum(is.na(sumna))}, na.action = NULL)
y = aggregate(df$rapes,by=list(df$state), FUN = length)
rapes_group = merge(rapes_group,x,by.x = 'Group.1',by.y='state')
rapes_group = merge(rapes_group,y,by.x = 'Group.1',by.y='Group.1')
rename(rapes_group,c("meanvalue"="x.x","missing_count"="rapes","records_count"="x.y"))
# From these results we can see there are 40 records of IL and 108 records of MI and all there data is missing . 
# from the graph we can say that data is skewed towards right side. so, replacing it with median value would be better option
df$rapes[is.na(df$rapes)] <- median(df$rapes, na.rm=TRUE)
df$rapesPerPop <- as.numeric(df$rapesPerPop)
df$rapesPerPop <- as.numeric(df$rapesPerPop)
df$rapesPerPop <- ifelse(is.na(df$rapesPerPop),((100000)/df$population) * df$rapes , df$rapesPerPop)


# Handling Missing data in assaults
#Assaults attribute has data missing in only 13 records
df$assaults <- as.numeric(df$assaults)
hist(df$assaults)
# From the histogram we can see the results and fill missing values with median
df$assaults[is.na(df$assaults)] <- median(df$assaults, na.rm=TRUE)
df$assaultPerPop <- as.numeric(df$assaultPerPop)
df$population <- as.numeric(df$population)
df$assaultPerPop <- ifelse(is.na(df$assaultPerPop),((100000)/df$population) * df$assaults , df$assaultPerPop)


#Handling missing data in burglaries
df$burglaries <- as.numeric(df$burglaries)
df$burglaries[is.na(df$burglaries)] <- median(df$burglaries, na.rm=TRUE)
df$burglPerPop <- as.numeric(df$burglPerPop)
df$burglPerPop <- ifelse(is.na(df$burglPerPop),((100000)/df$population) * df$assaults , df$burglPerPop)

# Handling missing data in larcenies
df$larcenies <- as.numeric(df$larcenies)
df$larcenies[is.na(df$larcenies)] <- median(df$larcenies, na.rm=TRUE)
df$larcPerPop <- as.numeric(df$larcPerPop)
df$larcPerPop <- ifelse(is.na(df$larcPerPop),((100000)/df$population) * df$larcenies , df$larcPerPop)

# Handling data in Autothefts
df$autoTheft <- as.numeric(df$autoTheft)
df$autoTheft[is.na(df$autoTheft)] <- median(df$autoTheft, na.rm=TRUE)
df$autoTheftPerPop <- as.numeric(df$autoTheftPerPop)
df$autoTheftPerPop <- ifelse(is.na(df$autoTheftPerPop),((100000)/df$population) * df$autoTheft , df$autoTheftPerPop)


# Handling missing data in arsons
df$arsons <- as.numeric(df$arsons)
hist(df$arsons)
# replacing values in arsons with mode
df$arsons[is.na(df$arsons)] <- mean(df$arsons,na.rm = TRUE)
df$arsonsPerPop <- as.numeric(df$arsonsPerPop)
df$arsonsPerPop <- ifelse(is.na(df$arsonsPerPop),((100000)/df$population) * df$arsons , df$arsonsPerPop)

# Only value of 1 record is missing for robberies
df$robberies = as.numeric(df$robberies)
df$robberies[is.na(df$robberies)] <- mean(df$robberies,na.rm = TRUE)
df$robbbPerPop <- as.numeric(df$robbbPerPop)
df$robbbPerPop <- ifelse(is.na(df$robbbPerPop),((100000)/df$population) * df$robberies , df$robbbPerPop)

library(plyr)
# Replacing character value in states with FIPS codes
df$state <- revalue(df$state, c("AK"="2"))
df$state <- revalue(df$state, c("AL"="1"))
df$state <- revalue(df$state, c("AZ"="4"))
df$state <- revalue(df$state, c("CA"="6"))
df$state <- revalue(df$state, c("CO"="8"))
df$state <- revalue(df$state, c("CT"="9"))
df$state <- revalue(df$state, c("DE"="10"))
df$state <- revalue(df$state, c("FL"="12"))
df$state <- revalue(df$state, c("GA"="13"))
df$state <- revalue(df$state, c("IA"="19"))
df$state <- revalue(df$state, c("ID"="16"))
df$state <- revalue(df$state, c("IL"="17"))
df$state <- revalue(df$state, c("IN"="18"))
df$state <- revalue(df$state, c("KS"="20"))
df$state <- revalue(df$state, c("KY"="21"))
df$state <- revalue(df$state, c("LA"="22"))
df$state <- revalue(df$state, c("MA"="25"))
df$state <- revalue(df$state, c("MD"="24"))
df$state <- revalue(df$state, c("ME"="23"))
df$state <- revalue(df$state, c("MI"="26"))
df$state <- revalue(df$state, c("MN"="27"))
df$state <- revalue(df$state, c("MO"="29"))
df$state <- revalue(df$state, c("MS"="28"))
df$state <- revalue(df$state, c("NC"="37"))
df$state <- revalue(df$state, c("ND"="38"))
df$state <- revalue(df$state, c("NH"="33"))
df$state <- revalue(df$state, c("NJ"="34"))
df$state <- revalue(df$state, c("NM"="35"))
df$state <- revalue(df$state, c("NV"="32"))
df$state <- revalue(df$state, c("NY"="36"))
df$state <- revalue(df$state, c("OH"="39"))
df$state <- revalue(df$state, c("OK"="40"))
df$state <- revalue(df$state, c("OR"="41"))
df$state <- revalue(df$state, c("PA"="42"))
df$state <- revalue(df$state, c("RI"="44"))
df$state <- revalue(df$state, c("SC"="45"))
df$state <- revalue(df$state, c("SD"="46"))
df$state <- revalue(df$state, c("TN"="47"))
df$state <- revalue(df$state, c("TX"="48"))
df$state <- revalue(df$state, c("UT"="49"))
df$state <- revalue(df$state, c("VA"="51"))
df$state <- revalue(df$state, c("VT"="50"))
df$state <- revalue(df$state, c("WA"="53"))
df$state <- revalue(df$state, c("WI"="55"))
df$state <- revalue(df$state, c("WV"="54"))
df$state <- revalue(df$state, c("WY"="56"))
df$state <- revalue(df$state, c("DC"="0"))
df$state <- revalue(df$state, c("AR"="5"))
df$state<- as.numeric(df$state)

# violent crimes are: murder, rape, robbery and assault, non violent crimes are: burglaries larcencies auto thefts and arsons
df$ViolentCrimesPerPop <- as.numeric(df$ViolentCrimesPerPop)
df$ViolentCrimesPerPop <- ifelse(is.na(df$ViolentCrimesPerPop),df$murdPerPop + df$rapesPerPop + df$robbbPerPop + df$assaultPerPop, df$ViolentCrimesPerPop)
df$nonViolPerPop <- as.numeric(df$nonViolPerPop)
df$nonViolPerPop <- ifelse(is.na(df$nonViolPerPop),df$burglPerPop + df$larcPerPop + df$autoTheftPerPop + df$arsonsPerPop, df$nonViolPerPop)
violentdata$OtherPerCap <- as.numeric(violentdata$OtherPerCap)
nonviolentdata$OtherPerCap <- as.numeric(nonviolentdata$OtherPerCap)
violentdata = na.omit(violentdata)


# Data for predicting violent crimes
nonviolentdata <- subset(df,select = -c(murders,murdPerPop,rapes,rapesPerPop,robberies,robbbPerPop,assaults,assaultPerPop,ViolentCrimesPerPop))
violentdata <- subset(df,select = -c(burglaries,burglPerPop,larcenies,larcPerPop,autoTheft,autoTheftPerPop,arsons,arsonsPerPop,nonViolPerPop))

violentdata <- subset(violentdata,select = -c(communityname))
nonviolentdata <- subset(nonviolentdata,select = -c(communityname))
violentdata$OtherPerCap <- as.numeric(violentdata$OtherPerCap)
nonviolentdata$OtherPerCap <- as.numeric(nonviolentdata$OtherPerCap)

ggcorr(violentdata)






## check skewness and PCA, perform box cox transformation to remove skewness
skew = apply(violentdata,2,skewness)
trans = preProcess(violentdata, method = c("BoxCox","center","scale","pca"))
transformed = predict(trans,violentdata)  # Applying the transformed data

main_data = violentdata
transformed$ViolentCrimesPerPop = violentdata$ViolentCrimesPerPop

########### train test split ###################################################
violentdata = transformed
violentdata = na.omit(violentdata)
train <- sample(1:nrow(violentdata),nrow(violentdata)*0.8)
x = model.matrix(ViolentCrimesPerPop~.,violentdata)[,-1]
y = violentdata$ViolentCrimesPerPop
grid = 10^seq(10,-2,length=100)
x_train = model.matrix(ViolentCrimesPerPop~.,violentdata[train,])[,-1]
y_train = violentdata[train,"ViolentCrimesPerPop"]
x_test = model.matrix(ViolentCrimesPerPop~.,violentdata[-train,])[,-1]
y_test = violentdata[-train,"ViolentCrimesPerPop"]

#################### Linear Regression ####################################
lm.model <- lm(ViolentCrimesPerPop ~.,data = violentdata, standaradize = TRUE)
summary(lm.model)
par(mfrow = c(2,2))
plot(lm.model)

# R-square :0.9692
# Adjusted R-square : 0.9686
# Residual Square error : 125.5

##################### Lasso Regression ###################################
lasso.mod = cv.glmnet(x,y,alpha=1,lambda = grid)
plot(lasso.mod)  # From this plot we can see that best lambda can be obtained by considering 46 to 85 attributes.for log lambda between 0 and 3.5
cv.out = cv.glmnet(x[train,],y[train],alpha = 1) # fit lasso model on train data
plot(cv.out)
plot(glmnet(x,y),label = TRUE) # each curve is for each variable, axis above is number of non-zero coefficients 
plot(glmnet(x,y),label = TRUE)

lambdas = 10^seq(-3.5,2, by = 0.1)
lasso_reg <- cv.glmnet(x, y, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 10)
# Best
lambda_best <- lasso_reg$lambda.min
lambda_best
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE) # fit the model on train for best lambda
lasso_analyize <- coef(lasso_model,s = lambda_best)


prediction <- lasso_model %>% predict(x_test) %>% as.vector()
RMSE = RMSE(prediction, y_test)
Rsquare = R2(prediction,y_test)

# R-square : 0.9694
# RMSE : 130.95

############################# Ridge Regression ##############################
ridge.mod = cv.glmnet(x,y,alpha=0,lambda = grid)
plot(ridge.mod)  # From this plot we can see that best lambda can be obtained by considering 46 to 85 attributes.for log lambda between 0 and 3.5
cv.out = cv.glmnet(x[train,],y[train],alpha = 0) # fit lasso model on train data
plot(cv.out)
plot(glmnet(x,y),label = TRUE) # each curve is for each variable, axis above is number of non-zero coefficients 
plot(glmnet(x,y),label = TRUE)

lambdas = 10^seq(2,3.8, by = 0.1)
ridge_reg <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, standardize = TRUE, nfolds = 10)
# Best
lambda_best <- ridge_reg$lambda.min
lambda_best
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = lambda_best, standardize = TRUE) # fit the model on train for best lambda
ridge_analyize <- coef(lasso_model)


prediction <- ridge_model %>% predict(x_test) %>% as.vector()
RMSE = RMSE(prediction, y_test)
Rsquare = R2(prediction,y_test)

# R-square : 0.9692
# RMSE : 167.13


############################# Regression tree #########################

re.tree <- tree(ViolentCrimesPerPop~., data=main_data)
summary(re.tree)
plot(re.tree)
text(re.tree,pretty = 0)

cvtree <- cv.tree(re.tree)
plot(cvtree$size, cvtree$dev, type = 'b')

prunetree <- prune.tree(re.tree,best = 5)
plot(prunetree)
text(prunetree, pretty = 0)

# using unpruned trees to make test predictions
yhat = predict(re.tree,newdata = Violent2[-train,])
treetest <- violentdata[-train,"ViolentCrimesPerPop"]
plot(yhat,treetest)
abline(0,1)
mean((yhat-treetest) ^ 2) 
Rsquare = R2(yhat,treetest)

yhat1 = predict(prunetree,newdata = main_data[-train,])
treetest <- main_data[-train,"ViolentCrimesPerPop"]
plot(yhat1,treetest)
abline(0,1)
mean((yhat1-treetest) ^ 2) 
Rsquare = R2(yhat1,treetest)


#################### Random Forest #################
bag.violentdata <- randomForest(ViolentCrimesPerPop~., data = main_data[train,],na.action=na.exclude)
bag.violentdata # 46.14% of accuracy is explained
yhat.bag <- predict(bag.violentdata, newdata = main_data[-train,])
plot(yhat.bag, treetest)
abline(0,1)
mean((yhat.bag-treetest)^2) # 941386

# Extracts variable importance (Mean Decrease in Gini Index)
# Sorts by variable importance and relevels factors to match ordering
var_importance <- data_frame(variable=setdiff(colnames(main_data), "ViolentCrimesPerPop"),
                             importance=as.vector(importance(bag.violentdata)))
var_importance <- arrange(var_importance, desc(importance))
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

importance(rf.boston)

varImpPlot(bag.violentdata)

rf.violent <- randomForest(ViolentCrimesPerPop~assaultPerPop+rapes+state+robbbPerPop+rapesPerPop, data = violentdata, na.action = na.exclude)
rf.violent # 51.93% is explained
