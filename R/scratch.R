
#### Useful packages
#install.packages('data.table')
#install.packages('MASS')
#install.packages('ggplot2')


#Load the necessary packages
library(data.table)
library(ggplot2)
library(MASS)
library(randomForest)
library(glmnet)


relates <- fread("./R/lab notebook/data/relation.csv")
counts_sf <- fread("./R/lab notebook/data/sfmta_counts.csv")
counts_sl <- fread("./R/lab notebook/data/streetlight_counts.csv")
landuse <- fread("./R/lab notebook/data/landuse.csv")

counts_merged <- merge(relates, counts_sf, by="counterid")
counts_merged <- merge(counts_merged, counts_sl, by=c("taz_id", "day_type", "day_part","month"), all = F)
counts_merged <- merge(counts_merged, landuse, by='taz_id')




# COlumns to remove
drop_cols <- c('taz_id','counterid','lat','location','lon', 'day_type')

#Let's just do the peak weekday hours
counts <- counts_merged[, !drop_cols, with=F]

#Drop the NA in any count column
counts = counts[!is.na(sfmta_count) & !is.na(strtlght_count),]






minmax <- function(x) ( x - min(x) ) / ( max(x) - min(x) )
z_score <- function(x) (x-mean(x))/sd(x)
z_cols <- !(colnames(counts) %in% c('sfmta_count','day_type','day_part','month'))
z_cols <- colnames(counts)[z_cols]

#Make a fresh copy
counts_scaled <- counts
counts_scaled[ , (z_cols) := lapply(.SD, z_score), .SDcols = z_cols]
#counts_scaled[ , (z_cols) := lapply(.SD, minmax), .SDcols = z_cols]
#counts_scaled[ , (c('day_type','day_part','month')) := lapply(.SD, as.factor), .SDcols=c('day_type','day_part','month')]







######
index = sample(1:nrow(counts_scaled), .8*nrow(counts_scaled)) 
train = counts_scaled[index,] # Create the training data 
test = counts_scaled[-index,] # Create the test data


# Model Matrix
mm <- model.matrix( ~ .,  data = counts_scaled[,!'sfmta_count'])

#### LASSO
cv.glmmod = cv.glmnet(x=mm, y=counts_scaled$sfmta_count)
plot(cv.glmmod)

lambda = cv.glmmod$lambda.1se # the value of lambda used by default
coefs = as.matrix(coef(cv.glmmod)) # convert to a matrix (618 by 1)
ix = which(abs(coefs[,1]) > 0)
coefs[ix,1, drop=FALSE]




#####
rffit <- randomForest(
  sfmta_count ~ .,
  data=train,
  ntrees=10
)

test$rf_count <- predict(rffit, newdata=test)
plot(test$sfmta_count, test$rf_count)


#Neural Net
library(nnet)
nnfit <- nnet(sfmta_count ~ ., data=train, size=10, maxit=10000, trace=T, linout=T)
test$nn_count <- predict(nnfit, newdata=test)
plot(x=test$nn_count, y=test$sfmta_count)


#
library(neuralnet)
mm <- model.matrix( ~ sfmta_count + .,  data = train)
f <- as.formula(paste('sfmta_count', paste(colnames(mm)[-1:-2], collapse=" + "), sep=" ~ "))

nnfit <- neuralnet(f, data = mm, hidden=5, linear.output = T,
                   lifesign='full', act.fct = 'logistic', stepmax = 1e6, learningrate = 1e-3)
plot(nnfit)
test$nn_count <- predict(nnfit, test)
plot(x=test$nn_count, y=test$sfmta_count)


#SVM
library(e1071)
svmfit = svm(sfmta_count ~ ., data = train, kernel = "linear", cost = 10, scale = FALSE)
test$svm_count <- predict(svmfit, test)
plot(x=test$svm_count, y=test$sfmta_count)







