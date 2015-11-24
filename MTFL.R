## OBjective is to develop a 'useful' model that takes in wallnode+wearnode' data and uses to determine feature selection with minimal damage due to imputation
## Logic involves following steps:

### What is the contribution
## (1) Several Integration outputs considered as datasets (max signal, weighted signal, trilateration based). You know what I mean
## (2) MTFL*:
##### (a) Divide the data into subsets (manual for now) based on missing chunks
##### (b) Predict RMSSD for each chunk (few models try with CV)
##### (c) Wrapper based feature selection for each
##### (d) Get the support & final features table with Support and stuff
##### (e) (Optional) Logic for final featue ranking and test(actual) data predictions
##### (f) Try these for interactions and higher order and things!!!
##### (g) Use SGB Lasso, RF and Reg tree (for now)

## Additional Procedure 
## Use reduced modeling (Saar approach), Only imputation approach and MTL approach to predict
### Use the MTFL approach and prove it is better

library(data.table)
library(Hmisc)


data_in <- read.csv("Wear_wall_integrated.csv")

#data_sens <- data_in

# d <- dat$exampleColumn != ""
#d <- apply(data_in,1,function(x) is.na(x))
#new_DF <- DF[rowSums(is.na(DF)) > 0,]
d <- rowSums(is.na(data_in[1:100,]))
mp <- matrix(c(0,2,2,0,2,1,0,1,2,1,1,0,1,3,0,0),4,4,byrow=T)
round(as.data.frame(eigen(mp)$),2)
det(mp)

data_present <- data.frame(matrix(NA, nrow = nrow(data_in), ncol = ncol(data_in)))

for(row in 1:nrow(data_in))
{
  for(col in 1:ncol(data_in))
  {
  if(!is.na(data_in[row,col]))
  {
    data_present[row,col] <- 1
  }
  else { data_present[row,col] <- 0 }
  }
}

names(data_present) <- names(data_in)
## Use hierarchical clustering to determine membership

library(NbClust)
library(mclust)
# DOes not work. Go for simple hierchical clust
# o <- NbClust(data_present, method = "kmeans",distance="euclidean")

## Simply use kmeans for now 
o <- kmeans(data_present,4)

## Hclust
d <- dist(data_present, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")
## mclust
a <- data_present[1:1000,]
fit <- Mclust(a)
plot(fit) # plot results 
summary(fit)

clus <- list()
qual <- vector()
clus_centers <- list()
clus_size <- list()

for(i in 2:8)
{
  clus[[i]] <- kmeans(data_present,i)
  k_man <- clus[[i]]
  qual[i] <- (100-k_man$tot.withinss/k_man$totss*100)
  clus_centers[[i]] <- k_man$centers
  clus_size[[i]] <- k_man$size
}  

plot(qual,xlab = c("Number of clusters"),ylab=c("Goodness-of-fit"))

## In this case, 3 clusters are enough

set.seed(34333)
member_clusters <- kmeans(data_present,3)
members <- member_clusters$cluster
member_clusters$centers
member_clusters$size
## Once the cluster memberships are determined
data_subs <- split(data_in,members)

## First has both wear and wall, second has only wear, third has only wall 
data_both <- data_subs[[1]]

data_wear <- data_subs[[2]]
data_wall <- data_subs[[3]]

## Manually allocate columns as X & Y
hist(data_both$Activity,breaks=1000,xlim=c(0,0.1))

names(data_both)
 
## Impute for each- General imputation function 

impute_all <- function(data_in)
{
  for(j in 1:ncol(data_in))
  {
    if(is.factor(data_in[,j]))
    {
      for(k in 1:nrow(data_in))
      {
        if(is.na(data_in[k,j]))
        {
          data_in[k,j] <- data_in[which.max(data_in[,j]),j]
          #data_in$ToD[which.max(data_in$ToD)]
        }
      }
      data_in[,j] <- as.character(data_in[,j])  
      data_in[,j] <- factor(data_in[,j])  
    }
    else
    {
      data_in[,j]  <- as.numeric(round(impute(data_in[,j],mean),6))
    }  
    
  }
  return(data_in)
}

data_both_in <- impute_all(data_both[,5:25])
data_both_out <- as.numeric(round(impute(data_both[,'RMSSD'],mean),6))

data_wear_in <- impute_all(data_wear[,5:15])
data_wear_out <- as.numeric(round(impute(data_wear[,'RMSSD'],mean),6))

data_wall_in <- impute_all(data_wall[,c(5:10,16:25)])
data_wall_out <- as.numeric(round(impute(data_wall[,'RMSSD'],mean),6))


## Predictions for each 

library(car)
library(frbs)
library(caret)
library(klaR)
library(party)

train_control <- trainControl(method="cv", number=10)  # 10 fold CV

train_control_s <- trainControl(method="cv", number=5,repeats =1, returnResamp = 'none')

model_pred <- function(datain,method)
{
  model <- train(Y~., data=datain, trControl=train_control, method=as.character(substitute(method)))
  return (model)
}

print_RMSE <- function(method)
{
  predictions <- predict(method, X)
  print ((mean((predictions - Y)^2))^0.5 )
}

print_MAPE <- function(method)
{
  predictions <- predict(method, X)
  print (mean(abs(predictions - Y)/Y*100)) 
}

scale_num <- function(data_in)
{
data_num <- data_in[,!names(data_in)%in%c("ToD","DoW","Gender")]
data_cat <- data_in[,names(data_in)%in%c("ToD","DoW","Gender")]
data_nums <- as.data.frame(scale(as.matrix(data_num)),scale=T,center=T)
return (cbind(data_nums,data_cat)) 
}

X <- scale_num(data_both_in)
Y <- data_both_out

datam <- data.frame(X,Y)

set.seed(23422)

## lasso

system.time(
  model.lasso <- model_pred(datam,lasso)
)
varImp(model.lasso) 

'''
## Conditional inference random forest
system.time(
  model.cforest <- model_pred(datam,cforest)
)

## Conditional inference regression tree ##

system.time(
  model.ctree <- model_pred(datam,ctree)
)

'''
# Stochastic Gradient Boosting

system.time(
  model.gbm <- model_pred(datam,gbm)
)

varImp(model.gbm) 
# Multivariate Adaptive Regression Splines

system.time(
  model.gcvEarth <- model_pred(datam,gcvEarth)
)
varImp(model.gcvEarth)

# Random forest

system.time(
  model.rf <- model_pred(datam,rf)
)
rf <- model.rf$finalModel
importance(rf)

c(
  print_RMSE(model.lasso),
  print_RMSE(model.gbm),
  print_RMSE(model.gcvEarth),
  print_RMSE(model.rf)
)

c(
  print_MAPE(model.lasso),
  print_MAPE(model.gbm),
  print_MAPE(model.gcvEarth),
  print_MAPE(model.rf)
)

## We therefore choose Randomforest (popular demand, acceptibility, better predictions)
## But we test it for faster modeling

model_pred <- function(datain,method)
{
  model <- train(Y~., data=datain, trControl=train_control, method=as.character(substitute(method)))
  return (model)
}

datam2 <- data.table(datam)

system.time(
rf_model_f <-train(Y~.,data=datam,method="rf",
                trControl=train_control_s,
               allowParallel=TRUE, ntree=100)
)

rf2 <- rf_model_f$finalModel
importance(rf2)

##Use repeats =1, returnResamp = 'none' ?'

## Wrapper functions  ## NN as we are giving feature importance rather than selection
## Support 
## Heuristic for features

#names(getModelInfo())


