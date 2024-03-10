#Load libraries
loadlibraries <- c("Hmisc","dplyr","ggplot2","caret","pROC","ROCR","MASS","Information","ggthemes",
                   "visNetwork","rpart","sparkline","data.table")
installlib <- loadlibraries[!loadlibraries %in% installed.packages()]
for(libs in installlib) install.packages(libs, dependences = TRUE)
sapply(loadlibraries, require, character = TRUE)

#Load the file
setwd("C:/Users/dwaip/Desktop/")
df <- read.csv("german_credit.csv", stringsAsFactors = F)
prop.table(table(df$Creditability))
#Checking the Data
describe(df)
str(df)
names(df)
nrow(df)
# Splitting the data for model
#Splitting the data into train and test
dt = sort(sample(nrow(df), nrow(df)*.7))
train<-df[dt,]
test<-df[-dt,]
rm(list=ls()[! ls() %in% c("train","test","df")])

#Information Value ::
IV <- create_infotables(data=df, y="Creditability", bins=10, parallel=FALSE)
IV_Value = data.frame(IV$Summary)
# Age and Amount IV tables
print(IV$Tables$Age..years., row.names=FALSE)
Age = data.frame(IV$Tables$Age..years.)
plot_infotables(IV, "Age..years.")

#Age Vs Creditability
agebreaks <- c(0,22,25,27,29,32,35,38,43,51,95)
agelabels <- c("0-22","22-25","25-27","27-29","29-32","32-35","35-38","38-43",
               "43-51","51-75")
setDT(df)[ , agegroups := cut(Age..years., 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]
default_cred<- ggplot(df, aes(agegroups, fill = factor(Creditability))) + 
  geom_bar() + theme_few() + xlab("Creditability") + ylab("agegroups") + 
  scale_fill_discrete(name = "Creditability") +theme_few()+ ggtitle("Relationship between Creditability and Age Group")
default_cred 



# Amount IV tables
print(IV$Tables$Credit.Amount, row.names=FALSE)
Amount = data.frame(IV$Tables$Credit.Amount)
plot_infotables(IV, "Credit.Amount")


# Changing to factor
df <- as.data.frame(df)
F=c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20)
for(i in F) df[,i]=as.factor(df[,i])

plot.categoric <- function(cols, df){
  for (col in cols) {
    order.cols <- names(sort(table(df[,col]), decreasing = TRUE))
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 0, size=12))
    
    print(num.plot)
  }
}



plot.categoric("Creditability", df)
plot.categoric("Payment.Status.of.Previous.Credit",df)
plot.categoric("Purpose",df)
plot.categoric("Length.of.current.employment" ,df)
plot.categoric("Occupation" ,df)

# Variables 
var = names(train[,!(names(train) %in% c("Creditability"))])
var
variables<-reformulate(var, response = 'Creditability')
variables


#Logistic model
# Null Model First
fit1 <- glm(variables, data = train, family = binomial)
coef(fit1)
step_mod <- fit1 %>% stepAIC(trace = TRUE)
coef(step_mod)

# Results
probabilities <- fit1 %>% predict(train, type = "response")
train$prob = probabilities
g1 <- roc(Creditability ~ train$prob,data = train)
print(coords(g1, "best"))
train$prediction=ifelse(probabilities>=coords(g1, "best")[1],1,0)
train$prediction <- as.factor(train$prediction)
train$Creditability<-as.factor(train$Creditability)
confusionMatrix(train$prediction,train$Creditability)


# Make predictions :Test Data 
probabilities <- fit1 %>% predict(test, type = "response")
test$prob = probabilities
g1 <- roc(Creditability ~ train$prob,data = train)
plot(g1)


test$prediction=ifelse(probabilities>=0.6279582,1,0)
test$prediction <- as.factor(test$prediction)
test$Creditability<-as.factor(test$Creditability)
confusionMatrix(test$prediction,test$Creditability)

# Logistic model Ends Here 
lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

dt = lift(train$Creditability , train$prediction, groups = 10)
dt
graphics::plot(dt$bucket, dt$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket")



# Random Forest Model 
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
## Training
modFit <- train(variables,method="rf",data=train)
modFit


#Decision Trees
set.seed(1)
TreeModel <- rpart(Creditability ~ ., data = train)
visTree(TreeModel, main = "Credibility lassification Tree", width = "100%")
# library(rpart.plot)
# prp(TreeModel, type = 2, extra = 1)
