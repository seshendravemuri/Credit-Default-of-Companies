setwd("F:/R Programming/FRA/FRA/Project")
getwd()

install.packages(c("pscl","tidyr","purr","gridExtra","reshape","DMwR","Metrics","car","data.table","caret","scales",
                   "caTools","rpart","gmodels","InformationValue","ROCR","pROC"))

library(pscl)
library(tidyr)
library(purrr)
library(gridExtra)
library(reshape)
library(DMwR)
library(Metrics)
library(car)
library(data.table)
library(caret)
library(dplyr)
library(ggplot2)
library(scales)
library(caTools)
library(rpart)
library(gmodels)
library(InformationValue)
library(ROCR)
library(pROC)
library(openxlsx)

##Function:plotstackedbar()

plotstackedbar <- function(data, column, target, strTarget, strCol, axisLabel = "H"){
  title <- paste(strTarget," Vs ",strCol,sep = " ")
  data <- data[order(column),]
  if (axisLabel == "V") {
    ggplot(data, aes(x = as.factor(column),fill=factor(target))) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))),
                stat = "count", position=position_stack(vjust=0.5)
      ) +
      scale_y_continuous(labels = percent) +
      labs(title = title, y = strTarget, x = strCol) +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  }
  else {
    ggplot(data, aes(x = as.factor(column),fill=factor(target)))
    +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))),
                stat = "count", position=position_stack(vjust=0.5)
      ) +
      scale_y_continuous(labels = percent) +
      labs(title = title, y = strTarget, x = strCol) + theme(axis.text.x=element_text(angle=30,hjust=1,vjust=0.5))
  }
}

##Function: plothist()
plothist <-function(data, column){
  x1 = data[,column]
  title = paste('Histogram for ',column, sep = " ")
  ggplot(data, aes(x = x1)) +
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") +
    labs(title = title, x = column)
}

##Function: panel.cor()
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  test <- cor.test(x,y)
  txt1 <- format(c(test$p.value, 0.123456789), digits=digits)[1]
  txt1 <- paste(prefix, txt1, sep="")
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  text(0.5, 0.5, txt, cex = cex*r*1.75)
  text(1.5, 1.5, txt1, cex = cex*r*1.75)
  text(.8, .8, Signif, cex=cex, col=2)
}

##Function: outlier_cap(x)
percentcap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])
  }
  return(x)
}

##Loading Data

data <- read.xlsx("raw-data.xlsx")
data["default"]<-ifelse(data["Networth.Next.Year"]>0,c("0"),c("1"))

str(data)
names(data)

dim(data)

make.names(colnames(data))

plot_intro(data)

#For Testing
testdata <- read.xlsx("validation_data.xlsx")

str(testdata)
names(testdata)
dim(testdata)
make.names(colnames(testdata))
#Missing Values Identification

colSums(is.na(data))

#Calculating missing values %
n <- dim(data)[1]
m <- dim(data)[2]


miss_values_data <- data.frame()
for (i in 1:m){
  miss_values_data_i <- data.frame()
  x <- colnames(data)[i]
  valu <- sum(is.na(data[,x])) / n
  name <- x
  df1 <- data.frame(name = name, val = valu)
  miss_values_data <- rbind(miss_values_data, df1)
}
print(miss_values_data)

threshold<- 0.40

print(miss_values_data[miss_values_data$val > 0.40,c("name","val")],row.names = F)

##Remove columns with count more than threshold

print("\n Remove columns with missing values above threshold limit")

removeColumns <- names(data) %in% c("Deposits.(accepted.by.commercial.banks)", "Investments", "PE.on.BSE")
data <- data[!removeColumns]

#Calculating missing % on Testing data

a <- dim(testdata)[1]
s <- dim(testdata)[2]

miss_values_test_data <- data.frame()
for (i in 1:s) {
  miss_values_test_data_i <- data.frame()
  x <- colnames(testdata)[i]
  valu <- sum(is.na(testdata[,x])) / a
  name <- x
  df2 <- data.frame(name = name, val = valu)
  miss_values_test_data <- rbind(miss_values_test_data, df2)
}

print(miss_values_test_data)

threshold <- 0.40
print(miss_values_test_data[miss_values_test_data$val > 0.40,c('name','val')],row.names=F)


removeColumns_test <- names(testdata) %in% c("Deposits.(accepted.by.commercial.banks)", "PE.on.BSE")
testdata <- testdata[!removeColumns_test]


cat("\n Missing Value\n")
sapply(data, function(x) sum(is.na(x)))


cleandata <- subset(data, select = -c(1))
cleandata <- na.omit(cleandata)
dim(cleandata)
names(cleandata)

table(cleandata$default)

##Missing Values for Testdata

sapply(testdata, function(x) sum(is.na(x)))

cleantestdata <- subset(testdata, select = -c(1))
cleantestdata <- na.omit(cleantestdata)
dim(cleantestdata)

names(cleantestdata)

colnames(cleantestdata)[1]<-"default"
table(cleantestdata$default)


##Variable Creation
cleandata["Total.Income.Ratio"] <- round(cleandata["Total.income"]/cleandata["Total.assets"],2)
cleandata["Change.in.stock.Ratio"] <- round(cleandata["Change.in.stock"]/cleandata["Total.income"],2)
cleandata["Total.Expenses.Ratio"] <- round(cleandata["Total.expenses"]/cleandata["Total.income"],2)
cleandata["Profit.after.Tax.Ratio"] <- round(cleandata["Profit.after.tax"]/cleandata["Total.assets"],2)
cleandata["PBDITA.Ratio"] <- round(cleandata["PBDITA"]/cleandata["Total.assets"],2)
cleandata["PBT.Ratio"] <- round(cleandata["PBT"]/cleandata["Total.assets"],2)
cleandata["Cash.Profit.Ratio"] <- round(cleandata["Cash.profit"]/cleandata["Total.assets"],2)
cleandata["Sales.Ratio"] <- round(cleandata["Sales"]/cleandata["Total.assets"],2)
cleandata["Income.from.financial.services.ratio"] <- round(cleandata["Income.from.financial.services"]/cleandata["Total.assets"],2)
cleandata["Other.income.Ratio"] <- round(cleandata["Other.income"]/cleandata["Total.assets"],2)
cleandata['Total.capital.Ratio'] <- round(cleandata['Total.capital']/cleandata['Total.assets'],2)
cleandata['Reserves.and.funds.Ratio'] <- round(cleandata['Reserves.and.funds']/cleandata['Total.assets'],2)
cleandata['Borrowings.Ratio'] <- round(cleandata['Borrowings']/cleandata['Total.assets'],2)
cleandata['Current.liabilities.&.provisions.Ratio'] <- round(cleandata['Current.liabilities.&.provisions'] /cleandata['Total.assets'],2)
cleandata['Deferred.tax.liability.Ratio'] <- round(cleandata['Deferred.tax.liability'] /cleandata['Total.assets'],2)
cleandata['Shareholders.funds.Ratio'] <- round(cleandata['Shareholders.funds'] /cleandata['Total.assets'],2)
cleandata['Cumulative.retained.profits.Ratio'] <- round(cleandata['Cumulative.retained.profits'] /cleandata['Total.income'],2)
cleandata['Capital.employed.Ratio'] <- round(cleandata['Capital.employed'] /cleandata['Total.assets'],2)
cleandata['Contingent.liabilities'] <- round(cleandata['Contingent.liabilities'] /cleandata['Total.assets'],2)
cleandata['Net.fixed.assets.Ratio'] <- round(cleandata['Net.fixed.assets'] /cleandata['Total.assets'],2)
cleandata['Current.assets.Ratio'] <- round(cleandata['Current.assets'] /cleandata['Total.assets'],2)
cleandata['Net.working.capital.Ratio'] <- round(cleandata['Net.working.capital'] /cleandata['Total.assets'],2)


head(cleandata)

data_new <- subset(cleandata, select = -c(1, 4, 5, 6, 7, 8, 9, 10, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 30, 31, 32,33))
dim(data_new)

##Outlier Treatment

newdata<- percentcap(data_new)
summary(newdata)

#For Testing Data
cleantestdata['Total.Income.Ratio'] <- round(cleantestdata['Total.income'] / cleantestdata['Total.assets'],2)
cleantestdata['Change.in.stock.Ratio'] <- round(cleantestdata['Change.in.stock'] / cleantestdata['Total.income'],2)
cleantestdata['Total.Expenses.Ratio'] <- round(cleantestdata['Total.expenses'] / cleantestdata['Total.income'],2)
cleantestdata['Profit.after.tax.Ratio'] <- round(cleantestdata['Profit.after.tax'] / cleantestdata['Total.assets'],2)
cleantestdata['PBDITA.Ratio'] <- round(cleantestdata['PBDITA'] / cleantestdata['Total.assets'],2)
cleantestdata['PBT.Ratio'] <- round(cleantestdata['PBT'] / cleantestdata['Total.assets'],2)
cleantestdata['Cash.profit./Ratio'] <- round(cleantestdata['Cash.profit'] / cleantestdata['Total.assets'],2)
cleantestdata['Sales.Ratio'] <- round(cleantestdata['Sales'] / cleantestdata['Total.assets'],2)
cleantestdata['Income.from.financial.services.ratio'] <- round(cleantestdata['Income.from.financial.services'] /cleantestdata['Total.assets'],2)
cleantestdata['Other.income.Ratio'] <- round(cleantestdata['Other.income'] /cleantestdata['Total.assets'],2)
cleantestdata['Total.capital.Ratio'] <- round(cleantestdata['Total.capital'] /cleantestdata['Total.assets'],2)
cleantestdata['Reserves.and.funds.Ratio'] <- round(cleantestdata['Reserves.and.funds'] /cleantestdata['Total.assets'],2)
cleantestdata['Borrowings.Ratio'] <- round(cleantestdata['Borrowings'] /cleantestdata['Total.assets'],2)
cleantestdata['Current.liabilities.&.provisions.Ratio'] <- round(cleantestdata['Current.liabilities.&.provisions'] /cleantestdata['Total.assets'],2)
cleantestdata['Deferred.tax.liability.Ratio'] <- round(cleantestdata['Deferred.tax.liability'] /cleantestdata['Total.assets'],2)
cleantestdata['Shareholders.funds.Ratio'] <- round(cleantestdata['Shareholders.funds'] /cleantestdata['Total.assets'],2)
cleantestdata['Cumulative.retained.profits.Ratio'] <- round(cleantestdata['Cumulative.retained.profits'] /cleantestdata['Total.income'],2)
cleantestdata['Capital.employed.Ratio'] <- round(cleantestdata['Capital.employed'] /cleantestdata['Total.assets'],2)
cleantestdata['Contingent.liabilities'] <- round(cleantestdata['Contingent.liabilities'] /cleantestdata['Total.assets'],2)
cleantestdata['Net.fixed.assets.Ratio'] <- round(cleantestdata['Net.fixed.assets'] /cleantestdata['Total.assets'],2)
cleantestdata['Current.assets.Ratio'] <- round(cleantestdata['Current.assets'] /cleantestdata['Total.assets'],2)
cleantestdata['Net.working.capital.Ratio'] <- round(cleantestdata['Net.working.capital'] /cleantestdata['Total.assets'],2)

testdatanew <- subset(cleantestdata,select = -c(4,5,6,7,8,9,10,16,17,18,19,20,21,22,23,24,25,26,30,31,33,34))

dim(testdatanew)


##EXPLORATORY Data Analysis
#Statistical Analysis of numerical Variables

traindata <- subset(data_new, select = -default)

names_traindata <- colnames(traindata)

lapply(traindata[,names_traindata], summary)

##Histograms
##First 15 histograms
a<- newdata[,1:15]
a %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value))+facet_wrap(~key, scales = "free", ncol = 5)+geom_histogram(fill = "blue")


##Next 15 Histograms

a<- newdata[,16:30]
a %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value))+facet_wrap(~key, scales = "free", ncol = 5)+geom_histogram(fill = "red")


a<- newdata[,31:45]
a %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value))+facet_wrap(~key, scales = "free", ncol = 5)+geom_histogram(fill = "gold")

a<- newdata[,46:47]
a %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value))+facet_wrap(~key, scales = "free", ncol = 5)+geom_histogram(fill = "green")



##Boxplots
d<-newdata[,c(1:10,26)]
apply(d[,-11], 2, function(x) tapply(x, newdata$default,summary))

d %>%
  gather(-default, key = "var", value = "value") %>%
  ggplot(aes(x = default, y = value))+ geom_boxplot()+ facet_wrap(~var,scales = "free", nrow = 5)+theme_bw()



d<-newdata[,c(11:20,26)]
apply(d[,-11], 2, function(x) tapply(x, newdata$default,summary))

d %>%
  gather(-default, key = "var", value = "value") %>%
  ggplot(aes(x = default, y = value))+ geom_boxplot()+ facet_wrap(~var,scales = "free", nrow = 5)+theme_bw()


d<-newdata[,c(21:30)]
apply(d[,-7], 2, function(x) tapply(x, newdata$default,summary))

d %>%
  gather(-default, key = "var", value = "value") %>%
  ggplot(aes(x = default, y = value))+ geom_boxplot()+ facet_wrap(~var,scales = "free", nrow = 5)+theme_bw()


d<-newdata[,c(31:40,26)]
apply(d[,-11], 2, function(x) tapply(x, newdata$default,summary))

d %>%
  gather(-default, key = "var", value = "value") %>%
  ggplot(aes(x = default, y = value))+ geom_boxplot()+ facet_wrap(~var,scales = "free", nrow = 5)+theme_bw()

d<-newdata[,c(41:47,26)]
apply(d[,-10], 2, function(x) tapply(x, newdata$default,summary))

d %>%
  gather(-default, key = "var", value = "value") %>%
  ggplot(aes(x = default, y = value))+ geom_boxplot()+ facet_wrap(~var,scales = "free", nrow = 5)+theme_bw()



#Multicollinearity Check
plot_correlation(data_new)

#Treating Multicollinearity
nndata <- colnames(data_new)
nndata <- nndata[-26]
nndata_df <- subset(data_new, select = nndata)
highcorr <- findCorrelation(cor(nndata_df), cutoff = 0.6, verbose = TRUE, names = TRUE)

print(highcorr)

dim(data_new)

cleantraindata<- data_new[,!(names(data_new) %in% highcorr)]
cleantraindata <-as.data.frame(cleantraindata)
dim(cleantraindata)

cleantraindata$default <- as.numeric(ifelse(cleantraindata$default == "1",1,0))

#ModelBuilding

model1 <- glm(default~., data = cleantraindata, family = binomial)

# Predict the probability (p) of default positivity
probs<- predict(model1,type = "response")

ourdata <- cleantraindata

# Bind the logit and tidying the data for the plot
ourdata$default <- as.factor(ourdata$default)
 ourdata <- ourdata %>% 
   dplyr::select_if(is.numeric)
 predictors <- colnames(ourdata)


 
ourdata <- ourdata %>% mutate(logit = log(probs / (1 - probs))) %>% gather(key = "predictors",
                                                                           value = "predictor.value", -logit)

ggplot(ourdata, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~ predictors, scales = "free_y")

summary(model1)

table(cleantraindata$default)

prop.table(table(cleantraindata$default))

#As we can see the data after cleaning i.e. Treating outliers and Missing Values the remaining data is imbalanced
#hence we can use SMOTE analysis to balance the data set.

smotedata <- cleantraindata
smotedata$default<- as.factor(smotedata$default)
smotedata <- SMOTE(default~., smotedata, perc.over = 2000, pperc.under = 200)

table(smotedata$default)
prop.table(table(smotedata$default))

smotedata$default <- as.numeric(ifelse(smotedata$default == "1",1,0))


#model2 with balanced Dataset

logmodel <- glm(default~., data = smotedata, family = binomial(link = "logit"))

logmodelfit<- pR2(logmodel)["McFadden"]
print(logmodelfit)

print(summary(logmodel))

print(exp(logmodel$coefficients))


#Evaluating model Performance

logmodel_predict <- predict(logmodel,newdata = smotedata,type = "response")
logmodel_predict <- ifelse(logmodel_predict>0.5,1,0)

perf1 <- table(as.factor(smotedata$default), logmodel_predict)
perf1

n <-sum(perf1)
nc <- nrow(perf1)
diag <- diag(perf1)
rowsums <- apply(perf1, 1, sum)
colsums <- apply(perf1, 2, sum)

p <- rowsums/n
q <- colsums/n


#Accuracy

accuracy = sum(diag)/n

precision = diag/colsums 
recall = diag/rowsums   #Sensitivity
f1 = 2 * precision*recall/(precision + recall)

type <- 'Training'

p <- predict(logmodel,newdata = smotedata, type = 'response')
pr <- prediction(p, smotedata$default)
prf<- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

observation = data.frame(type,accuracy,auc,f1)

observation[2,]



#Model Performance on Test Data
predict_test <- predict(logmodel, newdata = testdatanew, type = "response")

test_log_predicted <- ifelse(predict_test>0.5,1,0)
pref2 <- table(as.factor(testdatanew$default),test_log_predicted)
pref2

a <- sum(pref2)
c <- nrow(pref2)
diags <- diag(pref2)
rowsums_new <- apply(pref2, 1, sum)
colsums_new <- apply(pref2, 2, sum)

w <- rowsums_new/a
e <- colsums_new/a



#Accuracy

accuracy_test = sum(diags)/a

precision_test = diags/colsums_new

recall_test = diags/rowsums_new

f2 = 2*precision_test*recall_test/(precision_test+recall_test)

type_test <- "Testing"
z <- predict(logmodel, newdata = testdatanew, type = "response")
zr <- prediction(z, testdatanew$default)
zrf <- performance(zr, measure = "tpr", x.measure = "fpr")
plot(zrf)

auc_test <- performance(zr, measure = "auc")
auc_test <- auc_test@y.values[[1]]

auc_test

observation_test <- data.frame(type_test, accuracy_test, auc_test, f2)
observation_test[2,]
