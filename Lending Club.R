source("http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R")
require(dplyr)
require(ggplot2)
require(corrplot)
require(pROC)
require(caret)
require(doParallel)
registerDoParallel(cores = 2)

#data import ####
files = dir()[grep("csv",dir())]
files = paste0("./",files)


importData <- function(file){
        
        read.csv(file, skip = 1)
        
}

data <- do.call(rbind,mclapply(files,importData))


#data clean ####

#replaced underscore with with no space
names(data) <- gsub("_","", names(data))

#columns to remove

colRemove <- c("zipcode", "id", "memberid", "nextpymentd", "totalpymntinv", "url", 
               "lastpymntd", "nextpymntd", "policycode", "pymntplan","totalpymnt",
               "totalrecprncp", "totalrecint", "totalreclatefee", "recoveries",
               "collectionrecoveryfee", "lastpymntamnt","lastficorangehigh",
               "lastficorangelow","grade","subgrade","fundedamnt","fundedamntinv",
               "issued","lastcreditpulld"
               )

data <- data[,!names(data) %in% colRemove]

#include loans in 2007-2011 that include "Does not meet the credit policy"
data$loanstatus <- gsub("Does not meet the credit policy.  Status:","",data$loanstatus)

#changed classes for variables
colChar <- c("desc","emptitle","title")

data[colChar] <- sapply(data[colChar],as.character)

#removed months from term, percentages to decimal
data <-data %>%
        mutate(term = as.numeric(gsub("\\D","",term))) %>%
        rename(termmonths = term) %>%
        mutate(intrate = as.numeric(gsub("%","",intrate))/100) %>%
        mutate(revolutil = as.numeric(gsub("%","",revolutil))/100)

#removed "Borrower added on..." string from desc
data$desc <- with(data,gsub("Borrower added on \\d+/\\d+/\\d+ >" ,"",desc))
data$desc <- with(data,gsub("<br>","",desc))
data$desc <- with(data,gsub("^\\s+ ","",desc))

#created variables for character counts, and binned variables
data<-data %>%
        mutate(desclength = nchar(desc),
               emptitlelength = nchar(emptitle),
               titlelength = nchar(title),
               earliestcrline =2015 - as.numeric(gsub("\\w+-","",earliestcrline)),
               numofdelinq = ifelse(is.na(mthssincelastdelinq), 0, 
                                    ifelse(mthssincelastdelinq > 24 & delinq2yrs == 0,1,delinq2yrs)),
               mthssincelastrecord = ifelse(mthssincelastrecord>=0 & mthssincelastrecord <=77, "0-77","78-129"),
               mthssincelastrecord = ifelse(is.na(mthssincelastrecord),"Never",mthssincelastrecord),
               mthssincelastrecord = as.factor(mthssincelastrecord),
               mthssincelastmajorderog = ifelse(mthssincelastmajorderog>=0 & mthssincelastmajorderog <=45,"0-45",
                                                ifelse(mthssincelastmajorderog >= 46 & mthssincelastmajorderog <=75, "46-75",">75")),
               mthssincelastmajorderog = ifelse(is.na(mthssincelastmajorderog),"Never",mthssincelastmajorderog),
               mthssincelastmajorderog = as.factor(mthssincelastmajorderog)) %>%
               filter(loanstatus != "Current", loanstatus != "")

#create classification variable "Default" or "No.Default" based on Lending Club's probabilities
statusClass <- as.factor(sapply(data$loanstatus, function(x){
                ret = 0
                if (x == "Fully Paid") ret = "No.Default" 
                else if (x == "Charged Off") ret = "Default" 
                else if(x == "Default") ret = if (rbinom(1,1,.92)==1) "Default" else "No.Default"
                else if(x == "In Grace Period") ret = if (rbinom(1,1,.24)==1) "Default" else "No.Default"
                else if(x == "Late (16-30 days)") ret = if (rbinom(1,1,.51)==1) "Default" else "No.Default"
                else if(x == "Late (31-120 days)") ret = if (rbinom(1,1,.72)==1) "Default" else "No.Default"
                return(ret)
}))


data <- cbind(data,statusClass)

#removed columns after I used them to create new variables
data <- data %>%
         select(-emptitle,-loanstatus,-desc,-title,-delinq2yrs, -mthssincelastdelinq)

data <- na.omit(data)

# save(data, file = "./data.RDS")

#Exploratory Analysis ####

#fully paid & defaults by year ####
a <- data %>%
        group_by(statusClass) %>%
        summarize(freq = n()) %>%
        mutate(pct = paste0(round(freq/sum(freq)*100,0),"%"))

ggplot(a,aes(statusClass,freq/1000,fill = statusClass)) + 
        geom_bar(stat = 'identity',position = 'dodge',width = 0.5,color = 'black') +  
        coord_cartesian(ylim=c(0,200)) +
        stat_identity(aes(x=statusClass, label=pct), geom="text", position = position_dodge(width=.95), vjust=-1) +
        scale_fill_manual(values=c("#CC6666", "#66CC99")) +
        labs(title = "Loans of 2007-2014", y = "Frequency(10^3)",x = "Loan Status") +
        theme(axis.title.x = element_text(size = 15, vjust = -.35),
              axis.title.y = element_text(size = 15, vjust = .50),
              plot.title = element_text(size = 20, face = "bold", vjust = 2),
              legend.title=element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"))




#RF variable importance ####
set.seed(1337)

trainImpIndx <- createDataPartition(data$statusClass,
                                  p = .2,
                                  list = F,
                                  times = 1)

trainDataImp <- data[trainImpIndx,]


#rf for Important Variables

impRf<- train(statusClass ~ .,
                data = trainDataImp,
                method = "rf",
                trControl = trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 2,
                        classProbs = T,
                        summaryFunction = twoClassSummary
                        ),
                metric = "ROC"
                 )

# save(impRf, file = "./impRf.RDS")

varImpObj <- varImp(impRf)
plot(varImpObj, main = "Top 50 Important Variables", top = 50)
impThresh <- (varImpObj$importance[, 1]>20)

#important coefficients
impCoefs <- impRf$coefnames[impThresh]
impCoefs <- impCoefs[impCoefs != "desclength"]

#rf with important variables
impCoefsData <- data[,impCoefs]
impCoefsData <- cbind(impCoefsData,statusClass = data[,ncol(data)])

# save(impCoefsData,file= "./impCoefsData.RDS")


#Machine Learning train data, test data, and train control ####
set.seed(1337)
trainIdx <- createDataPartition(impCoefsData$statusClass,
                                  p = .8,
                                  list = F,
                                  times = 1)

trainData <- impCoefsData[trainIdx,]
testData <- impCoefsData[-trainIdx,]

otherControl <- trainControl(method = "repeatedcv",
                        repeats = 3,
                        classProbs = T,
                        summaryFunction = twoClassSummary
                        )


#c5.0 ####

c5.0 <- train(statusClass ~ .,
                data = trainData,
                method = "C5.0",
                trControl = otherControl,
                preProcess= c("center","scale"),
                tuneGrid = expand.grid(model = "tree", winnow = c(TRUE,FALSE),
                                      trials = 1:5),
                metric = "ROC"
             )

# save(c5.0, file ="./c50.RDS")

c5.0pre <- predict(c5.0, testData[,-ncol(testData)])
confusionMatrix(c5.0pre, reference = testData[,ncol(testData)], positive = "Default")

#c5.0 cost ####

#function for setting my own metric
stats <- function (data, lev = NULL, model = NULL)  {
  c(postResample(data[, "pred"], data[, "obs"]),
    Sens = sensitivity(data[, "pred"], data[, "obs"]),
    Spec = specificity(data[, "pred"], data[, "obs"]),
    negpre = negPredValue(data[,"pred"],data[,"obs"]))
}                      

c5.0cost <- train(statusClass ~ .,
               data = trainData,
               method = "C5.0Cost",
               trControl = trainControl(method = "repeatedcv", repeats = 3,
                     summaryFunction = stats, classProbs = T),
               preProcess= c("center","scale"),
               tuneGrid = expand.grid(model = "tree", winnow = c(TRUE, FALSE),
                                      trials = 1:5,
                                      cost = 1:5),
                metric = "negpre"
             )

#save(c5.0cost, file ="./c50Cost5Trial1.RDS" )

c5.0costpre <- predict(c5.0cost, testData[,-ncol(testData)])
confusionMatrix(c5.0costpre, reference = testData[,ncol(testData)], positive = "Default")


#GLM with impCoefsData ####

impCoefglm <- train(statusClass ~.,
                        data = trainData,
                        method = "glm",
                        trControl = otherControl,
                        metric = "ROC",
                        preProcess= c("center","scale")
                    
                 ) 

#save(impCoefglm, file = "./impCoefglm.RDS")

impCoefglmpre <- predict(impCoefglm, testData[,-ncol(testData)])

confusionMatrix(impCoefglmpre, reference = testData[,ncol(testData)], positive = "Default")


#GBM with impCoefsData ####

impCoefgbm <- train(statusClass ~.,
                        data = trainData,
                        method = "gbm",
                        trControl = otherControl,
                        metric = "ROC",
                        preProcess= c("center","scale"),
                        verbose = TRUE,
                        tuneGrid = expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1)
                ) 

#save(impCoefgbm, file = "./impCoefgbm.RDS")

impCoefgbmpre <- predict(impCoefgbm, testData[,-ncol(testData)])
confusionMatrix(impCoefgbmpre, reference = testData[,ncol(testData)], positive = "Default")

#Plot ROC ####

load(file = "./impCoefsData.RDS")
load(file ="./c50.RDS")
load(file = "./impCoefglm.RDS")
load(file = "./impCoefgbm.RDS")

set.seed(1337)
trainIndex <- createDataPartition(impCoefsData$statusClass,
                                  p = .8,
                                  list = F,
                                  times = 1)

trainData <- impCoefsData[trainIndex,]
testData <- impCoefsData[-trainIndex,]


c5.0pre <- predict(c5.0, testData[,-ncol(testData)], type = "prob")
impCoefglmpre <- predict(impCoefglm, testData[,-ncol(testData)], type = "prob")
impCoefgbmpre <- predict(impCoefgbm, testData[,-ncol(testData)],type = "prob")


#c5.0 Trees
plot.roc(testData[,ncol(testData)],c5.0pre[,"Default"], main="ROC Curve Comparison", xlim(1,0), ylim(0,1), col="red")

#GLM
lines.roc(testData[,ncol(testData)],impCoefglmpre[,"Default"], col="black")

#GBM
lines.roc(testData[,ncol(testData)],impCoefgbmpre[,"Default"], col="blue")

legend("bottomright", legend=c("C5.0", "GLM", "GBM"), col=c("red", "black", "blue"), lwd=2)
