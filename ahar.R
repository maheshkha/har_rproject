library(knitr)
opts_chunk$set(cache=TRUE,echo=TRUE)
options(width=120)
library(caret)
library(randomForest)
library(pander)

setwd("C:/Users/pragati/Documents/GitHub/har_rproject")

training <- read.csv("pml-training.csv",na.strings=c("NA",""))
testing <-read.csv("pml-testing.csv",na.strings=c("NA",""))
dim(training)
dim(testing)
training[1:5,c('user_name','classe','num_window','roll_belt','pitch_belt')]
sum(is.na(training))  
t1 <- table(colSums(is.na(training)))
t2 <- table(colSums(is.na(testing)))
pandoc.table(t1, style = "grid", justify = 'left', caption = 'Training data column NA frequencies')
pandoc.table(t2, style = "grid", justify = 'left', caption = 'Testing data column NA frequencies')
# for training dataset
columnNACounts <- colSums(is.na(training))        
badColumns <- columnNACounts >= 19000             
cleanTrainingdata <- training[!badColumns]        
sum(is.na(cleanTrainingdata))                     
cleanTrainingdata <- cleanTrainingdata[, c(7:60)] 

# for testing dataset
columnNACounts <- colSums(is.na(testing))         
badColumns <- columnNACounts >= 20                
cleanTestingdata <- testing[!badColumns]        
sum(is.na(cleanTestingdata))                     
cleanTestingdata <- cleanTestingdata[, c(7:60)] 
s <- summary(cleanTrainingdata$classe)
pandoc.table(s, style = "grid", justify = 'left', caption = '`classe` frequencies')
plot(cleanTrainingdata$classe,col=rainbow(5),main = "`classe` frequency plot")
partition <- createDataPartition(y = cleanTrainingdata$classe, p = 0.6, list = FALSE)
trainingdata <- cleanTrainingdata[partition, ]
testdata <- cleanTrainingdata[-partition, ]
model <- train(classe ~ ., data = trainingdata, method = "rf", prox = TRUE, trControl = trainControl(method = "cv", number = 4, allowParallel = TRUE))
model
training_pred <- predict(model, trainingdata)
confusionMatrix(training_pred, trainingdata$classe)
testing_pred <- predict(model, testdata)
confusionMatrix(testing_pred, testdata$classe)
answers <- predict(model, cleanTestingdata)
answers <- as.character(answers)
answers
pml_write_files = function(x) {
    n = length(x)
    for (i in 1:n) {
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
            col.names = FALSE)
    }
}

pml_write_files(answers)


