dataset= read.csv("dataset.csv")

library(caret)
library(caTools)
newdataset = dataset


newdataset[nearZeroVar(newdataset)]=NULL # removing nearly zero variance column
colnames(newdataset)[colSums(is.na(newdataset)) > 0] # checking for NULL value column
cok=apply(newdataset,2,function(x)any(is.na(x))) # removing null value column
newdataset[,cok]= NULL 
predata <- preProcess(newdataset, 
                     method = c("center", "scale", "YeoJohnson"))
transformed = predict(predata, newdata = newdataset)
transformed$Block.ID = NULL
transformed$Local.Block.name = NULL


result = data.frame(matrix(nrow = 1718, ncol= 3))
colnames(result)= c("First Need", "Second Need", "Third Need")
result$`First Need`= newdataset$o2_1_of_the_needs_with_the_highest_severity_rating_what_is_the_first_most_important
result$`Second Need`= newdataset$o2_2_of_the_needs_with_the_highest_severity_rating_what_is_the_second_most_important
result$`Third Need` = newdataset$o2_3_of_the_needs_with_the_highest_severity_rating_what_is_the_third_most_important

transformed$o2_1_of_the_needs_with_the_highest_severity_rating_what_is_the_first_most_important= NULL
transformed$o2_2_of_the_needs_with_the_highest_severity_rating_what_is_the_second_most_important= NULL
transformed$o2_3_of_the_needs_with_the_highest_severity_rating_what_is_the_third_most_important= NULL



#result = dataset[228:245]
#result = dataset[150:152]


transformed$output = result$`Third Need`
split= sample.split(transformed$output, SplitRatio = .80)
training = subset(transformed, split==TRUE)
test = subset(transformed, split== FALSE)


#install.packages("caret")
set.seed(10)
train.control = trainControl(method = "cv", number = 10)
#model <- train(shelter ~., data = training, method = "ranger",
#              tuneLength = 20,trControl = train.control)

rfmodel <- train(output ~., data = training, method = "ranger",
                  tuneLength = 20,trControl = train.control)
prediction = predict(rfmodel, test)
confusionMatrix(prediction, test$output)
print(rfmodel)


svmmodel <- train(output ~., data = training, method = "svmRadial",
                  tuneLength = 5,trControl = train.control)

prediction = predict(svmmodel, test)
confusionMatrix(prediction, test$output)
print(svmmodel)


knnmodel <- train(output ~., data = training, method = "knn",
              tuneLength = 5,trControl = train.control)




prediction = predict(knnmodel, test)
confusionMatrix(prediction, test$output)
print(knnmodel)

