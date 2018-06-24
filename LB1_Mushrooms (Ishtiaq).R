rm(list=ls()) #clear workspace

#Importing Data from url... var_01


setwd('C:/Users/Ishtiaq/Desktop/CDA/CSDA 1010/Lab1')
mushrooms <- read.table("Mushrooms.data", header = TRUE, sep = ",")

#check # of rows/cols
ncol(mushrooms);
nrow(mushrooms);

str(mushrooms)
summary(mushrooms)
head(mushrooms)


#column names are added
fields <- c("class",
            "cap_shape",
            "cap_surface",
            "cap_color",
            "bruises",
            "odor",
            "gill_attachment",
            "gill_spacing",
            "gill_size",
            "gill_color",
            "stalk_shape",
            "stalk_root",
            "stalk_surface_above_ring",
            "stalk_surface_below_ring",
            "stalk_color_above_ring",
            "stalk_color_below_ring",
            "veil_type",
            "veil_color",
            "ring_number",
            "ring_type",
            "spore_print_color",
            "population",
            "habitat")
colnames(mushrooms) <- fields
head(mushrooms)

#Save new df in csv...
write.csv(mushrooms, 'C:/Users/Ishtiaq/Desktop/CDA/CSDA 1010/Lab1/Mushrooms.data', row.names=FALSE)


# Remove incomplete data or without info
summary(mushrooms$veil_type)
head(mushrooms$veil_type)
mushrooms$veiltype = NULL;

str(mushrooms$class)
mushrooms$class <- as.character(mushrooms$class)
mushrooms$class[mushrooms$class == "e"] = "s";
mushrooms$class <- as.factor(mushrooms$class)

str(mushrooms$class)



# Load libraries.
library(ggplot2);
library(rpart)
library(caret);
library(gridExtra);
library(randomForest);

###Explore the data

qp1 = qplot(class, odor, color = class, data = mushrooms, geom = "jitter", main = "Mushrooms - Odor");
qp2 = qplot(odor, spore_print_color, color = class, data = mushrooms, geom = "jitter", main = "Mushrooms - Odor vs Spore Print Color");
qp3 = qplot(cap_shape, cap_surface, color = class, data = mushrooms, geom = "jitter", main = "Mushrooms - Cap Shape vs Cap Surface");
qp4 = qplot(stalk_color_above_ring, stalk_color_below_ring, color = class, data = mushrooms, geom = "jitter", main = "Mushrooms by Stalk Color - Abrove ring vs Below ring");

grid.arrange(qp1, qp2, qp3, qp4, ncol = 2, nrow = 2)

grid.arrange(qp1, ncol = 1, nrow = 1)
grid.arrange(qp2, ncol = 1, nrow = 1)
grid.arrange(qp3, ncol = 1, nrow = 1)
grid.arrange(qp4, ncol = 1, nrow = 1)

## Creating Training and Test Sample sets

set.seed=2
id <- sample(2,nrow(mushrooms), prob=c(0.66,0.33), replace = TRUE)
mushrooms_train <-mushrooms[id==1,]
mushrooms_test <-mushrooms[id==2,]



## Selecting most relevant attributes after feature engineering and APPLY RANDOM FORST#####
classifier = randomForest(class ~ cap_shape + cap_surface + odor + cap_color, data = mushrooms_train, ntree=100)

## Training Data
training_predict = predict(classifier, newdata=mushrooms_train)
training_confusion = table(training_predict, mushrooms_train$class)
training_confusion # Run Confusion Matrix

sum(diag(training_confusion))/sum(training_confusion) # Calculating Accuracy of Confusion Matrix Training Data


## Test Data
testing_predict = predict(classifier, newdata=mushrooms_test)
testing_confusion = table(testing_predict, mushrooms_test$class)
testing_confusion
sum(diag(testing_confusion))/sum(testing_confusion) # Calculating Accuracy of Confusion Matrix Test Data

plot(testing_confusion)



#### DECISION TREE

DT <- rpart(class ~ odor +stalk_color_above_ring + stalk_color_below_ring + spore_print_color, mushrooms_train, method = "class", cp=0)
summary(DT)
printcp(DT)
rpart.plot(DT, type=1, extra = 102)




t_pred <- predict(DT, mushrooms_train, type = "class")
confMat <- table(mushrooms_train$class,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy

predict_dt <- predict(DT, mushrooms_test, type = "class")


