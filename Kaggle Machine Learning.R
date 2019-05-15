######################################################################
######################################################################
######################################################################
####################  KAGGLE MACHINE LEARNING  #######################
######################################################################
######################################################################
######################################################################

# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)

# Print train and test to the console
str(train)
str(test)

# 0->Died
# Survival rates in absolute numbers
table(train$Survived)

# Survival rates in proportions
prop.table(table(train$Survived))

# Two-way comparison: Sex and Survived
table(train$Sex,train$Survived)

# Two-way comparison: row-wise proportions
prop.table(table(train$Sex,train$Survived),1)


# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- "child"
train$Child[train$Age>=18] <- "Ad"

# Two-way comparison
prop.table(table(train$Child,train$Survived),1)

#############################################################################
## First test, we asume all men die, all women live
# Copy of test
test_one <- test

# Initialize a Survived column to 0
test_one$Survived<-0

# Set Survived to 1 if Sex equals "female"
test_one$Survived[test_one$Sex=="female"]<-1

####################
library(rpart) #  To build the tree
# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)


# Make predictions on the test set
my_prediction <- predict(my_tree_two, newdata = test, type = "class")
# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# Now we use minsplit and cp, minsplit determines the minimum amount of observations on a leaf,
# cp determines when the splitting of the decision tree stops, 0 indicates no stopping
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                       data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))

# Visualize my_tree_three
fancyRpartPlot(my_tree_three)

# Create train_two
train_two <- train
# Add a new variable, family_size using SibSp(siblins or spoues) and Parch(parents or children)
# maybe big families needed more time to get together and so the died
train_two$family_size <- train_two$SibSp + train_two$Parch + 1

my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size,
                      data = train_two, method = "class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)

## Family size is not included in this decision tree, it seems it isn't relevant
###############################################################################################
###############################################################################################
###############################################################################################
#check where is the following data 
# train_new and test_new are available in the workspace

# Finish the command
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
                      data = train_new, method = "class")

# Visualize my_tree_five
fancyRpartPlot(my_tree_five)

# Make prediction
my_prediction <- predict(my_tree_five, newdata=test_new, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)