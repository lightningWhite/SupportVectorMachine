
# Include the LIBSVM package
library(e1071)

# Load the dataset from the .csv file
setwd(dir = "C:/Users/Daniel/Documents/College/2018 Fall Semester/CS450 - Machine Learning and Data Mining/Week09")
data <- read.csv(file="vowel.csv", header=TRUE, sep=",")
# data <- read.csv(file="letters.csv", header=TRUE, sep=",")

# Place all of the rows into a variable so we can split it into a test/train set
allRows <- 1:nrow(data)

# Obtain random indexes for 30% of the rows for testing use
testRows <- sample(allRows, trunc(length(allRows) * 0.3))

# The test set contains all the test rows
dataTest <- data[testRows,]

# The training set contains all the other rows
dataTrain <- data[-testRows,]

# Train an SVM model
# Tell it the attribute to predict vs the attributes to use in the prediction,
#  the training data to use, and the kernal to use, along with its hyperparameters.
#  Please note that "Species~." contains a tilde character, rather than a minus
#  Note: Class~ says that "Class depends on" and the "." says "everything but Class"

bestCost = 0
bestGamma = 0
bestAccuracy = c(0,0)

count = 0

# Apply a grid-search to find the best cost and gamma values for the best accuracy
for(Cost in c(2**-3, 2**-1, 2**1, 2**3, 2**5, 2**7, 2**9, 2**11, 2**13, 2**15)) {
  for (Gamma in c(2**-15, 2**-13, 2**-11, 2**-9, 2**-7, 2**-5, 2**-3, 2**-1, 2**1, 2**3)){
    
    model <- svm(Class~., data = dataTrain, kernel = "radial", gamma = Gamma, cost = Cost) # Vowel data
    # model <- svm(letter~., data = dataTrain, kernel = "radial", gamma = 2e-1, cost = 2e3)  # Letter data
    
    # Use the model to make a prediction on the test set
    # Notice, we are not including the last column here (our target)
    prediction <- predict(model, dataTest[,-13]) # This line is for the vowel data
    # prediction <- predict(model, dataTest[,-1]) # This line is for the letter data 
    
    # Produce a confusion matrix
    confusionMatrix <- table(pred = prediction, true = dataTest$Class)   # Vowel data
    # confusionMatrix <- table(pred = prediction, true = dataTest$letter)# Letter data
    
    # Calculate the accuracy, by checking the cases that the targets agreed
    agreement <- prediction == dataTest$Class # Vowel data
    # agreement <- prediction == dataTest$letter  # Letter data
    
    print(table(agreement))
    accuracy <- prop.table(table(agreement))
    
    # Print our results to the screen
    print(confusionMatrix)
    print(accuracy)

    # If a better accuracy was obtained, update the best cost and gamma values
    if (accuracy[2] > bestAccuracy[2]) {
      print("Better")
      bestCost = Cost
      bestGamma = Gamma
      bestAccuracy = accuracy
    }
    
    count = count + 1
    print("Count: ")
    print(count)
  }
}

# Display the best results obtained and which gamma and cost was used
print("BestCost")
print(bestCost)

print("BestGamma")
print(bestGamma)

print("BestAccuracy")
print(bestAccuracy)
  