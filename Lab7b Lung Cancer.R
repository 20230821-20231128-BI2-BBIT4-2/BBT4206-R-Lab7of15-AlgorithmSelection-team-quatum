if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# Introduction ----
# There are hundreds of algorithms to choose from.
# A list of the classification and regression algorithms offered by
# the caret package can be found here:
# http://topepo.github.io/caret/available-models.html

# The goal of predictive modelling is to use the most appropriate algorithm to
# design an accurate model that represents the dataset. Selecting the most
# appropriate algorithm is a process that involves trial-and-error.

# If the most appropriate algorithm was known beforehand, then it would not be
# necessary to use Machine Learning. The trial-and-error approach to selecting
# the most appropriate algorithm involves evaluating a diverse set of
# algorithms on the dataset, and identifying the algorithms that create
# accurate models and the ones that do not.

# Once you have a shortlist of the top algorithms, you can then improve their
# results further by either tuning the algorithm parameters or by combining the
# predictions of multiple models using ensemble methods.

# STEP 1. Install and Load the Required Packages ----
library(readr)
## stats ----
if (require("stats")) {
  require("stats")
} else {
  install.packages("stats", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## MASS ----
if (require("MASS")) {
  require("MASS")
} else {
  install.packages("MASS", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## glmnet ----
if (require("glmnet")) {
  require("glmnet")
} else {
  install.packages("glmnet", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## kernlab ----
if (require("kernlab")) {
  require("kernlab")
} else {
  install.packages("kernlab", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## rpart ----
if (require("rpart")) {
  require("rpart")
} else {
  install.packages("rpart", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}







lung_cancer <- read_csv("data/survey lung cancer.csv",
           col_types =
             cols(
               GENDER = col_character(),
               AGE = col_integer(),
               SMOKING = col_integer(),
               LUNG_CANCER =
                 col_factor(levels =
                              c("YES",
                                "NO")),
               
               
               # license = col_skip()
             ))
### 3.d. kNN for a regression problem with CARET's train function ----
#### Load and split the dataset ----
data(lung_cancer)

# Define an 80:20 train:test data split of the dataset.
train_index <- createDataPartition(lung_cancer$LUNG_CANCER,
                                   p = 0.8,
                                   list = FALSE)
lung_cancer_train <- lung_cancer[train_index, ]
lung_cancer_test <- lung_cancer[-train_index, ]

#### Train the model ----
# We apply the 5-fold cross validation resampling method
# We also apply the standardize data transform
set.seed(7)
train_control <- trainControl(method = "cv", number = 5)
housing_caret_model_knn <- train(LUNG_CANCER ~ ., data = lung_cancer,
                                 method = "knn", metric = "Accuracy",
                                 preProcess = c("center", "scale"),
                                 trControl = train_control)

#### Display the model's details ----
print(housing_caret_model_knn)

#### Make predictions ----
predictions <- predict(housing_caret_model_knn,
                       lung_cancer_test[, 1:16])


#### Display the model's evaluation metrics ----

confusion_matrix <-
  caret::confusionMatrix(predictions,
                         lung_cancer_test[, 1:16]$LUNG_CANCER)
print(confusion_matrix)

fourfoldplot(as.table(confusion_matrix), color = c("grey", "lightblue"),
             main = "Confusion Matrix")

