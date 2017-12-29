#========================================================================
# +++ QUESTION +++

# Classer le type d'activité (erreur ou OK) en fonction des capteurs installés

#========================================================================
# INITIALISATIONS

rm(list = ls())
setwd('./Data_Science/Coursera/_08_Practical_machine_learning/W4/Project')
library(dplyr)
library(caret)
library(ggplot2)

#========================================================================
# CHARGEMENT DES DONNEES
# +++ INPUT DATA +++

training <- read.table('pml-training.csv', sep = ',', header = TRUE)
testing <- read.table('pml-testing.csv', sep = ',', header = TRUE)
dim(training)

#========================================================================
# TRI DES VARIABLES

nsv <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[ , which(nsv$nzv == FALSE)]
dim(training)
# supprimer les variables qui ne doivent pas servir
training <- training[, -(1:6)]
dim(training)
# il reste des variables pleines de NaN
training <- training[, colSums(is.na(training)) < 1000]
dim(training)

# Prévisualisations
featurePlot(x = select(training, contains("arm_")), 
    y = training$classe, 
    plot = "box")
qplot(magnet_forearm_z, magnet_arm_z, 
    col = classe, alpha = .01, 
    data = training) +
    geom_smooth()
qplot(magnet_arm_x, col = classe, data = training, geom = "density")
    
# Centrage et échelle des variables
preObj <- preProcess(training[, -53], method = c("center", "scale"))
trainingPC <- predict(preObj, training)     

# Ca, c'est juste une visu de plus, comme ça
#featurePlot(x = select(trainingPC, contains("magnet_forearm")), 
#    y = trainingPC$classe, 
#    plot = "box")

# Et ça, un autre prétraitement, en composantes principales (le problème, c'est que le sens des données est moins évident ensuite)
#preObj <- preProcess(training[, -53], method = "pca", pcaComp = 10)
#trainingPC <- predict(preObj, training)

#========================================================================
# CROSS-VALIDATION
# +++ ALGORITHM  +++

# train methods : 
#Possible values are found using ‘names(getModelInfo())’. 
#See <URL: http://topepo.github.io/caret/bytag.html>. 
#A list of functions can also be passed for a custom model function. 
#See <URL: http://topepo.github.io/caret/custom_models.html> for details.

ctrl <- trainControl(method = "repeatedcv",
    number = 5,
    repeats = 4,
    classProbs = TRUE)
library(tictoc)
#ctrl <- trainControl(method = "cv", number = 10)
tic('Train Caret')    
model <- train(classe ~ .,
    data = training,
    method = "rpart",
    tuneLength = 10,
    trControl = ctrl)
    ,
#    preProc = c("center", "scale"))
toc()    
#model$resample # accuracy en fonction du resample
mean(model$resample$Accuracy) # valeur fournie par le modèle !
model$finalModel # le modèle final de l'arbre

# Accuracy/time en fonction de la méthode
# rpart :       .684 /  88s
# pls :         .580 / 180s
# svmRadial :   trop long
# rf :          
# nb :          

data(iris)

# modelglm <- train(Species ~ ., data = iris, method = 'glm') # KO
#model <- train(Species ~ ., data = iris, method = 'bagEarth')# KO
modelpls <- train(Species ~ ., data = iris, method = 'pls') # Total: 1.79
modelrp <- train(Species ~ ., data = iris, method = 'rpart') #Total: 1.96
model <- train(Species ~ ., data = iris, method = 'svmRadial')#Total:3.03
modelrf <- train(Species ~ ., data = iris, method = 'rf') # Total : 4.54
modelnb <- train(Species ~ ., data = iris, method = 'nb') # Total :4.81
modelgbm <- train(Species ~ ., data = iris, method = 'gbm') # Total :6.25
modeltb <- train(Species ~ ., data = iris, method = 'treebag')#Total:8.73
modelrda <- train(Species ~ ., data = iris, method = 'rda') # Total 10.0
modelfda <- train(Species ~ ., data = iris, method = 'bagFDA')#Total:128

metric = "ROC"
traincontrol "repeatedcv"

print modFit$finalModel
plot(modFit$finalModel, uniform = TRUE, main = "Arbre")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = .8)
library(rattle)
# avec rpart
fancyRpartPlot(modFit$finalModel)

# seulement avec random forest
varImpPlot(model$finalModel)

# k-folds
createFolds W2 pdf 11

#========================================================================
# PREVOIR OUT OF SAMPLE ERROR
# +++ PARAMETERS +++

# Crée 5 listes d'index avec à chaque fois 80% du total, bien répartis
partition <- createFolds(training$classe, k = 10, 
    list = TRUE, returnTrain = TRUE)
precision <- c()
in_sample_accuracy <- c()
# Pour chaque partition...
library(rpart)
for (nb in 1:10) {
    # création du modèle sur le n-ième folder
    modele <- rpart(classe ~ ., 
        data = training[partition[[nb]], ], method = "class")
    # prédiction sur les cas mis de côté
    prediction <- predict(object = modele, 
        newdata = training[-partition[[nb]], ], type = "class")
    attendu <- training[-partition[[nb]], ]$classe
    # on garde que "accuracy"
    precision[nb] <- confusionMatrix(prediction, attendu)$overall[[1]]
    # Et le in-sample ??
    predIn <- predict(object = modele, 
        newdata = training[partition[[nb]], ], type = "class")
    in_sample_accuracy[nb] <- confusionMatrix(predIn, 
        training[partition[[nb]], ]$classe)$overall[[1]]
}
precision           # .747 - .744
in_sample_accuracy  # .754 - .751

#========================================================================
# APPLIQUER SUR 20 CAS TEST
# +++ EVALUATION +++

testing

#########################################################################
# O L D
# Tri des variables
# utiliser des outils internes

modelFit <- train(classe ~ ., data = training2, method = 'rf')
modelFit

# Réponse = variable classe (dernière)

classenew <- predict(modelFit, newdata = training2)
table(classenew, training2$classe)

confusionMatrix(classenew, training2$classe)
# spécificité : j'en veux en A. Si ce n'est pas OK comme exercice, je veux être sûre de bien le dire. Sensibilité : j'en veux surtout dans les autres : s'il y a un type d'erreur, je veux dire à coup sûr quelle erreur c'est

#-------------------------------------------------------------------------
# Tri des variables --> OK
    # voir si on peut utiliser des outils internes
    nsv <- nearZeroVar(training, saveMetrics = TRUE)
    nsv$nzv 
    # standardisation à faire ?
    preObj <- preProcess(training[, -classes], 
        method = c("center", "scale"))
        method = c("BoxCox"))
        method = "PCA"))
        # directement dans le train, avec preprocess = "PCA"
# construction du modele --> en cours
# Cross validation --> à faire
    # use the training set
    # split it into training/test sets
    # build a model on the training set
    # evaluate on the test set
    # repeat and average the estimated errors
    
    # permet de:
    # - trouver les variables à inclure dans le modèle
    # - trouver la fonction de prediction à utiliser
    # - trouver les paramètres de prédiction, finalement
    
# prévoir l'erreur out of sample error --> à faire / en cours
    # sup. à error in sample
# appliquer sur 20 cas test --> à faire

#The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

#Your submission for the Peer Review portion should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).

#Course Project Prediction Quiz Portion

#Apply your machine learning algorithm to the 20 test cases available in the test data above and submit your predictions in appropriate format to the Course Project Prediction Quiz for automated grading.

liste = c("roll_belt", "pitch_forearm", "roll_forearm", 
    "total_accel_dumbbell", "magnet_dumbbell_z", "magnet_dumbbell_y")
featurePlot(x = training[, liste],
            y = training$classe, 
            plot = "box")
qplot(roll_belt, color = classe, data = training, geom = "density")

#------------------------------------------------------------------------
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p = .75, list = FALSE)
training <- spam[ inTrain,]
testing  <- spam[-inTrain,]
# Création de la matrice de corrélations entre variables
M <- abs(cor(training[, -58]))
diag(M) <- 0
# fournit les coordonnées des points de la matrice supérieurs à 0.8
which(M > .8, arr.ind = TRUE)
plot(spam[, 34], spam[, 32])

X <- .71 * training$num415 + .71 * training$num857
Y <- .71 * training$num415 - .71 * training$num857
plot(X, Y)

smallSpam <- spam[, c(34, 32)]
prComp <- prcomp(smallSpam)

typeColor <- ((spam$type == 'spam')*1 + 1)
prComp <- prcomp(log10(spam[, -58] + 1))
plot(prComp$x[, 1], prComp$x[, 2], col = typeColor, 
    xlab = 'PC1', ylab = 'PC2')

