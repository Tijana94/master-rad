mlr_learn <- function(data, inputNames, outputNames) {
  n <- nrow(data)
  nTrain <- 0.8 * n
  
  set.seed(5)
  
  trainIndexes <- sample(n, nTrain, replace=F)
  trainSet <- data[trainIndexes,]
  testSet <- data[-trainIndexes,]
  
  inputs <- paste(inputNames, collapse = "+")
  formula <- paste(outputNames, inputs, sep = "~")

  mlr <- lm(as.formula(formula), data = trainSet)
  
  predictionTrain <- predict(mlr, trainSet[,inputNames])
  
  mlr_RMSE_train <- rmseMetric(predictionTrain, trainSet[,outputNames])
  cat ("RMSE skupa podataka za obucavanje visestrukog linearnog modela: ",mlr_RMSE_train,"\n")
  
  predictionTest <- predict(mlr, testSet[,inputNames])
  
  mlr_RMSE_test <- rmseMetric(predictionTest, testSet[,outputNames])
  cat ("RMSE skupa podataka za testiranje visestrukog linearnog modela: ",mlr_RMSE_test,"\n")
  
  print(coef(mlr))
  
  #graphic Train
  
  main <- "Predikcija izlaza nad skupom podataka za obucavanje modela"
  graphics_comparePredAndMeasure(predictionTrain, trainSet[,outputNames], outputNames, main)
  
  #graphic Test
  
  main <- "Predikcija izlaza nad skupom podataka za testiranje modela"
  graphics_comparePredAndMeasure(predictionTest, testSet[,outputNames], outputNames, main)
  
  return (mlr)
}

mlr_stepWise_regression <- function(mlr, data, inputNames, outputNames) {
  n <- nrow(data)
  nTrain <- 0.8 * n
  
  set.seed(5)
  
  trainIndexes <- sample(n, nTrain, replace=F)
  trainSet <- data[trainIndexes,]
  testSet <- data[-trainIndexes,]
  
  stepwise_mlr <- ols_step_forward_p(mlr, pent = 0.1, prem = 0.3, details = F)
  
  predictionTrain <- predict(stepwise_mlr$model, trainSet[,inputNames])
  
  mlr_RMSE_train <- rmseMetric(predictionTrain, trainSet[,outputNames])
  cat ("RMSE skupa podataka za obucavanje stepwise forward: ",mlr_RMSE_train,"\n")
  
  predictionTest <- predict(stepwise_mlr$model, testSet[,inputNames])
  
  mlr_RMSE_test <- rmseMetric(predictionTest, testSet[,outputNames])
  cat ("RMSE skupa podataka za testiranje stepwise forward: ",mlr_RMSE_test,"\n")
  
  print(coef(stepwise_mlr$model))
  
  #graphic Train
  
  main <- "Predikcija izlaza nad skupom podataka za obucavanje modela"
  graphics_comparePredAndMeasure(predictionTrain, trainSet[,outputNames], outputNames, main)
  
  #graphic Test
  
  main <- "Predikcija izlaza nad skupom podataka za testiranje modela"
  graphics_comparePredAndMeasure(predictionTest, testSet[,outputNames], outputNames, main)
  
  return (stepwise_mlr)
}

mlr_crossValidation <- function(data, inputNames, outputNames) {
  set.seed(5)
  samp <- sample(nrow(data))
  data <- data[samp,]
  
  inputs <- paste(inputNames, collapse = "+")
  formula <- paste(outputNames, inputs, sep = "~")
  
  folds <- cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)
  best_rmse_MLR <- Inf
  sum_rmse_MLR <- 0.0
  RMSE_MLR_CV <- c()
  
  for(i in 1:10) {
    testIndexes <- which(folds == i, arr.ind = T)
    
    testSet <- data[testIndexes,]
    trainSet <- data[-testIndexes,]
    
    MLR_CV <- glm(as.formula(formula), data=trainSet)
    
    predictionTest <- predict(MLR_CV, testSet[,inputNames])
    
    RMSE_MLR_CV[i] <- rmseMetric(predictionTest,testSet[,outputNames])
    sum_rmse_MLR <- sum_rmse_MLR + RMSE_MLR_CV[i]
    cat("RMSE tekuceg modela: ",RMSE_MLR_CV[i],"\n")
    
    if(RMSE_MLR_CV[i]<best_rmse_MLR){
      bestModelMLR <- MLR_CV
      best_rmse_MLR <- RMSE_MLR_CV[i]
    }
  }
  
  avg_rmse_MLR <- sum_rmse_MLR / 10.0
  cat("Prosecna RMSE MLR CV: ",avg_rmse_MLR,"\n")
  cat("Minimalna RMSE MLR CV: ",best_rmse_MLR,"\n")
  
  hist(RMSE_MLR_CV, density=35, main = "RMSE MLR CV kroz 10 iteracija", xlab = "Vrednost RMSE", col="blue", border="black")
  abline(v=mean(RMSE_MLR_CV), lwd=3, col="red")
  
  min = min(RMSE_MLR_CV)
  max = max(RMSE_MLR_CV)
  xlab <- paste("Broj iteracija ")
  ylab <- paste("RMSE test ", outputNames, sep='')
  main <- "RMSE pojedinacnih iteracija u odnosu na prosecan RMSE"
  
  plot(c(1:10), RMSE_MLR_CV, col='red', main=main, pch=18, cex=1, xlab = xlab, ylab = ylab, xlim = c(1, 10), ylim = c(min,max))
  
  abline(h=mean(RMSE_MLR_CV),lwd=2, col="blue")
  
  boxplot(RMSE_MLR_CV, horizontal = TRUE, main = "RMSE kroz 10 iteracija", xlab = "RMSE")
}

mlr_showSignificantVars <- function (mlr, data, inputNames, significantNames, outputNames) {
  numOfData <- 20
  
  values <- expand.grid('first' = seq(from=(min(data[,significantNames[1,]$vars]) + 0.1*getRange(data[,significantNames[1,]$vars])),to=(max(data[,significantNames[1,]$vars])-0.1*getRange(data[,significantNames[1,]$vars])),length.out = numOfData),
                        'second' = seq(from=(min(data[,significantNames[2,]$vars]) + 0.1*getRange(data[,significantNames[2,]$vars])),to=(max(data[,significantNames[2,]$vars])-0.1*getRange(data[,significantNames[2,]$vars])),length.out = numOfData))
  
  dataMesh <- as.data.frame(matrix(ncol = length(inputNames)))
  colnames(dataMesh) <- inputNames
  dataMesh <- dataMesh[c(-1),]
  
  means <- apply(data[,inputNames], 2, mean)
  
  for(i in 1:400) {
    dataMesh[nrow(dataMesh)+1,] <- as.vector(means)
  }
  
  dataMesh[,significantNames[1,]$vars] <- values$first
  dataMesh[,significantNames[2,]$vars] <- values$second
  
  #predikcija
  prediction <- predict(mlr,dataMesh[,inputNames])
  
  #dijagram
  plot_ly(x = ~dataMesh[,significantNames[1,]$vars], y = ~dataMesh[,significantNames[2,]$vars], z = ~as.vector(prediction),type="mesh3d") %>%
    layout(scene = list(xaxis = list(title = significantNames[1,]$vars,range=c(min(dataMesh[,significantNames[1,]$vars]),max(dataMesh[,significantNames[1,]$vars]))),
                        yaxis = list(title = significantNames[2,]$vars,range=c(min(dataMesh[,significantNames[2,]$vars]),max(dataMesh[,significantNames[2,]$vars]))),
                        zaxis = list(title = outputNames,range=c(min(as.vector(prediction)),max(as.vector(prediction))))))
  
}