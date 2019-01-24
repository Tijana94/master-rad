ann_learn <- function(data, inputNames, outputNames, hidden) {
  options(digits=20)

  n <- nrow(data)
  nTrain <- 0.8*n
  
  set.seed(5)
  
  trainIndexes <- sample(n,nTrain,replace=F)
  trainSet <- data[trainIndexes,]
  testSet <- data[-trainIndexes,]
  
  dataNorm <- normalizeDataFrame(data)
  trainSetNorm <- dataNorm[trainIndexes,]
  testSetNorm <- dataNorm[-trainIndexes,]
  
  inputs <- paste(inputNames, collapse = "+")
  formula <- paste(outputNames, inputs, sep = "~")
  
  ann <- neuralnet(as.formula(formula), data = trainSetNorm, hidden = hidden, linear.output = T)
  plot(ann)
  
  #prediction train set
  predictionNormTrain <- compute(ann,trainSetNorm[,inputNames])
  predictionTrain <- unnormalizeDataFrame(predictionNormTrain$net.result, min(data[,outputNames]), max(data[,outputNames]))
  
  ann_RMSE_train <- rmseMetric(predictionTrain, trainSet[,outputNames])
  cat ("RMSE skupa podataka za treniranje neuronske mreze: ", ann_RMSE_train, "\n")
  
  #prediction train set
  predictionNormTest <- compute(ann,testSetNorm[,inputNames])
  predictionTest <- unnormalizeDataFrame(predictionNormTest$net.result, min = min(data[,outputNames]), max = max(data[,outputNames]))
  
  ann_RMSE_test <- rmseMetric(predictionTest, testSet[,outputNames])
  cat ("RMSE skupa podataka za testiranje neuronske mreze: ", ann_RMSE_test, "\n")

  #graphic Train
  main <- "Predikcija izlaza nad skupom podataka za obucavanje mreze"
  graphics_comparePredAndMeasure(predictionTrain, trainSet[,outputNames], outputNames, main)
  
  #graphic Test
  
  main <- "Predikcija izlaza nad skupom podataka za testiranje mreze"
  graphics_comparePredAndMeasure(predictionTest, testSet[,outputNames], outputNames, main)
  
  return (ann)
}

ann_significance <- function(ann) {
  significance <- olden(ann, bar_plot = T)
   
  significance$theme$axis.text$angle = 90
  significance$theme$axis.text$size = 20
  significance$theme$text$size = 20
  plot(significance)
  
  return (significance$data[order(-abs(significance$data$importance)),])
}

ann_crossValidation <- function(data, inputNames, outputNames, hidden) {

  dataNorm <- normalizeDataFrame(data)
  
  samp <- sample(nrow(dataNorm))
  dataNorm <- dataNorm[samp,]
  data <- data[samp,]
  
  inputs <- paste(inputNames, collapse = "+")
  formula <- paste(outputNames, inputs, sep = "~")
  
  folds <- cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)
  best_rmse_ANN <- Inf
  sum_rmse_ANN <- 0.0
  RMSE_ANN_CV <- c()
  
  for(i in 1:10) {
    testIndexes <- which(folds == i, arr.ind = T)
    
    testSet <- data[testIndexes,]
    testSetNorm <- dataNorm[testIndexes,]
    trainSetNorm <- dataNorm[-testIndexes,]
    
    ANN_CV <- neuralnet(as.formula(formula), data=trainSetNorm, hidden = hidden, linear.output = T)

    predictionNormTest <- compute(ANN_CV,testSetNorm[,inputNames])
    predictionTest <- unnormalizeDataFrame(predictionNormTest$net.result, min(data[,outputNames]), max(data[,outputNames]))
    
    RMSE_ANN_CV[i] <- rmseMetric(predictionTest,testSet[,outputNames])
    sum_rmse_ANN <- sum_rmse_ANN + RMSE_ANN_CV[i]
    cat("RMSE tekuceg modela: ",RMSE_ANN_CV[i],"\n")
    
    if(RMSE_ANN_CV[i]<best_rmse_ANN){
      bestModelANN <- ANN_CV
      best_rmse_ANN <- RMSE_ANN_CV[i]
    }
  }
  
  avg_rmse_ANN <- sum_rmse_ANN / 10.0
  cat("Prosecna RMSE ANN CV: ",avg_rmse_ANN,"\n")
  cat("Minimalna RMSE ANN CV: ",best_rmse_ANN,"\n")

  nf <- graphics::layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
  par(mar=c(5.1, 5.1, 1.1, 2.1))
  boxplot(RMSE_ANN_CV, horizontal=TRUE,  outline=TRUE, frame=F, col = "green1", cex.axis = 2)
  hist(RMSE_ANN_CV, col = "pink", main = "RMSE ANN CV kroz 10 iteracija", xlab = "RMSE", ylab = "Frekvencija", cex.lab = 2, cex.axis = 2, cex.main = 2)
  
  par(mfrow=c(1,1))
}

ann_showSignificantVars <- function (ann, data, inputNames, varNames, outputNames) {
  numOfData <- 20

  values <- expand.grid('first' = seq(from=(min(data[,varNames[1]]) + 0.1*getRange(data[,varNames[1]])),to=(max(data[,varNames[1]])-0.1*getRange(data[,varNames[1]])),length.out = numOfData),
                        'second' = seq(from=(min(data[,varNames[2]]) + 0.1*getRange(data[,varNames[2]])),to=(max(data[,varNames[2]])-0.1*getRange(data[,varNames[2]])),length.out = numOfData))

  dataMesh <- as.data.frame(matrix(ncol = length(inputNames)))
  colnames(dataMesh) <- inputNames
  dataMesh <- dataMesh[c(-1),]
  
  means <- apply(normalizeDataFrame(data)[,inputNames], 2, mean)
  
  for(i in 1:400) {
    dataMesh[nrow(dataMesh)+1,] <- as.vector(means)
  }

  dataMesh[,varNames[1]] <- as.vector(values$first)
  dataMesh[,varNames[2]] <- values$second
  
  #normalizacija podataka
  dataNorm <- dataMesh 
  dataNorm[,c(varNames[1],varNames[2])] <- normalizeDataFrame(dataMesh[,c(varNames[1],varNames[2])])
  
  #predikcija
  predictionNorm <- compute(ann,dataNorm[,inputNames])
  prediction <- unnormalizeDataFrame(predictionNorm$net.result, min(data[,outputNames]), max(data[,outputNames]))

  #dijagram
  plot_ly(x = ~dataMesh[,varNames[1]], y = ~dataMesh[,varNames[2]], z = ~as.vector(prediction),type="mesh3d") %>%
    layout(scene = list(xaxis = list(title = varNames[1], titlefont = list(size = 23), tickfont = list(size = 18), range=c(min(dataMesh[,varNames[1]]),max(dataMesh[,varNames[1]]))),
                        yaxis = list(title = varNames[2], titlefont = list(size = 23), tickfont = list(size = 18), range=c(min(dataMesh[,varNames[2]]),max(dataMesh[,varNames[2]]))),
                        zaxis = list(title = outputNames, titlefont = list(size = 23), tickfont = list(size = 18), range=c(min(as.vector(prediction)),max(as.vector(prediction))))))
  
}

pso_optimization <- function(ann, data, inputNames, outputNames) {
  set.seed(6)
  
  f <- function(x) {
    input <- as.data.frame(matrix(c(rawWaterInput,x), ncol=length(inputNames)))
    colnames(input) <- inputNames

    inputNorm <- normalizeDataFrame(input, apply(data[,inputNames], 2, min), apply(data[,inputNames], 2, max))

    r <- compute(ann, inputNorm)
    
    result <- unnormalizeDataFrame(r$net.result, min(data[,outputNames]), max(data[,outputNames]))

    return (result)
  }
  
  lowerLimits <- c()
  upperLimits <- c()
  
  for (i in 19:31) {
    lowerLimits[[names(data)[i]]] <- min(data[,names(data)[i]]) + 0.3*getRange(data[,names(data)[i]])
    upperLimits[[names(data)[i]]] <- max(data[,names(data)[i]]) - 0.3*getRange(data[,names(data)[i]])
  }
  
  for (i in 1:5) {
    rawWaterInput <- c()
    
    for (j in 1:15) {
      rawWaterInput[[names(data)[j]]] <- runif(n = 1, min = min(data[,j]) + 0.3*getRange(data[,j]), max = max(data[,j] - 0.3*getRange(data[,j]))) 
    }
    
    opt <- psoptim(par = rep(NA,13), f, lower = as.vector(lowerLimits),  upper = as.vector(upperLimits), control = list(trace = 0,  max.restart = 5))
    if (opt$value >= min(data[,outputNames]) && opt$value <= max(data[,outputNames])) {
      input <- as.data.frame(matrix(c(rawWaterInput,opt$par), ncol=length(inputNames)))
      colnames(input) <- inputNames
      cat("Vrednosti nezavisnih varijabli:","\n")
      print(input)
      cat("\n","\n","Vrednost izlazne varijable ",outputNames,": ",opt$value)
      cat("\n","\n","Rezultat optimizacije:","\n","\n")
      print(opt)
    }
  }
}