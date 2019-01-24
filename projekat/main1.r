install.packages("dplyr")
install.packages("psych")
install.packages("corrplot")
install.packages("neuralnet")
install.packages("NeuralNetTools")
install.packages("plotly")
install.packages("pso")
install.packages("xlsx")
install.packages("ggplot2")

library("dplyr")
library("psych")
library("corrplot")
library("neuralnet")
library("NeuralNetTools")
library("plotly")
library("pso")
library("xlsx")
library("ggplot2")

#use other files

setwd("C:\\Users\\Andjela\\Desktop")

source("ann.r")
source("mlr.r")
source("metrics.r")
source("graphics.r")

#load data

filename <- "Uzice H2O podaci 2014-16 FINAL Mir1.xlsx"
rawData <- read.xlsx(filename, sheetName = "PodaciH2O-14-16-FINALe")

filename <- "Uzice H2O podaci 2014-16 FINAL.xlsx"
oldRawData <- read.xlsx(filename, sheetName = "PodaciH2O-14-16-FINALb")

boxplot(oldRawData$xMutnoca..NTU., horizontal = TRUE, main = "Mutnoca stari podaci")
boxplot(rawData$xMutnoca..NTU., horizontal = TRUE, main = "Mutnoca novi podaci")

boxplot(oldRawData$xOksidabilnost..KMg04...mg.l., horizontal = TRUE, main = "Oksidabilnost stari podaci")
boxplot(rawData$xOksidabilnost..KMg04...mg.l., horizontal = TRUE, main = "Oksidabilnost novi podaci")

p = data.frame(oldRawData$Dan,oldRawData$Mesec,oldRawData$Godina,oldRawData$Vreme)
plot(oldRawData$Merenje, oldRawData$xMutnoca..NTU.)


#remove not needed variables

data <- rawData[c(8:39)]

#statistics

describe(data)

mcor <- cor(data)
round(mcor, digits=2)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black", order="AOE", type = "upper", tl.cex = 0.6, number.cex = 0.6)

#set input and output variables

inputNames <- c(names(data)[1:16], names(data[20:32]))
#inputNames <- c('xMutnoca', 'xOksidabilnost', 'xpHvrednost')

waterData <- data

#mlr

outputNames <- names(data)[17]
mlr_predicedData <- mlr_learn(waterData, inputNames, outputNames)

outputNames <- names(data)[18]
mlr_predicedData <- mlr_learn(waterData, inputNames, outputNames)

outputNames <- names(data)[19]
mlr_predicedData <- mlr_learn(waterData, inputNames, outputNames)

mlr_crossValidation(waterData, inputNames, outputNames)

#ann1

outputNames <- names(data)[17]
ann_predictedDataAl <- ann_learn(waterData, inputNames, outputNames)

outputNames <- names(data)[18]
ann_predictedDataCl <- ann_learn(waterData, inputNames, outputNames)

outputNames <- names(data)[19]
ann_predictedDataPoly <- ann_learn(waterData, inputNames, outputNames)






ann_crossValidation(waterData, inputNames, outputNames)
