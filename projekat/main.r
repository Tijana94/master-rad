install.packages("psych")
install.packages("corrplot")
install.packages("neuralnet")
install.packages("NeuralNetTools")
install.packages("plotly")
install.packages("pso")
install.packages("xlsx")
install.packages("olsrr")

library("psych")
library("corrplot")
library("neuralnet")
library("NeuralNetTools")
library("plotly")
library("pso")
library("xlsx")
library("olsrr")

#use other files

setwd("D:\\Fakultet\\Master\\Master rad\\praktican deo\\projekat")

source("ann.r")
source("mlr.r")
source("metrics.r")
source("graphics.r")

#load data

filename <- "Uzice H2O podaci 2014-15 FINAL kor.xlsx"
rawData <- read.xlsx(filename, sheetName = "PodaciH2O-14-15-FINALe")

#outliers

filename <- "Uzice H2O podaci 2014-15 FINAL.xlsx"
oldRawData <- read.xlsx(filename, sheetName = "PodaciH2O-14-15-FINALb")

nf <- graphics::layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 5.1, 1.1, 2.1))
boxplot(oldRawData$xMut, horizontal=TRUE,  outline=TRUE, frame=F, col = "green1", cex.axis = 2)
hist(oldRawData$xMut, col = "pink", main = "Mutnoca pre korekcije", xlab = "Mutnoca [NTU]", ylab = "Frekvencija", cex.lab = 2, cex.axis = 2, cex.main = 2)

nf <- graphics::layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 5.1, 1.1, 2.1))
boxplot(rawData$xMut, horizontal=TRUE,  outline=TRUE, frame=F, col = "green1", cex.axis = 2)
hist(rawData$xMut, col = "pink", main = "Mutnoca nakon korekcije", xlab = "Mutnoca [NTU]", ylab = "Frekvencija", cex.lab = 2, cex.axis = 2, cex.main = 2)

nf <- graphics::layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 5.1, 1.1, 2.1))
boxplot(oldRawData$xOksid, horizontal=TRUE,  outline=TRUE, frame=F, col = "red", cex.axis = 2)
hist(oldRawData$xOksid, col = "pink", main = "Oksidabilnost pre korekcije", xlab = "Oksidabilnost [mg/l]", ylab = "Frekvencija", cex.lab = 2, cex.axis = 2, cex.main = 2)

nf <- graphics::layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(mar=c(5.1, 5.1, 1.1, 2.1))
boxplot(rawData$xOksid, horizontal=TRUE,  outline=TRUE, frame=F, col = "red", cex.axis = 2)
hist(rawData$xOksid, col = "pink", main = "Oksidabilnost nakon korekcije", xlab = "Oksidabilnost [mg/l]", ylab = "Frekvencija", cex.lab = 2, cex.axis = 2, cex.main = 2)

#remove not needed variables

data <- rawData[c(8:38)]

#statistics

apply(data, 2, function(x) sum(is.na(x)))

describe(data)

mcor <- cor(data)
round(mcor, digits=2)

par(mfrow=c(1,1))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method="shade", shade.col=NA, tl.col="black", col=col(200), addCoef.col="black", order="AOE", type = "upper", tl.cex = 1.2, number.cex = 0.6, cl.cex = 1.2)

#set input and output variables

inputNames <- c(names(data)[1:15], names(data[19:31]))
outputNamesAl <- names(data)[16]
outputNamesCl <- names(data)[17]
outputNamesPoly <- names(data)[18]

waterData <- data

#ann1

hidden1 = c(18, 11)

annAl <- ann_learn(waterData, inputNames, outputNamesAl, hidden1)
annCl <- ann_learn(waterData, inputNames, outputNamesCl, hidden1)
annPoly <- ann_learn(waterData, inputNames, outputNamesPoly, hidden1)


hidden2 = c(36, 22)

annAl2 <- ann_learn(waterData, inputNames, outputNamesAl, hidden2)
annCl2 <- ann_learn(waterData, inputNames, outputNamesCl, hidden2)
annPoly2 <- ann_learn(waterData, inputNames, outputNamesPoly, hidden2)

#cv

ann_crossValidation(waterData, inputNames, outputNamesAl, hidden2)
ann_crossValidation(waterData, inputNames, outputNamesCl, hidden2)
ann_crossValidation(waterData, inputNames, outputNamesPoly, hidden2)

#ann significance

significanceAl <- ann_significance(annAl2)
print(significanceAl)
significanceCl <- ann_significance(annCl2)
print(significanceCl)
significancePoly <- ann_significance(annPoly2)
print(significancePoly)

#ANN 3d plot

ann_showSignificantVars(annAl2, waterData, inputNames, c(as.character(significanceAl$x_names[1]), as.character(significanceAl$x_names[2])), outputNamesAl)
ann_showSignificantVars(annCl2, waterData, inputNames, c(as.character(significanceCl$x_names[1]), as.character(significanceCl$x_names[2])), outputNamesCl)
ann_showSignificantVars(annPoly2, waterData, inputNames, c(as.character(significancePoly$x_names[1]), as.character(significancePoly$x_names[2])), outputNamesPoly)

ann_showSignificantVars(annAl2, waterData, inputNames, c('xMut','xOksid'), outputNamesAl)
ann_showSignificantVars(annAl2, waterData, inputNames, c('ySulfat','xOksid'), outputNamesAl)

ann_showSignificantVars(annCl2, waterData, inputNames, c('xUkuAlk','xOksid'), outputNamesCl)
ann_showSignificantVars(annCl2, waterData, inputNames, c('yUkuAlk','xMut'), outputNamesCl)

ann_showSignificantVars(annPoly2, waterData, inputNames, c('xHlorid','xElektro'), outputNamesPoly)
ann_showSignificantVars(annPoly2, waterData, inputNames, c('xKalc','xUkuAlk'), outputNamesPoly)

#mlr

mlrAl <- mlr_learn(waterData, inputNames, outputNamesAl)
mlrCl <- mlr_learn(waterData, inputNames, outputNamesCl)
mlrPoly <- mlr_learn(waterData, inputNames, outputNamesPoly)

waterDataMLRAl <- waterData
waterDataMLRAl['XY'] <- waterData$ySulfat * waterData$xOksid
waterDataMLRAl['X2Y'] <- waterData$ySulfat^2 * waterData$xOksid
waterDataMLRAl['XY2'] <- waterData$ySulfat * waterData$xOksid^2

mlrAl2 <- mlr_learn(waterDataMLRAl, c(inputNames, 'XY', 'X2Y', 'XY2'), outputNamesAl)

waterDataMLRCl <- waterData
waterDataMLRCl['XY'] <- waterData$xOksid * waterData$xUkuAlk
waterDataMLRCl['X2Y'] <- waterData$xOksid^2 * waterData$xUkuAlk
waterDataMLRCl['XY2'] <- waterData$xOksid * waterData$xUkuAlk^2

mlrCl2 <- mlr_learn(waterDataMLRCl, c(inputNames, 'XY', 'X2Y', 'XY2'), outputNamesCl)

waterDataMLRPoly <- waterData
waterDataMLRPoly['XY'] <- waterData$xKalc * waterData$xElektro
waterDataMLRPoly['X2Y'] <- waterData$xKalc^2 * waterData$xElektro
waterDataMLRPoly['XY2'] <- waterData$xKalc * waterData$xElektro^2

mlrPoly2 <- mlr_learn(waterDataMLRPoly, c(inputNames, 'XY', 'X2Y', 'XY2'), outputNamesPoly)

#mlr stepwise

mlr_stepwise_Al <- mlr_stepWise_regression(mlrAl, waterData, inputNames, outputNamesAl)
mlr_stepwise_Cl <- mlr_stepWise_regression(mlrCl, waterData, inputNames, outputNamesCl)
mlr_stepwise_Poly <- mlr_stepWise_regression(mlrPoly, waterData, inputNames, outputNamesPoly)

#mlr 3d plot

mlr_showSignificantVars(mlrAl, waterData, inputNames, significanceAl[1:2,], outputNamesAl)
mlr_showSignificantVars(mlrCl, waterData, inputNames, significanceCl[1:2,], outputNamesCl)
mlr_showSignificantVars(mlrPoly, waterData, inputNames, significancePoly[1:2,], outputNamesPoly)

#cv

mlr_crossValidation(waterData, inputNames, outputNamesAl)
mlr_crossValidation(waterData, inputNames, outputNamesCl)
mlr_crossValidation(waterData, inputNames, outputNamesPoly)

#pso

pso_optimization(annAl2, waterData, inputNames, outputNamesAl)
pso_optimization(annCl2, waterData, inputNames, outputNamesCl)
pso_optimization(annPoly2, waterData, inputNames, outputNamesPoly)