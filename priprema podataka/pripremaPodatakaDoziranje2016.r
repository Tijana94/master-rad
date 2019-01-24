install.packages("xlsx")
library("xlsx")

setwd('C:\\Users\\Andjela\\Desktop\\Tehnologija16nedovrsena')

names = c('Q', 'mik', 'Poliel.', 'CI1', 'CI2', 'AI2SO4', 'day', 'month', 'year', 'time')
result <- as.data.frame(matrix(ncol = length(names)))
colnames(result) <- names
result <- result[c(-1),]
#########

#load files

filenames <- list.files(path='.', recursive = T, pattern = '.*xls.*')

month31 = c('Januar', 'Mart', 'Maj', 'Jul', 'Avgust', 'Oktobar', 'Decembar')
month30 = c('April', 'Jun', 'Septembar', 'Novembar')

for(filename in filenames) 
{
  name <- strsplit(filename,'/')[[1]][2]
  month <- strsplit(name,'16')[[1]][1]
  year <- 2016
  
  num <- 29
  if(is.element(month,month31))
    num <- 31
  else if(is.element(month,month30))
    num <- 30
  
  for(day in 1:num)
  {
    dfData <- read.xlsx(filename,header=FALSE, sheetName = paste(day,".", sep=''), colIndex = c(seq(23,28)), startRow = 6, endRow = 9)
    result[nrow(result)+1,] = c(as.vector(dfData[1,]), day, month, year, '8:00')
    result[nrow(result)+1,] = c(as.vector(dfData[2,]), day, month, year, '11:00')
    result[nrow(result)+1,] = c(as.vector(dfData[3,]), day, month, year, '14:00')
    result[nrow(result)+1,] = c(as.vector(dfData[4,]), day, month, year, '17:00')
  
    dfData <- read.xlsx(filename,header=FALSE, sheetName = paste(day,".", sep=''), colIndex = c(seq(23,28)), startRow = 21, endRow = 24)
    result[nrow(result)+1,] = c(as.vector(dfData[1,]), day, month, year, '20:00')
    result[nrow(result)+1,] = c(as.vector(dfData[2,]), day, month, year, '23:00')
    result[nrow(result)+1,] = c(as.vector(dfData[3,]), day, month, year, '2:00')
    result[nrow(result)+1,] = c(as.vector(dfData[4,]), day, month, year, '5:00')
  }
}

write.xlsx(result, '../Doziranje2016.xlsx',row.names = FALSE, col.names = FALSE, sheetName = "Podaci")