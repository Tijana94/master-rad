install.packages("xlsx")
library("xlsx")

setwd('D:\\Fakultet\\Master\\Master rad\\praktican deo\\projekat')

## prepare data frame
filename <- 'data\\cistaVoda2015\\Apr ok\\01\\FizickoHemijskeAnalize_DnevnaApr01.xls'

columns <- read.xlsx(filename, sheetName = 'Podaci', colIndex = c(2), startRow = 10, endRow = 42)

names = c(names(columns), as.vector(columns[[1]]), 'day', 'month', 'year', 'time')
result <- as.data.frame(matrix(ncol = length(names)))
colnames(result) <- names
result <- result[c(-1),]
#########

#load files

filenames <- list.files(path='.', recursive = T, pattern = '.*FizickoHemijskeAnalize_Dnevna.*xls.*')

for(filename in filenames) {
  dfData <- read.xlsx(filename, sheetName = 'Podaci', colIndex = c(2, 6, 15), startRow = 10, endRow = 42)
  date <- read.xlsx(filename, sheetName = 'Podaci', colIndex = c(1), startRow = 5, endRow = 7)
  
  #split date
  str <- strsplit(as.vector(date[[1]][1]), ':')[[1]][2]
  str1 <- strsplit(str, '[.]')
  day <- str1[[1]][1]
  month <- str1[[1]][2]
  year <- str1[[1]][3]
  time <- '8:00'
  
  result[nrow(result)+1,] = c(as.vector(dfData[[2]]), day, month, year, time)
  
  if (length(dfData) == 3) {
    time <- '20:00'
    result[nrow(result)+1,] = c(as.vector(dfData[[3]]), day, month, year, time)
  } else {
    time <- '20:00'
    result[nrow(result)+1,] = c(rep(NA,length(names)-4), day, month, year, time)
  }
}

write.xlsx(result, 'AnalizaCistaVoda2015.xlsx')