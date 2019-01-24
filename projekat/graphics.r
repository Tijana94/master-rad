graphics_comparePredAndMeasure <- function (v1, v2, outputName, main) {
  min = min(min(v1), min(v2))
  max = max(max(v1), max(v2))
  xlab <- paste("Predikcija ", outputName, sep='')
  ylab <- paste("Merenje ", outputName, sep='')
  
  par(mar=c(5,5.5,5,5))
  plot(v1, v2, col='red', main=main, pch=18, cex=1, xlab = xlab, ylab = ylab, xlim = c(min, max), ylim = c(min,max), cex.lab = 2.8, cex.axis = 2.8, cex.main = 2.8)
  abline(0,1,lwd=2)
}