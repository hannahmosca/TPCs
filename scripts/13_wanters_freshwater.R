
require(ncdf4)
install.packages("fields")
require(fields)
library(here)
plotTemperature <- function(Temp, lon, lat, fileName = "averageWaterTemperature_annual.png", legendTitle = "Water temperature (Celcius)", plotHeight = 750, plotWidth=1250, pointsize = 24, Qlim = 5, Tlim = 35, Tbreaks=0.25, PlotBreaks = 5, PlotFactor = 0.025, cols=colorRampPalette(rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))), sides=1, unit="C", ylim=c(-60,90), xlim=c(-168.5,191.5), size=1.3){
  mask = matrix(NA, length(lon), length(lat))
  mask[discharge > Qlim] = 1
  if(ylim[2] < 90){
    selLon = which(lon >= xlim[1] & lon <= xlim[2])
    selLat = which(lat >= ylim[1] & lat <= ylim[2])
    mask[1:selLon[1]] = NA
    mask[tail(selLon[2],1):length(lon)] = NA
    mask[1:selLat[1]] = NA
    mask[tail(selLat[2],1):length(lat)] = NA
  }
  
  demPlot = matrix(1, length(lon), length(lat))
  demPlot[is.na(discharge)] = NA
  
  toPlot = Temp
  toPlot[is.na(mask)] = NA
  toPlot[toPlot > Tlim] = Tlim
  if(sides == 2){
    toPlot[toPlot < -Tlim] = -Tlim
  }
  coordSel = which(is.na(toPlot) == FALSE, arr.ind=T)
  pointSel = which(is.na(toPlot) == FALSE)
  
  breaks = seq(0,Tlim,Tbreaks)
  Legendbreaks = seq(0,Tlim,PlotBreaks)
  if(sides == 2){
    breaks = seq(-Tlim,Tlim,Tbreaks)
    Legendbreaks = seq(-Tlim,Tlim,PlotBreaks)
  }
  cols = cols
  ncols = length(breaks)-1
  
  png(fileName, height=plotHeight, width = plotWidth, pointsize=pointsize)
  A = matrix(1, 9, 1)
  A[9,] = 2
  layout(A)
  par(mar=c(0.5,0.5,0.5,0.5), bg="white")
  image(lon, lat ,demPlot, col = 1,
        main="", ylim=ylim, xlim=xlim, axes=FALSE, xlab="", ylab="")
  image(lon+360, lat ,demPlot, col = 1,
        main="", axes=FALSE, xlab="", ylab="", add=T)
  print(range(toPlot[pointSel], na.rm=T))
  col = cols(ncols)[as.numeric(cut(toPlot[pointSel], breaks=ncols))]
  points(lon[coordSel[,1]], lat[coordSel[,2]], col=col, 
         cex=(discharge[pointSel])^PlotFactor-1, pch=15)
  points(lon[coordSel[,1]]+360, lat[coordSel[,2]], col=col, 
         cex=(discharge[pointSel])^PlotFactor-1, pch=15)
  
  par(mar=c(0.2,0.2,0.2,0.2))
  plot(1, 1, xlim=c(0,1), ylim=c(0,1), type="n", axes=FALSE, xaxs="i", yaxs="i")
  symbols(seq(0.1, 0.9,length=ncols), rep(0.5,ncols), rectangles = matrix(rep(c(1/(ncols),0.4)), ncols, 2, byrow=TRUE), add=TRUE, fg=cols(ncols), bg=cols(ncols), inches=FALSE)
  text(seq(0.1,0.9, length=length(Legendbreaks)),rep(0.15, length(Legendbreaks)), paste(Legendbreaks, unit, sep=""), cex=size)
  text(0.5,0.85, legendTitle, cex=size)
  
  dev.off()
}

NC = nc_open(here("raw-data", "discharge_Avg.nc"))
discharge = ncvar_get(NC, "discharge")
nc_close(NC)

NC = nc_open(here("raw-data","waterTempAnnual_merged_1979-2014.nc"))
lat = ncvar_get(NC, "lat")
lon = ncvar_get(NC, "lon")

Temp = ncvar_get(NC, "waterTemp", start=c(1,1,1), count=c(4320,2160,1))

plotTemperature(Temp-273.15, lon, lat, fileName = "temperaturePlot.png", Qlim=3)

nc_close(NC)



summary(discharge)
hist(discharge)
quantile(discharge, c(0.5, 0.9, 0.99), na.rm = TRUE)


