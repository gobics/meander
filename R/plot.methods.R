#plot.lines(plot.frame,cl,'xTEST','yTEST','titleTEST')
plot.lines <- function(plot.frame,colorVec,xLabel,yLabel,mainTitle)
{
#plot data sharing the same z with x and y as coordinates
  samples.unique <- unique(plot.frame[,'z'])
  #generate empty canvas
  plot(0,0,xlim = c(0,max(plot.frame[,'x'])+(max(plot.frame[,'x'])*0.1)),ylim = c(0,max(plot.frame[,'y'])+(max(plot.frame[,'y'])*0.1)),type = "n",xlab = xLabel, ylab = yLabel, main = mainTitle)
    for (i in 1:length(samples.unique))
    {
      lines(plot.frame[plot.frame[,'z'] == i,'x'],plot.frame[plot.frame[,'z'] == i,'y'],col = colorVec[i])
    }
  legend(x = 'topleft', legend = samples.unique, fill = colorVec)
  
}

plot.points <- function(plot.frame,colorVec,xLabel,yLabel,mainTitle,shapes.FLAG,text.FLAG)
{
  #plot data sharing the same z with x and y as coordinates
  samples.unique <- unique(plot.frame[,'z'])
  #generate empty canvas
  plot(0,0,xlim = c(0,max(plot.frame[,'x'])+(max(plot.frame[,'x'])*0.1)),ylim = c(0,max(plot.frame[,'y'])+(max(plot.frame[,'y'])*0.1)),type = "n",xlab = xLabel, ylab = yLabel, main = mainTitle)
  for (i in 1:length(samples.unique))
  {
    if (text.FLAG)
    {
    text(plot.frame[plot.frame[,'z'] == i,'x'],plot.frame[plot.frame[,'z'] == i,'y'],labels = i,adj=c(0,1))
    }
    
    if (shapes.FLAG)
    {
      points(plot.frame[plot.frame[,'z'] == i,'x'],plot.frame[plot.frame[,'z'] == i,'y'],col = colorVec[i], pch = i)  
    }
    
    else
    {
      points(plot.frame[plot.frame[,'z'] == i,'x'],plot.frame[plot.frame[,'z'] == i,'y'],col = colorVec[i])  
    }
    
  }
  if (shapes.FLAG)
  {
    legend(x = 'topleft', legend = samples.unique, fill = colorVec, pch = samples.unique)
  }
  
  else
  {
    legend(x = 'topleft', legend = samples.unique, fill = colorVec)  
  }
  
}

plot.uproc.scores <- function(Object.job.statistics,Object.data.dataframes,Type = 'QQQQQQQQ')
{
  .DF <- slot(Object.data.dataframes,'Scores.Samples')
  .thresh <- sum( slot(Object.job.statistics,'ScoreCutoff')* (slot(Object.job.statistics,'UProCHits') / sum(slot(Object.job.statistics,'UProCHits'))))
  
  .setThresh = slot(Object.job.statistics,'FilteringScore')
  
  x11()
  p <- ggplot(data = .DF, aes(x=x,y=y))
  print(p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="#eafeef", high="#7ccd7c"))
  x11()
  p <- ggplot(data = .DF, aes(x=y,y=z, group = factor(x)))
  p <- p + geom_vline(xintercept = .thresh,colour = 'blue')
    if (length(.setThresh) == 1)
    {
    p <- p + geom_vline(xintercept = .setThresh, colour = 'red')
    }
  print(p + 
          geom_line(aes(colour =factor( x)))+ 
          stat_summary(fun.x = mean, geom="line") + 
          xlab("UProC Score") + 
          ylab("Fraction of Counts") +
          scale_colour_grey(name = "Samples")
        ) 
  
  
}