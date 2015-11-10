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