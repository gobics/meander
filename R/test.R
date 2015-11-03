#
# tmp.max = 0;
# for (i in 1:5)
# {
#   Z <- .process.uproc(paste('/home/hklingen/workspace/uproc-taxy/rodica/results/Forst_0',i,'.uproc',sep=''),paste('/home/hklingen/projects/TMP/qq',i,'.rds',sep=''), TRUE, TRUE, NULL, tmp.max)
#   tmp.max = max(Z)
# }
#
#
#
#
# for (i in 1:5)
# {
#   Q <- readRDS(paste('/home/hklingen/projects/TMP/qq',i,'.rds',sep=''))
#   X <- hist(Q[,score], breaks = Z,plot=FALSE)
#   plot.frame = rbind(plot.frame,data.frame(x = X$mids, y = X$counts/sum(X$counts), z = rep(i,length(X$counts))))
# }
#
# .plot.frame = data.frame(x = NULL, y = NULL, z = NULL)
UProcScorePlot <- function(plot.frame,rds.input,breaks,sample)
{
  .Q <- readRDS(paste('/home/hklingen/projects/TMP/qq',i,'.rds',sep=''))
  .X <- hist(.Q[,score], breaks = breaks,plot=FALSE)
  plot.frame = rbind(plot.frame,data.frame(x = .X$mids, y = .X$counts/sum(X$counts), z = rep(sample,length(.X$counts))))
  return(plot.frame)
}


#plot.lines(plot.frame,cl,'xTEST','yTEST','titleTEST')
plot.lines <- function(plot.frame,colorVec,xLabel,yLabel,mainTitle)
{
#cl <- rainbow(5)
#plot(0,0,xlim = c(0,max(plot.frame[,'x'])+(max(plot.frame[,'x'])*0.1)),ylim = c(0,max(plot.frame[,'y'])+(max(plot.frame[,'y'])*0.1)),type = "n",xlab = 'UProC-score', ylab = 'Fraction Hits')
#for (i in 1:5)
#{
#  lines(plot.frame[plot.frame[,'z'] == i,'x'],plot.frame[plot.frame[,'z'] == i,'y'],col = colorVec[i])
#}
#legend(x = 'topleft', legend = unique(plot.frame[,'z']), fill = colorVec)
samples.unique <- unique(plot.frame[,'z'])

  plot(0,0,xlim = c(0,max(plot.frame[,'x'])+(max(plot.frame[,'x'])*0.1)),ylim = c(0,max(plot.frame[,'y'])+(max(plot.frame[,'y'])*0.1)),type = "n",xlab = xLabel, ylab = yLabel, main = mainTitle)
    for (i in 1:length(samples.unique))
    {
      lines(plot.frame[plot.frame[,'z'] == i,'x'],plot.frame[plot.frame[,'z'] == i,'y'],col = colorVec[i])
    }
  legend(x = 'topleft', legend = samples.unique, fill = colorVec)

}



