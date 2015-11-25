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
  .thresh <- calc.FilteringScore(Object.job.statistics)
  
  .setThresh = slot(Object.job.statistics,'FilteringScore')
  

  p <- ggplot(data = .DF, aes(x=x,y=y))
  p + geom_hline(xintercept = .thresh,colour = 'blue')
  print(p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="#eafeef", high="#7ccd7c"))

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


plot.pca <- function(Object.Job.Config, Object.Job.Statistics,Object.Data.Big,minCount = 5)
{
  .Class.Vec <- slot(Object.Job.Config,'ClassVec')
  .Selected.Vec <- slot(Object.Job.Config,'SelectedClasses')
  .Names.Vec <- slot(Object.Job.Config,'ClassNames')
  XMat <- slot(Object.Data.Big,'Matrix')
  .I = .Class.Vec %in% .Selected.Vec
  
  .matDims = dim(XMat)
  
  
  I.K <- which(rowSums(XMat) > minCount);
  DimVec <- dim(XMat[I.K,])
  
  Kcount.matrix <-XMat[I.K,]
  Matrix.FULL <- matrix(0,ncol=.matDims[2],nrow=.matDims[1])
  res <- sapply(c(1:DimVec[2]), function(x) rank(XMat[I.K,x]))
  print(res[1:10])
  print(sum(XMat))
  res <- res + sapply(1:DimVec[2],function(x) sample(1000,DimVec[1],replace=T)/1000000)
  Matrix.FULL[I.K,] <- res
  X2 <- prcomp(t(res),center = TRUE)
  
  
  .PCA.Mat = X2$x
  
  df = data.frame(x = NULL, y = NULL, z = NULL, label = NULL, stringsAsFactors = FALSE)
  
  #plot(X2$x[,1],X2$x[,2])
  for (i in 1:length(.Selected.Vec))
  {
    .I <- .Class.Vec == .Selected.Vec[i]
    df = rbind(df,data.frame(x = .PCA.Mat[.I,1], y = .PCA.Mat[.I,2], z = .Names.Vec[i], label = which(.I),stringsAsFactors = FALSE))
  }
  
  .I = .Class.Vec %in% .Selected.Vec
  .I = !.I
  if (sum(.I) != 0)
  {
    print(.I)
    df = rbind(df,data.frame(x = .PCA.Mat[.I,1], y = .PCA.Mat[.I,2], z = rep(other,sum(.I)), label = .which(.I), stringsAsFactors = FALSE))
  }
  
  
  .xMin = min(df$x)
  .xMax = max(df$x)
  .yMin = min(df$y)
  .yMax = max(df$y)  
  
  .xAdd = (.xMax - .xMin)*0.1
  .yAdd = (.yMax - .yMin)*0.1
  
  .legend.label <- c(.Names.Vec[.Selected.Vec])
  .legend.symbol <- 1:2 

  
  .Var.Vec = (X2$sdev)^2/sum((X2$sdev)^2)
  #GGplot
  c <- ggplot(df, aes(label = label, colour = factor(z), shape = factor(z), x=x, y=y)) + geom_point(colour="black", size = 6) + geom_point(alpha = 1, size=5)  + theme_classic() + scale_colour_discrete(name  ="Classes") + scale_shape_discrete(name  ="Classes") +
    labs(title = "PCA ggplot2", 
         x = paste('1. PC',sprintf('[%.2f',100*.Var.Vec[1]),'% of variance]',sep=' '),
         y = paste('2. PC',sprintf('[%.2f',100*.Var.Vec[2]),'% of variance]',sep=' ')) + geom_text(alpha = 1, hjust=-1, vjust=0, show_guide = FALSE) +
    xlim(.xMin-.xAdd, .xMax + .xAdd) +
    ylim(.yMin-.yAdd, .yMax + .yAdd)

  print(c)
  #normal Plot.

  plot(main = 'PCA plot!',df$x,df$y,type = 'n',
       xlab = paste('1. PC',sprintf('[%.2f',100*.Var.Vec[1]),'% of variance]',sep=' '),
       ylab = paste('2. PC',sprintf('[%.2f',100*.Var.Vec[2]),'% of variance]',sep=' '),
       xlim=c(.xMin-.xAdd, .xMax + .xAdd), 
       ylim=c(.yMin-.yAdd, .yMax + .yAdd)
       )
  with(df,points(x[z == .Names.Vec[1]],y[z == .Names.Vec[1]], pch = 1, col = 'blue'))
  with(df,points(x[z == .Names.Vec[2]],y[z == .Names.Vec[2]], pch = 2, col = 'red'))


  #other are there
  if (3 %in% df$z)
  {
  .legend.label <- c(.legend.label,"other")
  .legend.symbol <- c(.legend.symbol,3)
    
  with(df,points(x[z == 3],y[z == 3], pch = 3, col = 'black'))
  }
with(df,legend('bottom',legend=.legend.label,pch=.legend.symbol,title='Conditions')) 

  
  return(df)
}


plot.vennreplacement <- function(Method.Vec = c('SAMseq','DESeq2','edgeR'), Mat.pVal, threshold = 0.05)
{
  TF.Mat <- Mat.pVal < threshold
  
  nFeatures = dim(TF.Mat)[1]
  
  nMethods = length(Method.Vec)
  
  Logic.T <- sapply(1:nFeatures, function(x) which(TF.Mat[x,]))
  LetterMat <- sapply(1:nFeatures, function(x) paste(Method.Vec[Logic.T[[x]]],sep='',collapse=''))
  
  LetterMat <- LetterMat[nchar(LetterMat) > 0]
  
  
  
  BarPlotTable <- table(unlist(Logic.T))
  LetterTable <- table(LetterMat)
  
  #sort shit via max counts
  
  
  Ind.Order <- order(LetterTable,decreasing=TRUE)
  LetterTable <- LetterTable[Ind.Order]
  
  #LetterTable = LetterTable[nchar(names(LetterTable)) != 0]
  
  TableNames <- names(LetterTable)
  
  Logic.Method <- sapply(1:nMethods, function(x) grepl(Method.Vec[x],TableNames))
  Coords.T <- which(Logic.Method,arr.ind=TRUE) -1
  Coords.F <- which(Logic.Method == FALSE,arr.ind=TRUE) -1
  
  
  
  
  
  smallGraph = data.frame(X= 0:(nMethods-1), Y=colSums(TF.Mat))
  
  
  
  
  nPoss = sum(sapply(1:3, function(x) choose(3,x)))
  #all possible
  nLimz = nPoss + 1
  #only used
  nLimz = length(LetterTable)
  
  
  nDim = dim(Logic.Method)[1]
  
  df <- data.frame(X = c(Coords.T[,1],Coords.F[,1]),Y = c(Coords.T[,2],Coords.F[,2]), Z = c(rep(1,length(Coords.T[,1])),rep(0,length(Coords.F[,1]))))
  
  #remove no-hit part
  
  
  df2 <- data.frame(Freq = as.vector(LetterTable),LetterMat = c(0:(length(LetterTable)-1)))
  
  p <- ggplot() + geom_point(data=df,aes(x=X,y=Y, colour=factor(Z)),size=10) + theme_minimal() + theme(legend.position="none") + scale_color_manual(values = c("black","grey70")) + 
    theme(
      plot.margin = unit(c(0,0,0,0),"inches"), 
      panel.margin = unit(c(0,0,0,0),"inches"),
      #axis.text = element_blank(), 
      axis.text.x = element_blank(),
      #axis.title = element_blank(),
      #panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.margin=unit(c(0,0),'cm'),
      panel.border = element_rect(colour = 'black', fill = 'transparent'),
      panel.margin = unit(0, 'mm')) +
    scale_x_continuous(limits=c(-1, nLimz))+
    labs(x=NULL, y=NULL) +
    scale_y_continuous(breaks=c(1:nMethods), labels=Method.Vec)
  #scale_x_continuous(	breaks=c(0:31), 	labels=TableNames,	limits=c(-1, 33))
  
  
  p <- ggplot() +geom_raster(data=df,aes(x=X,y=Y, fill=factor(Z)), alpha = 1) + theme(legend.position="none") + scale_fill_manual(values = c("white","black")) + 
    theme(
      plot.margin = unit(c(0,0,0,0),"inches"), 
      panel.margin = unit(c(0,0,0,0),"inches"),
      #axis.text = element_blank(), 
      axis.text.x = element_blank(),
      #axis.title = element_blank(),
      #panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.margin=unit(c(0,0),'cm'),
      panel.border = element_rect(colour = 'black', fill = 'transparent'),
      panel.margin = unit(0, 'mm')) +
    scale_x_continuous(limits=c(-1, nLimz))+
    labs(x=NULL, y=NULL) +
    scale_y_continuous(breaks=c(0:(nMethods-1)), labels=Method.Vec)
  #scale_x_continuous(	breaks=c(0:31), 	labels=TableNames,	limits=c(-1, 33))
  
  
  
  
  p2 <- ggplot() + theme_minimal() +
    geom_bar(data=df2,aes(x=LetterMat,y=Freq),stat="identity") +
    theme(
      plot.margin = unit(c(0,0,0,0),"inches"), 
      panel.margin = unit(c(0,0,0,0),"inches"),
      axis.text.x = element_blank(), 
      #axis.title = element_blank(),
      #panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.margin=unit(c(0,0),'cm'),
      panel.border = element_rect(colour = 'black', fill = 'transparent'),
      panel.margin = unit(0, 'mm')) +
    #scale_x_continuous( breaks=c(10,20,30)) +
    labs(x=NULL,y='method agreement') +
    geom_text(data=df2,aes(x=LetterMat,y=Freq,label=Freq), vjust=-1) + 
    scale_x_continuous(limits=c(-1, nLimz)) +
    scale_y_continuous(limits=c(0,max(LetterTable)+(max(LetterTable)*0.1)))
  
  
  e <- ggplot(data=df2,aes(x=LetterMat,y=Freq)) + theme_minimal() + geom_blank() +
    theme(
      plot.margin = unit(c(0,0,0,0),"inches"), 
      panel.margin = unit(c(0,0,0,0),"inches"),
      axis.text = element_blank(), 
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.ticks.margin=unit(c(0,0),'cm'),
      panel.margin = unit(0, 'mm'))
  
  f <- ggplot() + geom_bar(data=smallGraph, aes(x=X, y=Y),stat="identity",width = 0.2) + coord_flip()+
    theme(
      plot.margin = unit(c(0,0,0,0),"inches"), 
      panel.margin = unit(c(0,0,0,0),"inches"),
      axis.text.y = element_blank(), 
      axis.title = element_blank(),
      #panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.margin=unit(c(0,0),'cm'),
      panel.border = element_rect(colour = 'black', fill = 'transparent'),
      panel.margin = unit(0, 'mm')) +
    #scale_x_continuous( breaks=c(10,20,30))
    xlim(-0.5, (nMethods-0.5))
  gA <- ggplotGrob(p)
  gB <- ggplotGrob(p2)
  gC <- ggplotGrob(e)
  gD <- ggplotGrob(f)
  
  maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
  maxHight = grid::unit.pmax(gA$heights[2:5], gD$heights[2:5])
  gA$widths[2:5] <- as.list(maxWidth)
  gB$widths[2:5] <- as.list(maxWidth)
  gD$heights[2:5] <- as.list(maxHight)
  gA$heights[2:5] <- as.list(maxHight)
  grid.arrange(gC,gB,gD,gA, layout_matrix=cbind(c(1,1,3),c(2,2,4),c(2,2,4),c(2,2,4)))
  
}






plot.statistics.plot <- function(df)
{
  

ooze <- unique(df$y)
  rekt <- sapply(ooze, function(x) 
  {
    .t.df <-  df[df$y == x,]
    unlist(.t.df['x'])
  }
  )

colnames(rekt) <- ooze
rownames(rekt) <- c('reads','UProCHits','filtered.score','filtered.combo','filtered.ko','filtered.tax','filtered.rna','filtered.multi','RNA')
colz = rainbow(length(ooze))
barplot(rekt,beside = TRUE, col = colz)
legend('right', col = colz,legend = c('reads','UProCHits','filtered.score','filtered.combo','filtered.ko','filtered.tax','filtered.rna','filtered.multi','RNA'))
}

plot.statistics.ggplot2 <- function(df)
{
  ggplot(df,aes(x = y, y = x, fill = z)) + geom_bar(stat="identity")
}