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
  #print(p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="#eafeef", high="#7ccd7c"))

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

do.pca <- function(Matrix,minCount = 5)
{
  I.K <- which(rowSums(Matrix) > minCount);
  DimVec <- dim(Matrix[I.K,])
  
  Kcount.matrix <-Matrix[I.K,]
  
  res <- sapply(c(1:DimVec[2]), function(x) rank(Matrix[I.K,x]))
  
  res <- res + sapply(1:DimVec[2],function(x) sample(1000,DimVec[1],replace=T)/1000000)
  
  
  return(prcomp(t(res),center = TRUE))
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
  
  
  #print(c(.xMin,.xMax,.yMin,.yMax))
  
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
#   #normal Plot.
# 
#   plot(main = 'PCA plot!',df$x,df$y,type = 'n',
#        xlab = paste('1. PC',sprintf('[%.2f',100*.Var.Vec[1]),'% of variance]',sep=' '),
#        ylab = paste('2. PC',sprintf('[%.2f',100*.Var.Vec[2]),'% of variance]',sep=' '),
#        xlim=c(.xMin-.xAdd, .xMax + .xAdd), 
#        ylim=c(.yMin-.yAdd, .yMax + .yAdd)
#        )
#   with(df,points(x[z == .Names.Vec[1]],y[z == .Names.Vec[1]], pch = 1, col = 'blue'))
#   with(df,points(x[z == .Names.Vec[2]],y[z == .Names.Vec[2]], pch = 2, col = 'red'))
# 
# 
#   #other are there
#   if (3 %in% df$z)
#   {
#   .legend.label <- c(.legend.label,"other")
#   .legend.symbol <- c(.legend.symbol,3)
#     
#   with(df,points(x[z == 3],y[z == 3], pch = 3, col = 'black'))
#   }
# with(df,legend('bottom',legend=.legend.label,pch=.legend.symbol,title='Conditions')) 

  
  return(df)
}


plot.vennreplacement <- function(Method.Vec = c('SAMseq','DESeq2','edgeR'), threshold = 0.05)
{

calculate.vennreplacement(Method.Vec = Method.Vec, Mat.pVal = Mat.pVal, threshold = threshold)
  
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

plot.axis.function <- function(l)
{
  charZ = character()
  print(l)
  print(length(l))
  print(class(l))
  for (i in 1:length(l))
  {
    
    if (l[i]/1000000000 > 1)
    {
    .val = l[i]/1000000000
    charZ[i] = paste0(sprintf('%.2f',.val),' billion')
    }
    
    else if (l[i]/1000000 > 1)
    {
      .val = l[i]/1000000
      charZ[i] = paste0(sprintf('%.2f',.val),' million')
    }
    
    else if (l[i]/1000 > 1)
    {
      .val = l[i]/1000
      charZ[i] = paste0(sprintf('%.2f',.val),' thousand')
    }
    
    else
    {
    charZ[i] = as.character(l[i])
    }
    
  }
  
  return(charZ)
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
rownames(rekt) <- c('no match','high QL match','low QL match')
colz = rainbow(length(ooze))
barplot(rekt,beside = FALSE, col = colz)
legend('right', col = colz,legend = c('no match','high QL match','low QL match'))
}

plot.statistics.ggplot2 <- function(df)
{
  ggplot(df,aes(x = y, y = x, fill = z)) + geom_bar(stat="identity") +
    scale_fill_manual(values = c("#545454", "#238E23", "#A68064"),name="type of match") +
    labs(title = 'Sample read statistics', y = 'counts', x = 'Sample') +
    theme(legend.position="bottom") +
    scale_y_continuous(labels=plot.axis.function) +
    scale_x_discrete()
}



do.plot.naow <- function(df,title,x,y)
{
  #colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
  df <- transform(df, y=reorder(y, z) ) 
  p <- ggplot(df,aes(x = y, y = x, fill = z) ) +
    geom_bar(stat = "identity") + 
    coord_flip() + 
    labs(title = title, y = y, x = x) +
    #scale_fill_manual( values = colfunc(8) )
    #scale_fill_gradient2(low = "black",  high = "lightblue", midpoint = mean(range(df$x)))
    scale_fill_gradient2(midpoint = ceiling(mean(range(df$z))), space = 'Lab',
                         low = THREE_COL_FUNCTION[1], mid = THREE_COL_FUNCTION[2], high = THREE_COL_FUNCTION[3]
    )
  print(p)
}

ko2br.path <- function(O.data.kegg,O.data.refined,AllowedKO)
{
OJ <- slot(O.data.kegg,'ko2br.pathway')            # get ko2br object
KoZ <- which(slot(O.data.refined,'ConsensusVec'))  # find significant functions

KoZ.Ind <- KoZ <= dim(slot(OJ,'Matrix'))[1]
KoZ.red <- KoZ[KoZ.Ind]

AllowedKO <- AllowedKO[AllowedKO <= dim(slot(OJ,'Matrix'))[1]]

X <- colSums(slot(OJ,'Matrix')[KoZ.red,])
Z <- colSums(slot(OJ,'Matrix')[AllowedKO,]) 

df2 = data.frame(x = X/Z, y = unlist(slot(OJ,'Names')), z = X, stringsAsFactors = FALSE)

df2 = df2[df2$z != 0,]

  #ggplot
do.plot.naow(df2,'BRITE fraction significant','function','fraction')
  #plot
return(df2)
}

ko2br.path.A <- function(O.data.kegg,O.data.refined)
{
  OJ <- slot(O.data.kegg,'ko2br.pathway')            # get ko2br object
  KoZ <- which(slot(O.data.refined,'FlagVec') == 15)  # find significant functions
  
  KoZ.Ind <- KoZ <= dim(slot(OJ,'Matrix'))[1]
  KoZ.red <- KoZ[KoZ.Ind]
  
  X <- colSums(slot(OJ,'Matrix')[KoZ.red,])
  Z <- colSums(slot(OJ,'Matrix')) 
  
  df2 = data.frame(x = X/Z, y = unlist(slot(OJ,'Names')), z = X, stringsAsFactors = FALSE)
  
  df2 = df2[df2$z != 0,]
  
  #ggplot
  do.plot.naow(df2,'BRITE fraction significant UP [d1 vs d3]','function','fraction')
  #plot
  return(df2)
}

ko2br.path.B <- function(O.data.kegg,O.data.refined)
{
  OJ <- slot(O.data.kegg,'ko2br.pathway')            # get ko2br object
  KoZ <- which(slot(O.data.refined,'FlagVec') == 11)  # find significant functions
  
  KoZ.Ind <- KoZ <= dim(slot(OJ,'Matrix'))[1]
  KoZ.red <- KoZ[KoZ.Ind]
  
  X <- colSums(slot(OJ,'Matrix')[KoZ.red,])
  Z <- colSums(slot(OJ,'Matrix')) 
  
  df2 = data.frame(x = X/Z, y = unlist(slot(OJ,'Names')), z = X, stringsAsFactors = FALSE)
  
  df2 = df2[df2$z != 0,]
  
  #ggplot
  do.plot.naow(df2,'BRITE fraction significant DOWN [d1 vs d3]','function','fraction')
  #plot
  return(df2)
}


phancy.plot <- function(df2,O.data.kegg,O.data.refined, O.job.config)
{
  I.order = order(df2$z)
  df <- df2[I.order,]
  COOLCOLZ = THREE_COL_FUNCTION(max(df$z)+1)
  x11();
  par(oma = c(0, 15, 0, 0))
  barplot(height = df$x, names.arg = df$y, horiz = TRUE, cex.names = 1, space = c(0,2), xpd = FALSE, col = COOLCOLZ[df$z+1], las = 1)
  legend('bottomright',legend = rev(c(min(df$z),ceiling(median(df$z)),max(df$z))), fill = rev(c(COOLCOLZ[min(df$z)+1],COOLCOLZ[ceiling(median(df$z))],COOLCOLZ[max(df$z)])), title = '#Counts color code')
  par(oma = c(0, 0, 0, 0))
}




ko2br.path.counts <- function(O.job.config,O.data.kegg,O.data.refined)
{
  OJ <- slot(O.data.kegg,'ko2br.pathway')            # get ko2br object
  KoZ <- which(slot(O.data.refined,'ConsensusVec'))  # find significant functions
  
  KoZ.Ind <- KoZ <= dim(slot(OJ,'Matrix'))[1]
  KoZ.red <- KoZ[KoZ.Ind]
  
  dims <- dim(slot(OJ,'Matrix'))
  

  
  
  Allowed.KO <- slot(O.data.kegg,'KOinTax')[[as.character(slot(O.job.config,'SelectedTax'))]]
  

  
  Allowed.KO.Ind <-   c(1:dims[1]) %in% Allowed.KO

  #reduce allowed list
  #PossibleBRselection <-rowSums(slot(slot(O.data.kegg,'ko2br.pathway'),'Matrix')[,slot(O.job.config,'SelectedBR')]) > 0
  #PossibleBRselection <- PossibleBRselection[1:dims[1]]
  
  #Allowed.KO.Ind <- Allowed.KO.Ind & PossibleBRselection
  
  Forbidden.KO.Ind <- Allowed.KO.Ind == FALSE
  
  
  
  
  
  
  Sig.A <- slot(O.data.refined,'FlagVec')[1:dims[1]] == 15
  Sig.B <- slot(O.data.refined,'FlagVec')[1:dims[1]] == 11
  
  X.A <- rowSums(slot(O.data.refined,'Matrix')[,1:6])
  X.B <- rowSums(slot(O.data.refined,'Matrix')[,7:12])
  
  
  
  BR.Idx <- which(slot(O.job.config,'SelectedBR'))
  BR.Names <- unlist(slot(slot(O.data.kegg,'ko2br.pathway'),'Names'))[BR.Idx]
  
  nosig.Y.A <- sapply(BR.Idx, function(x) sum(slot(OJ,'Matrix')[Sig.A == FALSE & Sig.B == FALSE & Allowed.KO.Ind,x] == 1))
  sig.Y.A.O <- sapply(BR.Idx, function(x) sum(slot(OJ,'Matrix')[Sig.A & Allowed.KO.Ind,x] == 1))
  sig.Y.A.U <- sapply(BR.Idx, function(x) sum(slot(OJ,'Matrix')[Sig.B & Allowed.KO.Ind,x] == 1))
  
  #count based on all
  #nosig.Y.B <- sapply(BR.Idx, function(x) sum(slot(OJ,'Matrix')[Sig.A == FALSE & Sig.B == FALSE,x] == 1))
  #sig.Y.B.O <- sapply(BR.Idx, function(x) sum(slot(OJ,'Matrix')[Sig.B,x] == 1))
  #sig.Y.B.U <- sapply(BR.Idx, function(x) sum(slot(OJ,'Matrix')[Sig.A,x] == 1))

  
  df2 <- data.frame(Name = BR.Names, NonSig = nosig.Y.A, SigOver = sig.Y.A.O, SigUnder = sig.Y.A.U, TotalCounts = nosig.Y.A+sig.Y.A.O+sig.Y.A.U, SignificantCounts = sig.Y.A.O+sig.Y.A.U)
  #df2 <- data.frame(Name = BR.Names, NonSig = nosig.Y.B, SigOver = sig.Y.B.O, SigUnder = sig.Y.B.U, TotalCounts = nosig.Y.B+sig.Y.B.O+sig.Y.B.U, SignificantCounts = sig.Y.B.O+sig.Y.B.U)
  return(df2)
  
  
  
  
  
  X.A <- rowSums(slot(O.data.refined,'Matrix')[,1:6])
  X.B <- rowSums(slot(O.data.refined,'Matrix')[,7:12])
  Y.A <- sapply(1:dims[2], function(x) sum(X.A[slot(OJ,'Matrix')[Allowed.KO.Ind,x]]))
  Y.A.O <- sapply(1:dims[2], function(x) sum(X.A[slot(OJ,'Matrix')[Allowed.KO.Ind,x] & Sig.A]))
  Y.A.U <- sapply(1:dims[2], function(x) sum(X.A[slot(OJ,'Matrix')[Allowed.KO.Ind,x] & Sig.B]))
  
  Y.B <- sapply(1:dims[2], function(x) sum(X.B[slot(OJ,'Matrix')[Allowed.KO.Ind,x]]))
  Y.B.O <- sapply(1:dims[2], function(x) sum(X.B[slot(OJ,'Matrix')[Allowed.KO.Ind,x] & Sig.B]))
  Y.B.U <- sapply(1:dims[2], function(x) sum(X.B[slot(OJ,'Matrix')[Allowed.KO.Ind,x] & Sig.A]))
  
  cat(Y.A, '\n', Y.A.O, '\n', Y.A.U, '\n\n')
  
  
  non.signi.A = (Y.A - (Y.A.O + Y.A.U))/sum(Y.A);
  signi.U.A = Y.A.U / sum(Y.A)
  signi.O.A = Y.A.O / sum(Y.A)
  fraction.A <- c (non.signi.A,signi.O.A, signi.U.A)
  
  color.A <- rep(c('non signi','over','under'),each = dims[2])
  label.A <- rep(1,length(fraction.A))
  
  non.signi.B = (Y.B - (Y.B.O + Y.B.U))/sum(Y.B);
  signi.U.B = Y.B.U / sum(Y.B)
  signi.O.B = Y.B.O / sum(Y.B)
  fraction.B <- c (non.signi.B,signi.O.B, signi.U.B)
  
  color.B <- rep(c('non signi','over','under'),each = dims[2])
  label.B <- rep(2,length(fraction.B))
  
  .Counts.A <- c(c(Y.A - (Y.A.O + Y.A.U)),Y.A.O, Y.A.U)
  .Counts.B <- c(c(Y.B - (Y.B.O + Y.B.U)),Y.B.O, Y.B.U)
  
  
  
  
  
  
  
  nosig.Y.A <- sapply(1:dims[2], function(x) sum(slot(OJ,'Matrix')[Sig.A == FALSE & Sig.B == FALSE & Allowed.KO.Ind,x] == 1))
  sig.Y.A.O <- sapply(1:dims[2], function(x) sum(slot(OJ,'Matrix')[Sig.A & Allowed.KO.Ind,x] == 1))
  sig.Y.A.U <- sapply(1:dims[2], function(x) sum(slot(OJ,'Matrix')[Sig.B & Allowed.KO.Ind,x] == 1))
  
  nosig.Y.B <- sapply(1:dims[2], function(x) sum(slot(OJ,'Matrix')[Sig.A == FALSE & Sig.B == FALSE,x] == 1))
  sig.Y.B.O <- sapply(1:dims[2], function(x) sum(slot(OJ,'Matrix')[Sig.B,x] == 1))
  sig.Y.B.U <- sapply(1:dims[2], function(x) sum(slot(OJ,'Matrix')[Sig.A,x] == 1))
  
  sigfunc.counts.A <- c(nosig.Y.A,sig.Y.A.O,sig.Y.A.U)
  sigfunc.counts.B <- c(nosig.Y.B,sig.Y.B.O,sig.Y.B.U)
  
  cat(length(fraction.B),'\t',length(fraction.A),'\n',length(color.A),'\t',length(color.B),'\n')
  
  df2 = data.frame(fraction = c(fraction.A,fraction.B), functioncounts = c(sigfunc.counts.A,sigfunc.counts.B), counts = c(.Counts.A,.Counts.B), color = c(color.A,color.B),names = rep(unlist(slot(OJ,'Names')),6), z = c(label.A,label.B), stringsAsFactors = FALSE)

  
  
  
  
  
  
  X = c(Y.A/sum(Y.A),Y.B/sum(Y.B))
  Y = c(Y.A,Y.B)
  Z = c(rep(1,length(Y.A)),rep(2,length(Y.B)))
  
  df2 = data.frame(x = rep(1:dims[2],2), y = Y, fraction = X, counts = Y, names = rep(unlist(slot(OJ,'Names')),2), z = Z, stringsAsFactors = FALSE)
  
  df2 = df2[df2$y != 0,]
  return(df2);
  #ggplot
  do.plot.naow(df2,'BRITE fraction significant','function','fraction')
  #plot
  return(df2)
}