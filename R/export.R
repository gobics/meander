export.csv <- function()
{
basePath = file.path(slot(NEW$Object.job.path,'DirOut'),'HTML',slot(NEW$Object.job,'SelectedTax'))

ret.list <- save.object.datatable()

file.names <- c('UProC_scores.csv','UProC_hits.csv')

  for (i in 1:length(ret.list))
  {
    if (length(ret.list[[i]]) > 0)
    {
      write.csv(x = ret.list[[i]], file = file.path(basePath,file.names[i]), row.names = FALSE)
    }    
  }
}

#save.object.datatable(NEW$Object.data.dataframes,'Scores.Samples')

save.object.datatable <- function(Object,slot.name)
{
  #collect data and store into data.tables
  for (i in 1:length())
    #uproc-score
    curr <- data.frame()
    curr.modi <- slot(NEW$Object.data.dataframes,'Scores.Samples')
    
      if (length(curr.modi) > 0)
      {
        rownames(curr.modi) <- NULL
        colnames(curr.modi) <- c('Sample','Score','fraction of reads')
        curr <- curr.modi
      }

   #PCA, Venn, BR
    
    
    
    
    ##Venn
    #plot.generate.vennreplacement
    
    #read statistics
    
    
    return(curr)
}


save.figures <- function()
{
  basePath = file.path(slot(NEW$Object.job.path,'DirOut'),'HTML',slot(NEW$Object.job.config,'SelectedTax'))
  
  #PCA
  postscript(file.path(basePath,'PCA.eps'))
  df <- plot.pca(NEW$Object.job.config, NEW$Object.job.statistics,NEW$Object.data.big, minCount = 5)
  dev.off()
  
  #BR
  postscript(file.path(basePath,'BR.eps'))
  df <- ko2br.path.counts(NEW$Object.job.config,NEW$Object.data.kegg,NEW$Object.data.refined)
  df2 <- data.frame(Counts = c(df$SigOver,df$SigUnder), x = rep(df$Name,2), y = c(df$SigOver/df$TotalCounts,df$SigUnder/df$TotalCounts), z = c(rep('SigOver',length(df$SigOver)),rep('SigUnder',length(df$SigOver))))
  positions <- df[order((df$SignificantCounts/df$TotalCounts) * df$SignificantCounts, decreasing=FALSE),'Name']
  print(ggplot(df2, aes(x = x, y = y, color = factor(z), group = factor(z), fill = Counts)) + scale_x_discrete(limits = positions) + geom_bar(position = "dodge",stat="identity") + coord_flip() + scale_fill_gradient(low = "lightblue", high = "darkblue") +
  ylab('fraction significant') +
  xlab('kegg br categories'))
  dev.off()
  
  #Venn
  postscript(file.path(basePath,'venn.eps'))
  xxx <- plot.generate.vennreplacement(Method.Vec = slot(NEW$Object.job.config,'Methods'), Mat.pVal = NEW$Object.data.refined@ConsensusMat, threshold = 0.05)
  dev.off()
}