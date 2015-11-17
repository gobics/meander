perform.samr <- function(count.matrix,DiffCond)
{
  rownames(count.matrix) <- c(1:dim(count.matrix)[1])
  I.SAM <- which(rowSums(count.matrix) > 0);
  Samcount.matrix <-count.matrix[I.SAM,]
  SAMseq.FDRFULL <- vector(mode='numeric',length=dim(count.matrix)[1])+1
  
  
  SAMseq.test = SAMseq(Samcount.matrix, factor(DiffCond), resp.type = "Two class unpaired", geneid = rownames(Samcount.matrix), genenames = rownames(Samcount.matrix), nperms = 100, nresamp = 20, fdr.output = 1)
  SAMseq.result.table = rbind(SAMseq.test$siggenes.table$genes.up, SAMseq.test$siggenes.table$genes.lo)
  SAMseq.score = rep(0, nrow(Samcount.matrix))
  SAMseq.score[match(SAMseq.result.table[, 1], rownames(Samcount.matrix))] = as.numeric(SAMseq.result.table[, 3])
  SAMseq.FDR = rep(1, nrow(Samcount.matrix))
  SAMseq.FDR[match(SAMseq.result.table[, 1], rownames(Samcount.matrix))] = as.numeric(SAMseq.result.table[, 5])/100
  
  SAMseq.FDRFULL[I.SAM] <- SAMseq.FDR
  return(SAMseq.FDRFULL)
}

perform.deseq2 <- function(count.matrix, DiffCond)
{
  colData<-data.frame(condition=factor(DiffCond),type=rep('single-read',length(DiffCond)))
  dds <- DESeqDataSetFromMatrix(countData = count.matrix, colData = colData, design = ~ condition)
  dds <- DESeq(dds)
  res <- results(dds)
  
  res$padj[is.na(res$padj)] = 1
  
  return(res$padj)
}


#TODO!!!##############
perform.deseq <- function(Object.Job.Config,count.matrix)
{
  .Class.Vec <- slot(Object.Job.Config,'ClassVec')
  .Selected.Vec <- slot(Object.Job.Config,'SelectedClasses')
  
  .I = .Class.Vec %in% .Selected.Vec
  
  
  count.matrix = count.matrix[,.I]
  DiffCond = .Class.Vec[.I]
  
  print(.I)
  print(DiffCond)
  return()
  
UCond = unique(factor(DiffCond))  

cds = newCountDataSet(as.data.frame(count.matrix), factor(DiffCond))
cds = estimateSizeFactors( cds )
cds = estimateDispersions( cds )
res = nbinomTest( cds, UCond[1], UCond[2] )
return(res$padj)

}
##############TODO!##

perform.edger <- function(count.matrix, DiffCond)
{
  edgeR.dgelist = DGEList(counts = count.matrix, group = factor(DiffCond))
  edgeR.dgelist = calcNormFactors(edgeR.dgelist, method = "TMM")
  edgeR.dgelist = estimateCommonDisp(edgeR.dgelist)
  edgeR.dgelist = estimateTagwiseDisp(edgeR.dgelist, trend = "movingave")
  edgeR.test = exactTest(edgeR.dgelist)
  edgeR.pvalues = edgeR.test$table$PValue
  edgeR.adjpvalues = p.adjust(edgeR.pvalues, method = "BH")
  return(edgeR.adjpvalues)
}