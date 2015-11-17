#Z <- .process.uproc('/home/hklingen/workspace/uproc-taxy/rodica/results/Forst_01.uproc','/home/hklingen/projects/TMP/qq3.rds', TRUE, FALSE, c(1:200000))
process.uproc <- function(uproc.filein, rds.fileout, writeoutput.FLAG, keepinput.FLAG, rna.filter, tmp.max)
{
## Input
  # uproc.file      = input from UProC Taxy
  # rds.fileout     = output as RDS file or nothing if not wanted

## Output
  # .Return         = NULL or error
############################
  #read DT
  # TODO remove nrows!!!
  .Return <- fread(uproc.filein, nrows=100000, select = c(1,3,8,9,10,11))
  setnames(.Return,c('V1','V3','V8','V9','V10','V11'),c('seq.no','length','ko','score','x','y'))




  #get length of data.table
  .nLines <- length(.Return[,x])


# sort out hits with no taxID
  .I.Tax <- .Return[,x] != 10010101
# count number of hits per sequence
  .I.singlehit <- vector(mode='logical', length = .nLines)
  .HitCountVec <- .Return[,length(ko), by=seq.no]
  .Pos <- cumsum(.HitCountVec[[2]]);
  .Pos <- .Pos[.HitCountVec[[2]] == 1]

  # set single hits == TRUE
  .I.singlehit[.Pos] <- T

# get KO00000
  .I.KO <- .Return[,x] != "K00000"

  .I.All <- .I.singlehit & .I.Tax & .I.KO


  .nKO = sum(.I.KO & !.I.singlehit & !.I.Tax)
  .nMulti = sum(!.I.KO & .I.singlehit & !.I.Tax)
  .nTax = sum(!.I.KO & !.I.singlehit & .I.Tax)
  .nAll = sum(!.I.All)
  cat(.nKO,'\t',.nMulti,'\t',.nTax,'\t',.nAll,'\n')
  #reduce data.table by removing multiple hits and unknown taxonomy
  .Return <- .Return[.I.All,]
    if (!is.null(rna.filter))
    {
      .Return = .Return[!seq.no %in% rna.filter]
    }

    if (writeoutput.FLAG)
    {
    saveRDS(object = .Return, file = rds.fileout)
    }

    if (keepinput.FLAG == FALSE)
    {
    cat(uproc.filein,"\ti would delete it now...\n")
      #uproc.filein
    #file.remove('/home/hklingen/projects/TMP/qq2.rds')
    }

    if (max(.Return[,score]) > tmp.max)
    {
    .Y = hist(.Return[,score],25,plot = FALSE)
    return(.Y$breaks)
    }

  return(TRUE)
}

filter.key.dt <- function(DT, key, Filter)
{
  #returns logical Vector of all hits matching the Filter
  .I <- DT[j = key, with = FALSE][[1]] %in% Filter
  return(.I)
}

filter.multihit.dt <- function(DT,.nRows, key)
{
  # count number of hits per sequence
  .I.singlehit <- vector(mode='logical', length = .nRows)
  .HitCountVec <- DT[,.N, by=key][['N']]

  .Pos <- cumsum(.HitCountVec);
  .Pos <- .Pos[.HitCountVec == 1]
  
  # set single hits == TRUE
  .I.singlehit[.Pos] <- T  
  #mask all others
  return(!.I.singlehit)
}

process.storeRDS <- function(Object.data.big, Object.job.path, Object.job.statistics, tmp.data.frame, Sample)
{
  .uproc.filein <- slot(Object.job.path,FILETYPE.UproC)[Sample]
  .RDS.fileout <- slot(Object.job.path,FILETYPE.RDS)[Sample]
  cat('shit:',.uproc.filein,'\n')
  options(warn=-1)
  .Return <- fread(.uproc.filein, nrows=-1, select = c(1,3,8,9,10,11))
  options(warn=1)
  
  #.Return2 <- fread(.uproc.filein, nrows=1, skip = dim(.Return)[1])
  
  #Object.job.statistics <- appendInputdata(Object.job.statistics,'UProCHits',.Return2[[1]])
  #Object.job.statistics <- appendInputdata(Object.job.statistics,'reads',.Return2[[3]])
  
  
  
  setnames(.Return,c('V1','V3','V8','V9','V10','V11'),c('seq.no','length','ko','score','x','y'))
  .Return[,ko := as.integer(substr(.Return[,ko],2,100))]
  
  #get length of data.table
  .nLines <- length(.Return[,x])
  
  .filtered.out.taxonomy <- logical()
  .filtered.out.ko <- logical()
  .filtered.out.multihit <- logical()
  .filtered.out.rna <- logical()
  
  .Q <- filter.key.dt(.Return,'x',10010101)
  
  #combine to per sequence, not per uproc hit
  .tmp = data.table(x = .Return[,seq.no], y = .Q)
  .filtered.out.taxonomy <- .tmp[,sum(y),by=x][['V1']] > 0
  
  .All.to.filter <- logical(length(.Q))
  .All.to.filter <- .All.to.filter | .Q
  
  #cat('filtered out by taxonomy:', sum(.Q),'\n',sep = '')
  
  .res <- rle(sort(round(.Return[, j = score, with = TRUE]*10)/10))
  
  .totalCountSample = sum(.res$lengths)
  
  
  .res <- rle(sort(round(.Return[i = .Q, j = score, with = TRUE]*10)/10))
  
  
  .nframe.lines = dim(tmp.data.frame)[1]
  .nRLElines = length(.res$lengths)
  
  for (i in 1:.nRLElines)
  {
   tmp.data.frame[(i+.nframe.lines),] <- list(Sample,(.res$lengths[i]/.totalCountSample), .res$values[i],'miss') 
  }
  
  


  
  .res <- rle(sort(round(.Return[i = !.Q, j = score, with = TRUE]*10)/10))

  
    .nframe.lines = dim(tmp.data.frame)[1]
  .nRLElines = length(.res$lengths)
  
  for (i in 1:.nRLElines)
  {
    tmp.data.frame[(i+.nframe.lines),] <- list(Sample,.res$lengths[i]/.totalCountSample, .res$values[i],'hit') 
  }
  
  
  .Q <- filter.key.dt(.Return,'ko',0)
  
  #combine to per sequence, not per uproc hit
  .tmp = data.table(x = .Return[,seq.no], y = .Q)
  .filtered.out.ko <- .tmp[,sum(y),by=x][['V1']] > 0
  
  

  
  #cat('filtered out by ko:', sum(.Q),'\n',sep = '')
  .All.to.filter <- .All.to.filter | .Q
  
  .Q <- filter.multihit.dt(.Return, .nLines, 'seq.no')

  #combine to per sequence, not per uproc hit
  .tmp = data.table(x = .Return[,seq.no], y = .Q)
  .filtered.out.multihit <- .tmp[,sum(y),by=x][['V1']] > 0


  #cat('filtered out multihits:', sum(.Q),'\n',sep = '')
  .All.to.filter <- .All.to.filter | .Q
  
  #filter RNA out
  if (length(slot(Object.data.big,'SeqRNA')) != 0)
  {
  .Q <- filter.key.dt(.Return, 'seq.no', slot(Object.data.big,'SeqRNA')[[Sample]])
  
  #combine to per sequence, not per uproc hit
  .tmp = data.table(x = .Return[,seq.no], y = .Q)
  .filtered.out.rna <- .tmp[,sum(y),by=x][['V1']] > 0
  
  #cat('filtered out by RNA:', sum(.Q),'\n',sep = '')  
  .All.to.filter <- .All.to.filter | .Q
  }

  
  
  ##find unique filtered elements...
  .filtered.out.taxonomy
  .filtered.out.ko
  .filtered.out.multihit
  
  
  if (length(.filtered.out.rna) == 0)
  {
    .filtered.out.rna <- logical(length = length(.filtered.out.taxonomy))
  }
  
  .nfiltered.tax <- sum(.filtered.out.taxonomy & !.filtered.out.ko & !.filtered.out.multihit & !.filtered.out.rna)
  .nfiltered.ko <- sum(!.filtered.out.taxonomy & .filtered.out.ko & !.filtered.out.multihit & !.filtered.out.rna)
  .nfiltered.multihit <- sum(!.filtered.out.taxonomy & !.filtered.out.ko & .filtered.out.multihit & !.filtered.out.rna)
  .nfiltered.rna <- sum(!.filtered.out.taxonomy & !.filtered.out.ko & !.filtered.out.multihit & .filtered.out.rna)
  .nfiltered.multi <- sum(.All.to.filter) - .nfiltered.tax - .nfiltered.ko - .nfiltered.multihit - .nfiltered.rna
  
  Object.job.statistics <- appendInputdata(Object.job.statistics,'filtered.multi',.nfiltered.multihit)
  Object.job.statistics <- appendInputdata(Object.job.statistics,'filtered.rna',.nfiltered.rna)
  Object.job.statistics <- appendInputdata(Object.job.statistics,'filtered.tax',.nfiltered.tax)
  Object.job.statistics <- appendInputdata(Object.job.statistics,'filtered.ko',.nfiltered.ko)
  Object.job.statistics <- appendInputdata(Object.job.statistics,'filtered.combo',.nfiltered.multi)
  
    ##
  
  
  
  
  
  #cat('filter:',sum(.All.to.filter),'\n')
  
  saveRDS(.Return[!.All.to.filter,],.RDS.fileout)
  #saveRDS(.Return,.RDS.fileout)
  return(list(tmp.data.frame,Object.job.statistics))
}

perform.dataconstruction.rds <- function(Object.data.big, Object.job.path, Object.data.kegg, Object.job.statistics, Sample)
{
.file.in.rds <- slot(Object.job.path,'RDS')[Sample]  
.FULLDT <- readRDS(.file.in.rds)  
#remove scores below threshold
#get threshold
.thresh <- slot(Object.job.statistics,'FilteringScore')

#set statistics
Object.job.statistics <- appendInputdata(Object.job.statistics,'filtered.score',dim(.FULLDT[score <= .thresh])[1])

.FULLDT = .FULLDT[score > .thresh]

DF2 <- data.table(Sample = numeric(), ko = numeric(), TaxID = numeric(), Previous = numeric() )

  for (i in 1:TAXONOMY.LEVELS)
  {
    .PARTDT <- .FULLDT[y <= i-1]
    
    .xVals = .PARTDT[,x]+1
    
    .TaxID = Object.data.kegg@TaxMat[.xVals,i]
    .KO = .PARTDT[,ko]
    .Prev = Object.data.kegg@TaxMat[.xVals,i+1]
    if (is.null(.Prev))
    {
      DF <- data.table(Sample = rep(Sample,length(.KO)), ko = .KO, TaxID = .TaxID, Previous = rep(-1,length(.KO)) )
    }
    
    else
    {
      DF <- data.table(Sample = rep(Sample,length(.KO)), ko = .KO, TaxID = .TaxID, Previous = .Prev )  
    }
  DF2 <- rbindlist(list(DF2,DF))  
  }

DT <- DF2[,.N,by=c('TaxID','ko','Sample','Previous')]
setnames(DT,c('N'),c('Counts'))

DT2 <- slot(Object.data.big,'CountDT')
DT <- rbindlist(list(DT2,DT)) 

Object.data.big <- setInputdata(ObjectPart = Object.data.big, Type = 'CountDT', DT)

return(list(Object.job.statistics,Object.data.big))
}


perform.quickdatatable <- function(DT)
{
DT2 <- DT[,sum(Counts),by=c('Sample','TaxID','Previous')]
setnames(DT2,c('V1'),c('Counts'))
return(DT2)
}


perform.rna.statistics <- function(Object.job.statistics)
{
  
}


calc.FilteringScore <- function(Object.job.statistics)
{
  if (sum(slot(Object.job.statistics,'UProCHits')) > 0)
  {
  return(sum(slot(Object.job.statistics,'ScoreCutoff') * ( slot(Object.job.statistics,'UProCHits') / sum(slot(Object.job.statistics,'UProCHits')))))  
  }
  
  else
  {
    return(mean(slot(Object.job.statistics,'ScoreCutoff')))
    
  }
}

change.uprocscorethreshold <- function(Object.job.statistics,Object.data.dataframes) 
{
  .val <- calc.FilteringScore(Object.job.statistics = Object.job.statistics) 
  cat('suggested UProC filtering Score is :',.val,'\n')
  .loop = TRUE
  while(.loop)
  {
    .loop2 = TRUE
    while(.loop2)
    {
      .ret <- set.string(FALSE,'change it to:\n')
      .ret = type.convert(.ret, as.is = TRUE)
      if (class(.ret) != class(character()))
      {
        .ret = as.numeric(.ret)  
        if (length(.ret) == 1)
        {
          
          .loop2 = FALSE 
        }
        
        else
        {
          cat('only one entry allowed.\n')
        }
      }
      
      else
      {
        cat('please enter a numeric value\n')  
      }
    }
    slot(Object.job.statistics,'FilteringScore') <- .ret
    plot.uproc.scores(Object.job.statistics = Object.job.statistics,Object.data.dataframes = Object.data.dataframes) 
    .loop = !get.yesno(FALSE)
  }
  
  
return(list(Object.job.statistics))  
}
