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


process.storeRDS <- function(Obj,fileout)
{
  saveRDS(object = Obj, file = fileout)
}

filter.key.dt <- function(DT, key, Filter)
{
  #returns logical Vector of all hits matching the Filter
  .I <- DT[j = key, with = FALSE][[1]] %in% Filter
  return(.I)
}

filter.multihit.dt <- function(DT,.nRows, key.one, key.two)
{
  # count number of hits per sequence
  .I.singlehit <- vector(mode='logical', length = .nRows)
  .HitCountVec <- DT[,length(key.one), by=key.two][['V1']]
  .Pos <- cumsum(.HitCountVec[[2]]);
  .Pos <- .Pos[.HitCountVec[[2]] != 1]
  
  # set single hits == TRUE
  .I.singlehit[.Pos] <- T  
  return(.I.singlehit)
}

filter.uprocresult <- function( ko.key, KO.Filter)
{
  #DT data.table with UProC results
  
  
  #get dimensions
  .tmp <- dim(DT)
  .nRows <- .tmp[1]
  .nCol <- .tmp[2]
  
  

  

  
  
  
  
}

process.storeRDS <- function(rds.fileout,uproc.filein,tmp.data.frame,Sample)
{
  .Return <- fread(uproc.filein, nrows=-1, select = c(1,3,8,9,10,11))
  setnames(.Return,c('V1','V3','V8','V9','V10','V11'),c('seq.no','length','ko','score','x','y'))
  .Return[,ko := as.integer(substr(.Return[,ko],2,100))]
  
  #get length of data.table
  .nLines <- length(.Return[,x])
  
  .Q <- filter.key.dt(.Return,'x',10010101)
  cat('filtered out by taxonomy:', sum(.Q),'\n',sep = '')
  
  .res <- rle(sort(round(.Return[, j = score, with = TRUE]*10)/10))
  
  .totalCountSample = sum(.res$lengths)
  
  
  .res <- rle(sort(round(.Return[i = .Q, j = score, with = TRUE]*10)/10))
  cat('result:',.res$lengths,'\n')
  
  
  .nframe.lines = dim(tmp.data.frame)[1]
  .nRLElines = length(.res$lengths)
  print(.nframe.lines)
  
  .nLen = sum()
  
  for (i in 1:.nRLElines)
  {
   tmp.data.frame[(i+.nframe.lines),] <- list(Sample,(.res$lengths[i]/.totalCountSample), .res$values[i],'miss') 
  }
  
  


  
  .res <- rle(sort(round(.Return[i = !.Q, j = score, with = TRUE]*10)/10))
  cat('result:',.res$lengths,'\n')
  
    .nframe.lines = dim(tmp.data.frame)[1]
  .nRLElines = length(.res$lengths)
  print(.nframe.lines)
  
  for (i in 1:.nRLElines)
  {
    tmp.data.frame[(i+.nframe.lines),] <- list(Sample,.res$lengths[i]/.totalCountSample, .res$values[i],'hit') 
  }
  
  
  .Q <- filter.key.dt(.Return,'ko',0)
  cat('filtered out by ko:', sum(.Q),'\n',sep = '')
  
  
  .Q <- filter.multihit.dt(.Return, .nLines, 'ko', 'seq.no')
  cat('filtered out multihits:', sum(.Q),'\n',sep = '')
  return(tmp.data.frame)
}



process.uproc.scores <- function(uproc.filein,tmpmax)
{
  .Return <- fread(uproc.filein, nrows=-1, select = c(1,3,8,9,10,11))
  setnames(.Return,c('V1','V3','V8','V9','V10','V11'),c('seq.no','length','ko','score','x','y'))
  
  
  
  
  #get length of data.table
  .nLines <- length(.Return[,x])
  
  ##TODO
  #check if there are too few results...
    ##
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
  
  .nKO = sum(!.I.KO )
  .nMulti = sum(!.I.singlehit)
  .nTax = sum(!.I.Tax)
  
  .nAll = sum(.I.All)
  cat(.nKO,'\t',.nMulti,'\t',.nTax,'\t',.nAll,'\n')
  Cutoff <- mean(.Return[!.I.Tax,score]) + (0.5*sd(.Return[!.I.Tax,score]))
  
  #reduce data.table by removing multiple hits and unknown taxonomy
  .Return <- .Return[.I.All,]
    if (tmpmax < max(.Return[,score]))
    {
      .Y = hist(.Return[,score],25,plot = FALSE)
      return(list(TRUE,Cutoff,.Y$breaks))
    }
  return(list(FALSE,Cutoff,NULL))
}