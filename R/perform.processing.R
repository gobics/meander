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
  
  
  
  # if this does not work, approximate the read counts... ... ... .. .. .
  .Return2 <- try(fread(.uproc.filein, nrows=1, skip = dim(.Return)[1]))
  
              if (length(.Return2) == 1)
              {
                  if (class(.Return2) == "try-error")
                  {
                    cat("no stuff with stuff...\n")
                    #set #reads to highest seen read in results
                    Object.job.statistics <- appendInputdata(Object.job.statistics,'reads',max(.Return[,V1]))
                    #set #hits to #unique hits
                    Object.job.statistics <- appendInputdata(Object.job.statistics,'UProCHits',length(unique(.Return[,V1])))
                  }
                  
                  else
                  {
                  Object.job.statistics <- appendInputdata(Object.job.statistics,'UProCHits',.Return2[[1]])
                  Object.job.statistics <- appendInputdata(Object.job.statistics,'reads',.Return2[[3]])                    
                  }
              }

  else
  {
    Object.job.statistics <- appendInputdata(Object.job.statistics,'UProCHits',.Return2[[1]])
    Object.job.statistics <- appendInputdata(Object.job.statistics,'reads',.Return2[[3]])                    
  }
  
  
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

  .tmp = data.table(x = .Return[,seq.no], y = .All.to.filter)
  .filtered.out.multi <- .tmp[,sum(y),by=x][['V1']] > 0  
  
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
  .nfiltered.multi <- sum(.filtered.out.multi) - .nfiltered.tax - .nfiltered.ko - .nfiltered.multihit - .nfiltered.rna
  
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
Object.job.statistics <- appendInputdata(Object.job.statistics,'filtered.score',dim(.FULLDT[score < .thresh])[1])

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
    cat('Use selected Score?\n')
    .loop = !get.yesno(FALSE)
  }
  
  
return(list(Object.job.statistics))  
}



create.matrix <- function(Object.DATA.BIG,Object.Job.Config)
{
  #takes objects and puts out a matrix for use in PCA
  .DT <- slot(Object.DATA.BIG,'CountDT');
  .selectedTax <- slot(Object.Job.Config,'SelectedTax');
  .ClassVec <- slot(Object.Job.Config,'ClassVec');
  .SelectedClasses <- slot(Object.Job.Config,'SelectedClasses');
  
  .Allowed.Samples <- which(.ClassVec %in% .SelectedClasses)
  
  #reduce table accordingly
    if (.selectedTax == -1)
    {
      .DT <- .DT[Previous == .selectedTax]
    }
  
    else
    {
      .DT <- .DT[TaxID == .selectedTax]
    }

  .nMaxKO <- max(.DT[,ko])
  .nSamples = max(.DT[,Sample])
  
  XMat = matrix(data = 0, nrow = .nMaxKO, ncol = .nSamples)
  #XMat[.DT[,ko],.DT[,Sample]] = .DT[,V1]
    for (i in 1:.nSamples)
    {
      .red = .DT[Sample == i,]
      XMat[.red[,ko],i] = .red[,Counts]
    }
  #warning if too few counts...
  .I.Col = which(colSums(XMat) < 1000)
    if (length(.I.Col) != 0)
    {
      cat("WARNING!!!!!!",.I.Col,"\n")
    }
  return(list(XMat,.ClassVec[.Allowed.Samples]))
}

perform.consensusselecion <- function(Type = 'Consensus', O.Job.Config, O.DATA.Refined)
{
  .Methods = slot(O.Job.Config,'Methods')
  .nMethods = length(.Methods)
  
  print(.nMethods)
  
  .ConMat = slot(O.DATA.Refined,'ConsensusMat')
  
  .pThresh = slot(O.Job.Config,'pValThresh')
  
  
  .ConMat <- .ConMat <= .pThresh
  
  #type consensus
    if (Type == 'Consensus')
    {
      .Vec <- rowSums(.ConMat) >= (.nMethods/2)
    }
  #type one method
    else if (Type %in% .Methods)
    {
      .Vec <- .ConMat[,.Methods %in% Type]
    }
  #type all
    else if (Type == 'all')
    {
      .Vec <- rowSums(.ConMat) == .nMethods
    }
  
    else
    {
      #ERROR
      .Vec = FALSE
    }
  O.DATA.Refined <- setInputdata(O.DATA.Refined,'ConsensusVec', .Vec)
  
  return(O.DATA.Refined)
}




perform.pathwaydetection <- function(O.Job.Config,O.Data.Kegg,O.Data.Refined)
{
  
  #parts of object
  TaxID <- as.character(slot(O.Job.Config,'SelectedTax'))
  KEGG2Path <- slot(O.Data.Kegg,'KEGG2PATH')
  
  #select all possible KO for the species
  KO.Hits <- which(slot(O.Data.Refined,'ConsensusVec'))
  ALLKO.Hits <- slot(O.Data.Refined,'ALLKOabove')
    #NAME.getData(Object = Object, LEVEL1 = 'Results', LEVEL2 = 'Consensus',LEVEL3 = 'KOwithHits')
  
  PossibleKO = slot(O.Data.Kegg,'KOinTax')[[TaxID]]
  #PossibleKO = NAME.getData(Object = Object, LEVEL1 = 'Parameter', LEVEL2 = 'Output',LEVEL3 = 'KOinTax')[[TaxID]]
  
  ##FLAGS
  .PathMode = 'inTax'
  #.PathMode = 'all'
  
  #.PathKOMode = 'inTax'
  .PathKOMode = 'all'
  ##Settings
  #.Threshold <- NAME.getData(Object = Object, LEVEL1 = 'Parameter',LEVEL2 = 'R', LEVEL3 = 'SelectedThreshold')
  .Threshold = 0.05
  
  
  
  
  
  nKO = dim(KEGG2Path)[1]
  nPath <- dim(KEGG2Path)[2]
  
  cat(nKO,nPath,'\n')
  
  ReducedPossibleKO = PossibleKO[PossibleKO <= nKO]
  
  PathMat <- matrix(0,ncol=nPath,nrow=nKO)
  PathMat[ReducedPossibleKO,] = KEGG2Path[ReducedPossibleKO,]
  #Find allowed KO
  
  
  
  if (.PathMode == 'inTax')
  {
    .Vec <- sapply(1:nPath, function(x) which(PathMat[,x] == 1))
  }
  
  else if (.PathMode == 'all')
  {
    .Vec <- sapply(1:nPath, function(x) which(KEGG2Path[,x] == 1))
  }
  
  
  SigKOCountVec <- sapply(1:nPath, function(x) sum(.Vec[[x]] %in% KO.Hits))
  PossibleKOCountVec <- sapply(1:nPath, function(x) length(.Vec[[x]]))
  
  
  
  if (.PathKOMode == 'inTax')
  {
    .nPossKO <- length(ReducedPossibleKO)
  }
  
  else if (.PathKOMode == 'all')
  {
    .nPossKO <- sum(rowSums(KEGG2Path) > 0)
  }
  
  
  nSig = length(KO.Hits)
  cat(nSig,.nPossKO,'\n')
  #phyper(62-1, 1998, 5260-1998, 131, lower.tail=FALSE)
  .Vals <- unlist(lapply(c(1:nPath), function(x) phyper(SigKOCountVec[x]-1, PossibleKOCountVec[x], .nPossKO-PossibleKOCountVec[x], nSig, lower.tail = FALSE, log.p = FALSE)))
  
  .padjVals = p.adjust(.Vals, method = 'BH');
  .SigPaths <- which(.padjVals < .Threshold);
  
  
  .AllPaths <- colSums(KEGG2Path[ALLKO.Hits[ALLKO.Hits <= nKO],]);
  .AllPathsTax <- colSums(PathMat[ALLKO.Hits[ALLKO.Hits <= nKO],]);
  
  
  cat(.SigPaths,'\n')
  cat(.padjVals,'\n')
  
  return(list(.SigPaths,.padjVals,SigKOCountVec,PossibleKOCountVec))
}



process.kowithmincounts <- function(DT,O.Job.Config,nMin = 5)
{
#returns all ko in the selected samples whith rowsum > nMin
  Sample.Ind <- which(slot(O.Job.Config,'ClassVec') %in% slot(O.Job.Config,'SelectedClasses'))  
  
  XX <- DT[Previous == -1 & Sample %in% Sample.Ind ,sum(Counts),by = ko]
return(sort(XX[XX[,V1] <= nMin,ko]))
}
  


perform.plot.statistics <- function(O.Job.Statistic)
{
  df = data.frame(x = NULL, y = NULL, z = NULL, stringsAsFactors = FALSE)  
  if (length(slot(O.Job.Statistic,'RNA')) == 0)
  {
    .RNA <- vector(mode = 'numeric', length = length(slot(O.Job.Statistic,'ScoreCutoff')))  
  }
  
  else
  {
    .RNA <- slot(O.Job.Statistic,'RNA')
  }
  
  .filtered.score <- slot(O.Job.Statistic,'filtered.score')
  .filtered.combo <- slot(O.Job.Statistic,'filtered.combo')
  .filtered.ko <- slot(O.Job.Statistic,'filtered.ko')
  .filtered.tax <- slot(O.Job.Statistic,'filtered.tax')
  .filtered.rna <- slot(O.Job.Statistic,'filtered.rna')
  .filtered.multi <-slot(O.Job.Statistic,'filtered.multi')
  .reads <- slot(O.Job.Statistic,'reads')
  .UProCHits <- slot(O.Job.Statistic,'UProCHits')
  
  .reads <- .reads - .UProCHits
  
  .UProCHits <- .UProCHits - (.filtered.score+.filtered.combo+.filtered.multi+.filtered.rna+.filtered.tax+.filtered.ko)
  
  
  
  
  df <- rbind(df,data.frame(x = .reads, y = 1:length(.RNA), z = rep('no match',length(.RNA))))
  df <- rbind(df,data.frame(x = .UProCHits, y = 1:length(.RNA), z = rep('high QL match',length(.RNA))))
  df <- rbind(df,data.frame(x = (.filtered.score+.filtered.combo+.filtered.multi+.filtered.rna+.filtered.tax+.filtered.ko), y = 1:length(.RNA), z = rep('low QL match',length(.RNA))))
  
  
  
  
  
  #df <- rbind(df,data.frame(x = .reads, y = 1:length(.RNA), z = rep('reads',length(.RNA))))
  #df <- rbind(df,data.frame(x = .UProCHits, y = 1:length(.RNA), z = rep('UProCHits',length(.RNA))))
  #df <- rbind(df,data.frame(x = .filtered.score, y = 1:length(.RNA), z = rep('filtered.score',length(.RNA))))
  #df <- rbind(df,data.frame(x = .filtered.combo, y = 1:length(.RNA), z = rep('filtered.combo',length(.RNA))))
  #df <- rbind(df,data.frame(x = .filtered.ko, y = 1:length(.RNA), z = rep('filtered.ko',length(.RNA))))
  #df <- rbind(df,data.frame(x = .filtered.tax, y = 1:length(.RNA), z = rep('filtered.tax',length(.RNA))))
  #df <- rbind(df,data.frame(x = .filtered.rna, y = 1:length(.RNA), z = rep('filtered.rna',length(.RNA))))
  #df <- rbind(df,data.frame(x = .filtered.multi, y = 1:length(.RNA), z = rep('filtered.multi',length(.RNA))))
  
  
  #df <- rbind(df,data.frame(x = .RNA, y = 1:length(.RNA), z = rep('RNA',length(.RNA))))
  
  
  return(df)
}

prepare.svgvectors.colour <- function(O.data.refined, O.data.kegg, O.job.config)
{
  .Tax <- slot(O.job.config,'SelectedTax')
  .Sig.Ko <- slot(O.data.refined,'ConsensusVec')
  .AllowedKO <- slot(O.data.kegg,'KOinTax')[[as.character(.Tax)]]
  
  
  
  .Mat <- slot(O.data.refined,'Matrix')
  
  nMax.dim <- max(c(max(.AllowedKO),dim(.Mat)[1]))
  
  Vec.ratio <- vector(mode = 'numeric', length = nMax.dim) + 1
  Vec.Col = vector(mode = 'character', length = nMax.dim)
  
  #find rows with min counts
  .Mat.I <- rowSums(.Mat) > 0
  
  .Mat.Label <- slot(O.data.refined,'Matrix.label')
  U.label = unique(.Mat.Label)
  
  #spit the two conditions
  Cond.A <- .Mat.Label == U.label[1]
  Cond.B <- .Mat.Label == U.label[2]
  
  
  #create rank matrix
  .Mat.rank <- sapply(1:length(.Mat.Label), function(x) rank(.Mat[.Mat.I,x]))
  
  .Counts.A <- rowMeans(.Mat.rank[,Cond.A])
  .Counts.B <- rowMeans(.Mat.rank[,Cond.B])
  
  Vec.ratio[which(.Mat.I)] = .Counts.A / .Counts.B
  
  #All my KO
  KO.hit.all <- which(rowSums(.Mat) > 0)
  KO.hit.tax <- KO.hit.all[KO.hit.all %in% .AllowedKO]
  KO.hit.extra <- KO.hit.all[(KO.hit.all %in% .AllowedKO) == FALSE]
  KO.miss <- .AllowedKO[(.AllowedKO %in% KO.hit.all) == FALSE]
  
  KO.hit.sig <- which(.Sig.Ko)
  KO.hit.usig <- KO.hit.tax[(KO.hit.tax %in% KO.hit.sig) == FALSE]
  
  KO.greater <- which(Vec.ratio > 1)
  KO.smaller <- which(Vec.ratio < 1)
  
  cat(length(KO.hit.all),length(KO.hit.tax),length(KO.hit.extra),length(KO.miss),length(KO.hit.sig),length(KO.hit.usig),length(KO.greater),length(KO.smaller),'\n')

  
  Vec.Col[1:nMax.dim] <- NOT_MAPPED_AND_SHOULD_NOT_DEFAULT_COLOR
  Vec.Col[KO.hit.sig[KO.hit.sig %in% KO.smaller]] <- SIGNIFICANT_DOWN_REGULATED_DEFAULT_COLOR
  Vec.Col[KO.hit.sig[KO.hit.sig %in% KO.greater]] <- SIGNIFICANT_UP_REGULATED_DEFAULT_COLOR
  Vec.Col[KO.hit.usig[KO.hit.usig %in% KO.smaller]] <- INSIGNIFICANT_DOWN_REGULATED_DEFAULT_COLOR
  Vec.Col[KO.hit.usig[KO.hit.usig %in% KO.greater]] <- INSIGNIFICANT_UP_REGULATED_DEFAULT_COLOR
  Vec.Col[KO.hit.extra] <- MAPPED_AND_SHOULD_NOT_DEFAULT_COLOR
  Vec.Col[KO.miss] <- NOT_MAPPED_AND_SHOULD_DEFAULT_COLOR
  
  
  O.data.refined <- setInputdata(O.data.refined,'ColorVec',Vec.Col)
  return(O.data.refined)
}


perform.pvalcalc <- function(O.data.refined)
{
.pMat <- slot(O.data.refined,'ConsensusMat')
.p.Val <- sapply(1:dim(.pMat)[1], function(x) median(.pMat[x,]))
}
