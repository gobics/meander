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
  ALLKO.Hits <- which(rowSums(slot(O.Data.Refined,'Matrix')) > 5)
    #NAME.getData(Object = Object, LEVEL1 = 'Results', LEVEL2 = 'Consensus',LEVEL3 = 'KOwithHits')
  
  PossibleKO = slot(O.Data.Kegg,'KOinTax')[[TaxID]]
  #PossibleKO = NAME.getData(Object = Object, LEVEL1 = 'Parameter', LEVEL2 = 'Output',LEVEL3 = 'KOinTax')[[TaxID]]
  
  
  PossibleBRselection <- which(rowSums(slot(slot(O.Data.Kegg,'ko2br.pathway'),'Matrix')[,slot(O.Job.Config,'SelectedBR')]) > 0)
  
  ##FLAGS
  .PathMode = 'inTax'
  #.PathMode = 'all'
  
  .PathKOMode = 'inTax'
  #.PathKOMode = 'all'
  ##Settings
  #.Threshold <- NAME.getData(Object = Object, LEVEL1 = 'Parameter',LEVEL2 = 'R', LEVEL3 = 'SelectedThreshold')
  .Threshold = 0.05
  
  nKO = dim(KEGG2Path)[1]
  nPath <- dim(KEGG2Path)[2]
  
  cat(nKO,nPath,'\n')
  
  ReducedPossibleKO = PossibleKO[PossibleKO <= nKO]
  
  ReducedPossibleKO = ReducedPossibleKO[ReducedPossibleKO %in% PossibleBRselection]
  
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
    KO.Hits <- KO.Hits[KO.Hits %in% ReducedPossibleKO]
  }
  
  else if (.PathKOMode == 'all')
  {
    .nPossKO <- sum(rowSums(KEGG2Path) > 0)
  }
  
  
  nSig = length(KO.Hits)
  cat(nSig,.nPossKO,'\n')
  #phyper(62-1, 1998, 5260-1998, 131, lower.tail=FALSE)
  .Vals <- unlist(lapply(c(1:nPath), function(x) {
    phyper(SigKOCountVec[x]-1, PossibleKOCountVec[x], .nPossKO-PossibleKOCountVec[x], nSig, lower.tail = FALSE, log.p = FALSE)}))
  
  .padjVals = p.adjust(.Vals, method = 'BH');
  .SigPaths <- which(.padjVals < .Threshold);
  
  
  .AllPaths <- colSums(KEGG2Path[ALLKO.Hits[ALLKO.Hits <= nKO],]);
  .AllPathsTax <- colSums(PathMat[ALLKO.Hits[ALLKO.Hits <= nKO],]);
  
  
  #cat(ALLKO.Hits,'\n')
  
  #cat(.SigPaths,'\n')
  #cat(.padjVals,'\n')
  .Vec <- sapply(1:nPath, function(x) which(KEGG2Path[,x] == 1))
  .A <- sapply(1:nPath, function(x) length(.Vec[[x]]))
  
  print(.Vec[[1]])
  
  
  .Vec <- sapply(1:nPath, function(x) which(PathMat[,x] == 1))
  .B <- sapply(1:nPath, function(x) length(.Vec[[x]]))
   
  return(list(.SigPaths,.padjVals,SigKOCountVec,.AllPathsTax,.A,.B))
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
  Vec.Flag = vector(mode = 'integer', length = nMax.dim);
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
  

  
  #should map
  Vec.Flag[.AllowedKO] <- set.Flag(Vec.Flag[.AllowedKO],SHOULD_MAP_FLAG)
  #maps
  KO.hit.all <- which(rowSums(.Mat) > 0)
  Vec.Flag[KO.hit.all] <- set.Flag(Vec.Flag[KO.hit.all],MAPPED_FLAG)
  #up
  Vec.Flag[Vec.ratio > 1] <- set.Flag(Vec.Flag[Vec.ratio > 1],UP_REGULATED_FLAG)
  #sig
  Vec.Flag[.Sig.Ko] <- set.Flag(Vec.Flag[.Sig.Ko],SIGNIFICANT_FLAG)
  
  
  #set colors
  Vec.Col <- ORTHOLOG_COLORS[Vec.Flag+COLOR_OFFSET];
  
  
  
  O.data.refined <- setInputdata(O.data.refined,'ColorVec',Vec.Col)
  O.data.refined <- setInputdata(O.data.refined,'FlagVec',Vec.Flag)
  return(O.data.refined)
}


perform.pvalcalc <- function(O.data.refined)
{
.pMat <- slot(O.data.refined,'ConsensusMat')
.p.Val <- sapply(1:dim(.pMat)[1], function(x) median(.pMat[x,]))
return(.p.Val)
}


calculate.vennreplacement <- function(Method.Vec = c('SAMseq','DESeq2','edgeR'), Mat.pVal, threshold = 0.05)
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
  return(list(df,df2,nLimz,LetterTable,smallGraph))
}


plot.generate.vennreplacement <- function(Method.Vec = c('SAMseq','DESeq2','edgeR'), Mat.pVal = Mat.pVal,threshold = 0.05)
{
  
  ret <- calculate.vennreplacement(Method.Vec = Method.Vec, Mat.pVal = Mat.pVal, threshold = threshold)
  df <- ret[[1]]
  df2 <- ret[[2]]
  nLimz <- ret[[3]]
  LetterTable <- ret[[4]]
  smallGraph <- ret[[5]]
  
  nMethods = length(Method.Vec)
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
  return(list(gC,gB,gD,gA, cbind(c(1,1,3),c(2,2,4),c(2,2,4),c(2,2,4))))
}