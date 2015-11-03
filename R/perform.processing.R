#Z <- .process.uproc('/home/hklingen/workspace/uproc-taxy/rodica/results/Forst_01.uproc','/home/hklingen/projects/TMP/qq3.rds', TRUE, FALSE, c(1:200000))
.process.uproc <- function(uproc.filein, rds.fileout, writeoutput.FLAG, keepinput.FLAG, rna.filter, tmp.max)
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
  .Return <- .Return[I.All,]
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
    file.remove('/home/hklingen/projects/TMP/qq2.rds')
    }

    if (max(.Return[,score]) > tmp.max)
    {
    .Y = hist(.Return[,score],25,plot = FALSE)
    return(.Y$breaks)
    }

  return(TRUE)
}
