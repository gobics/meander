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

.ok.fun <- function()
{
  cat("all is ok.\n")  
}

.bad.fun <- function()
{
  cat("all is bad.\n") 
  stop('rip')
}


OK <- setClass	(
  #Name
  "OK",
  #Sots
  slots =	c(
  handle = "function",
  message = 'character',
  description = 'character'
  ),
  prototype = list(
    handle = .ok.fun,
    message = 'OK',
    description = 'all good.'
  )
)

BAD <- setClass (
  #NAME
  "BAD",
  slots = c(
    handle = "function",
    message = 'character',
    description = 'character'
  ),
  prototype = list(
    handle = .bad.fun,
    message = 'OK',
    description = 'all good.'
  )
)


.dummy.RNA <- function(filepath)
{
  con <- file(filepath, "r", blocking = FALSE)
  linn=readLines(con)
  close(con)
  .nSeq <- sum(substr(linn,1,1) == '>')
  .fRNA = 0.05
  .RNA <- sample(.nSeq,as.integer((.nSeq*.fRNA)))
  return(.RNA)
}



test.someuproc.error <- function(returned.system.string)
{
  if (!is.null(attr(returned.system.string,'status')))
  {
    FATAL_ERROR$new('I WILL FIND YOU AND I WILL KILL YOU')$throw()
  }
}


test.func <- function(system.callboy)
{
  #QQ <- system('/home/hklingen/workspace/uproc-1.1.2_sl/uproc-dna -f -s -p -o /scratch/transcriptome/small/out/UPROC/Forst_12_R1.fasta.upoc /scratch/KEGG_2014-08_full_uproc_2 /home/hklingen/DB/PFAM/Comet/model/model /scratch/transcriptome/small/Forst_12_R1.fasta 2>&1',intern = TRUE)
  QQ <- system(system.callboy, intern = TRUE)
  if (!is.null(attr(QQ,'status')))
  {
    FATAL_ERROR$new('I WILL FIND YOU AND I WILL KILL YOU')$throw()
  }
  print(QQ)
}