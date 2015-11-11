#
start.DNA <- function(Object.job.path,Object.data.big,object.save.FLAG)
{
  #get all files
  .Allfiles <- slot(Object.job.path,FILETYPE.DNA)
  .nAllFiles <- length(.Allfiles)
  
  .res <- check.input(FILETYPE.DNA,NULL,.Allfiles)
  #error handling
  .res@handle()
  
  #set DNA to DNAwithoutRNA
  
  #write DNA to DNAnoRNA...
  cat("write DNA to DNAnoRNA","\n")
  
Object.job.path <- setInputdata(Object.job.path,FILETYPE.DNAwoRNA,.Allfiles)
  
  for (i in 1:.nAllFiles)
  {
    #remove RNA
    cat("remove RNA","\n")
    
    .ret <- .dummy.RNA(.Allfiles[i])
    print(.ret)
    cat("save Vec","\n") 
    Object.data.big <- appendInputdata(Object.data.big,'SeqRNA',list(.ret))
  }    
  
  #rest
  return(list(Object.job.path,Object.data.big,'start.DNAnoRNA'))
}


start.DNAnoRNA <- function(Object.job.path,Object.data.big,object.save.FLAG)
{
  #start uproC
  cat("start uproC","\n")
  .Allfiles <- slot(Object.job.path,FILETYPE.DNAwoRNA)
  .nAllFiles <- length(.Allfiles)
  
  .UPROCbin = '/home/hklingen/workspace/uproc-1.1.2_sl/uproc-dna '
  .UPROCmode = '-f -s -p -o '
  .UProCmodel = '/home/hklingen/DB/PFAM/Comet/model/model '
  .UProCDB = ' /scratch/KEGG_2014-08_full_uproc_2 '
  
  #data.frame to store plot
  .Z <- data.frame(Sample = numeric(), length = numeric(), values= numeric(), type = character(), stringsAsFactors = FALSE)
  #test files again
  for (i in 1:.nAllFiles)
  {
    
    #get basename, put a .uproc behind and put the dirout path to it...
    .file.out = file.path(slot(Object.job.path,'DirOut'),'UPROC',paste0(basename(.Allfiles[i]),'.upoc'))
    .file.out.RDS = file.path(slot(Object.job.path,'DirOut'),'RDS',paste0(basename(.Allfiles[i]),'.rds'))
    .systemcommand = paste0(.UPROCbin, .UPROCmode, .file.out, .UProCDB, .UProCmodel, .Allfiles[i],  ' 2>&1' )

    cat("systemcommand: ",.systemcommand,"\n")
    .ret = system(.systemcommand, intern = TRUE)
    attemptExecution(test.someuproc.error(.ret))
    cat('Ret:\n',.ret,'\n')
    
    #process.uproc.scores(.file.out,0)
    Object.job.path <- appendInputdata(Object.job.path,FILETYPE.UproC,.file.out)
    Object.job.path <- appendInputdata(Object.job.path,FILETYPE.RDS,.file.out.RDS)
    
  .Z <- process.storeRDS(Object.data.big, Object.job.path, .Z,i)
  }
  
data.blame <- data.frame(x = NULL, y = NULL, z = NULL)
  for (i in 1:.nAllFiles)
  {
    x.sub <- subset(.Z, Sample == i)
    
    ntotalSample = sum(x.sub$length)
    
    x.hit <- subset(x.sub, type == 'hit')
    x.miss <-subset(x.sub, type == 'miss')
    ##smooth
    #lines(lowess(x.hit$length~x.hit$values,f = .25), col = ColVec[i])
    #lines(lowess(x.miss$length~x.miss$values,f = .25), col = ColVec[i])
    #raw
    #lines(x.hit$values,x.hit$length/ntotalSample, col = ColVec[i])
    #lines(x.miss$values,x.miss$length/ntotalSample, col = ColVec[i])
    ##hist
    h.hit <- hist(rep(x.hit$values, x.hit$length/min(x.hit$length)),breaks = Break.Vec, plot = FALSE)
    h.miss <- hist(rep(x.miss$values, x.miss$length/min(x.miss$length)), breaks = Break.Vec, plot = FALSE)
    .val <- (sd(rep(x.miss$values,x.miss$length/min(x.miss$length)))*0.5) + sum(x.miss$values * x.miss$length)/sum(x.miss$length)
    
    #.val <- (sd(rep(x.miss$values, x.miss$length))*0.5) + mean(rep(x.miss$values, x.miss$length))
    if (i == 2)
    {
      #abline(v = .val,col = 'red')
    }
    x = rep(i,length(h.hit$counts))
    y = h.hit$mids
    z = h.hit$counts/sum(h.hit$counts)
    data.blame <- rbind(data.blame, data.frame(x = x, y = y, z = z))
  }
x11()
p <- ggplot(data.blame, aes(x=x,y=y))
print(p + geom_tile(aes(fill=z)) + scale_fill_gradient(low="#eafeef", high="#7ccd7c"))
x11()
p <- ggplot(data = data.blame, aes(x=y,y=z, group = x))
print(p + geom_line(aes(colour = x)))

  #rest
return(list(Object.job.path,Object.data.big,'start_uproc'))
}

start.UProC <- function(ObjectUProCModus)
{
  #read Uproc
  cat("read Uproc","\n")
  #rest
  start.RDS()
}

start.RDS <- function(ObjectPaths)
{
  
  .AllFiles <- slot(ObjectPaths,FILETYPE.RDS)
  .nAllFiles <- length(.AllFiles)
  for (i in 1:.nAllFiles)
  {
    #read RDS  
    cat("read RDS","\n")
    #store concentrated in Object
    cat("store concentrated in Object","\n")
    #store Object?  
    cat("store Object? ","\n")
  }
  start.Object()
  #rest
}

start.Object <- function()
{
  #taxonomy selection
  cat("taxonomy selection","\n")
  #PCA
  cat("PCA","\n")
  #run methods
  cat("run methods","\n")
  #venn
  cat("venn ","\n")
  #pathway analysis
  cat("pathway analysis ","\n")
  #output
  cat("output ","\n")
}