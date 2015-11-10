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
  start.DNAnoRNA(Object.job.path,Object.data.big,TRUE)  
  return(list(Object.job.path,Object.data.big,'start.DNAnoRNA'))
}


start.DNAnoRNA <- function(ObjectPaths,ObjectData,object.save.FLAG)
{
  #start uproC
  cat("start uproC","\n")
  .Allfiles <- slot(ObjectPaths,FILETYPE.DNAwoRNA)
  .nAllFiles <- length(.Allfiles)
  
  .UPROCbin = '/home/hklingen/workspace/uproc-1.1.2_sl/uproc-dna '
  .UPROCmode = '-s -p -o '
  .UProCmodel = '/home/hklingen/DB/PFAM/Comet/model/model '
  .UProCDB = ' /scratch/KEGG_2014-08_full_uproc_2 '
  
  #test files again
  for (i in 1:.nAllFiles)
  {
    
    #get basename, put a .uproc behind and put the dirout path to it...
    .file.out = file.path(slot(ObjectPaths,'DirOut'),'UPROC',paste0(basename(.Allfiles[i]),'.upoc'))
    .systemcommand = paste0(.UPROCbin, .UPROCmode, .file.out, .UProCDB, .UProCmodel, .Allfiles[i] )

    cat("systemcommand: ",.systemcommand,"\n")
    .ret = system(.systemcommand, intern = TRUE)
    cat('Ret:\n',.ret,'\n')
    
    process.uproc.scores(.file.out)
  }
  #rest
  start.UProC()
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