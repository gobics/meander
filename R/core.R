
#Q <- meander.start(Dir.in = '/gobics/home/hklingen/Vortrag/EST/',Dir.out = '/c1')
meander.start <- function(
  #preselected files & types
  File.list = NULL, File.type = NULL,
  #preselected
  Selected.TaxID = NULL,
  #preselected
  UPRoCScore = NULL,
  FLAG.plot = TRUE,
  FLAG.tcltk = FALSE,
  #preselected dir
  Dir.in = NULL,
  Dir.out = NULL,
  #preselected
  AllFiles = FALSE,
  ObjectSave = 'Always',
  #
  Set.Conditions = NULL,
  Set.ConditionVec = NULL,
  Set.ConditionNames = NULL,
  Show.plots = TRUE
  )
{
## load general config & load objects
  Object.job.path <- .Object.Job.Paths()
  Object.job.config <- .Object.Job.Config()
  Object.job.statistics <- .Object.Job.Statistics()
  
  Object.data.big <- .Object.DATA.BIG()
  Object.data.kegg <- .Object.DATA.KEGG()
  Object.data.refined <- .Object.DATA.Refined()
  Object.data.dataframes <- .Object.DATA.dataframes()
  ##load fixed data
  #tax Mat
  Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'TaxMat',value = readRDS('/home/hklingen/projects/test123/data/TaxMat.rds'))
  #ko2path
  #kointax
  #...
  #png
    ##
  
  ##
  Object.Final <- Object();
  
  DEBUG.PRINT <- TRUE
  
  

## Where to start?
  # preselected parameters?

    #set availibe methods
    attemptExecution(Object.job.config <- set.methods(Object.job.config))
  
  #set input dir if missing
  if( is.null(Dir.in) & is.null(File.list))
  {
  Dir.in = select.dir(FLAG.tcltk = FALSE,FLAG.create = FALSE ,Message = 'please select input folder :\n')
  }

  #set output dir if missing
  if( is.null(Dir.out) )
  {
  Dir.out = select.dir(FLAG.tcltk = FALSE,FLAG.create = TRUE ,Message = 'please select output folder :\n')
  }

  
  
  #create all subfolders for dir
  create.directory(Dir.out,c('UPROC','RDS','HTML','OBJECT'))
  
  
  
  # get filelist from Dir.in

  # put data in
    #select files

      if( is.null(File.list) )
      {
      .inFiles = dir(path = Dir.in)
      
        if (AllFiles)
        {
         File.list <- .inFiles
        }
        
        else
        {
          File.list <- select.multiple.List(FLAG = FALSE, Choices = .inFiles) 
        }
        
      }

      if( is.null(File.type) )
      {
        .inList = c(FILETYPE.DNA,FILETYPE.DNAwoRNA,FILETYPE.UproC)
        File.type <- select.List(FLAG = FALSE, Choices = .inList)
      }
  
  
    #combine path and filenames
    .full.filepaths = file.path(Dir.in,File.list)
    #check if files are what is claimed
    .res <- check.input(File.type,NULL,.full.filepaths)
    
    
    #add to objectpart
    Object.job.path <- setInputdata(Object.job.path,File.type,.full.filepaths)
    Object.job.path <- setInputdata(Object.job.path,'DirOut',Dir.out)

  
    
    #create paths
    
  ##
  
  ##check if error.
    if (DEBUG.PRINT) {  cat('checking for error : \t') }

    if (DEBUG.PRINT) {  cat('OK\n') }
  
  ## set conditions
  Object.job.config <- set.Conditions(List = File.list, Object.job.config = Object.job.config, FALSE)
  
    ##
  Object.job.config <- set.selected.Conditions(FALSE,Object.job.config)
  ## select conditions
  
  
    ##
  
  ## save Object for the first time.

  
  #STEP1
  if (File.type %in% INPUTDEPENDENTSTEPS.LIST.ONE)
  {
    cat('STEP1\n')
  .ret <- start.DNA(Object.job.path = Object.job.path, Object.data.big = Object.data.big, Object.job.statistics = Object.job.statistics, object.save.FLAG = FALSE)
  Object.job.path <- .ret[[1]]
  Object.data.big <- .ret[[2]]
  Object.job.statistics <- .ret[[3]]
  print(.ret[[4]])
  }
  
  #STEP2
  if (File.type %in% INPUTDEPENDENTSTEPS.LIST.TWO)
  {
    cat('STEP2\n')
  .ret <- start.DNAnoRNA(Object.job.path = Object.job.path,TRUE)
  Object.job.path <- .ret[[1]]
  print(.ret[[2]])
  }
  
  

  
    #STEP3
  if (File.type %in% INPUTDEPENDENTSTEPS.LIST.THREE)
  {
    cat('STEP3\n')
    .ret <- start.UProC(Object.job.path,Object.job.statistics,Object.data.big,Object.data.dataframes)
    Object.job.path = .ret[[1]]
    Object.job.statistics = .ret[[2]]
    Object.data.big = .ret[[3]]
    Object.data.dataframes = .ret[[4]]
  }
  
  

          if (Show.plots)
        {
            plot.uproc.scores(Object.job.statistics = Object.job.statistics,Object.data.dataframes = Object.data.dataframes) 
          
        }
        
        #set score threshold
        if (!is.null(UPRoCScore))
        {
        
        }  
  
  usesuggestedscore = FALSE

  if (!usesuggestedscore)
  {
    .ret <- change.uprocscorethreshold(Object.job.statistics,Object.data.dataframes) 
    Object.job.statistics <- .ret[[1]]
  }
  
  
  
  
  .ret <- start.RDS(Object.data.big = Object.data.big, Object.job.path = Object.job.path, Object.data.kegg = Object.data.kegg, Object.job.statistics = Object.job.statistics, Object.data.refined =  Object.data.refined, object.save.FLAG = FALSE)
  Object.data.big <- .ret[[2]]
  Object.job.statistics <- .ret[[1]]
  Object.data.refined <- .ret[[3]]  


  ## LOOP
  #select taxonomy
  Object.job.config <- setInputdata(Object.job.config,'SelectedTax',33090)
  
    #create Matrix
  .Mat <- create.matrix(Object.DATA.BIG = Object.data.big,Object.Job.Config = Object.job.config)
  Object.data.big <- setInputdata(Object.data.big,'Matrix',.Mat)
    #perform pca
  plot.pca(Object.Job.Config = Object.job.config,Object.Job.Statistics = Object.job.statistics,Object.Data.Big = Object.data.big)
    
    ##
  
  
  #create final matrix
  
  .Mat <- create.matrix(Object.DATA.BIG = Object.data.big,Object.Job.Config = Object.job.config)
  Object.data.big <- setInputdata(Object.data.big,'Matrix',.Mat)
  
  
  #consensus methods
  .X <- start.consensus(Object.data.big, Object.job.config)
  Object.data.refined <- setInputdata(Object.data.refined,'ConsensusMat',.X)
  
  
  Object.job.config <- setInputdata(Object.job.config,'SelectedTax',33090)
  
  
  #'select' consensus
  
###build complete object
  slot(slot(Object.Final,'Job'),'Paths') = Object.job.path
  slot(slot(Object.Final,'Job'),'Config') = Object.job.config
  slot(slot(Object.Final,'Job'),'Statistics') = Object.job.statistics
  
  slot(slot(Object.Final,'DATA'),'BIG') = Object.data.big
  slot(slot(Object.Final,'DATA'),'KEGG') = Object.data.kegg
  slot(slot(Object.Final,'DATA'),'Refined') = Object.data.refined
  slot(slot(Object.Final,'DATA'),'DataFrames') =  Object.data.dataframes
  ###
  
  
  
return(Object.Final)  
}


meander.resume <- function()
{

}