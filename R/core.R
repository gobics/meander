
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
  Set.ConditionNames = NULL
  )
{
## load general config & load objects
  Object.job.path <- .Object.Job.Paths()
  Object.job.config <- .Object.Job.Config()
  Object.data.big <- .Object.DATA.BIG()
  
  Object.data.kegg <- .Object.DATA.KEGG()
  Object.data.refined <- .Object.DATA.Refined()
  Object.job.statistics <- .Object.Job.Statistics()
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
  
# TODO put into Object
  set.globals()

## Where to start?
  # preselected parameters?

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
        .inList = c(FILETYPE.DNA,FILETYPE.DNAwoRNA,FILETYPE.UproC,FILETYPE.RDS)
        File.type <- select.List(FLAG = FALSE, Choices = .inList)
      }
  
  
    #combine path and filenames
    .full.filepaths = file.path(Dir.in,File.list)
    #check if files are what is claimed
    .res <- check.input(File.type,NULL,.full.filepaths)
    .res@handle()
    
    
    
    
    #add to objectpart
    Object.job.path <- setInputdata(Object.job.path,File.type,.full.filepaths)
    Object.job.path <- setInputdata(Object.job.path,'DirOut',Dir.out)

  
    
    #create paths
    
  ##
  
  ##check if error.
    if (DEBUG.PRINT) {  cat('checking for error : \t') }
  .res@handle()
    if (DEBUG.PRINT) {  cat('OK\n') }
  
  ##set next function
  
  
  
  ## set conditions
  
  
  
  ## save Object for the first time.
  
  #ToDo: create shitty method for each...
  if (File.type == FILETYPE.DNA)
  {
    #STEP1
  .ret <- start.DNA(Object.job.path = Object.job.path, Object.data.big = Object.data.big, Object.job.statistics = Object.job.statistics, object.save.FLAG = FALSE)
  Object.job.path <- .ret[[1]]
  Object.data.big <- .ret[[2]]
  Object.job.statistics <- .ret[[3]]
  print(.ret[[4]])
    #STEP2
  .ret <- start.DNAnoRNA(Object.job.path,Object.data.big, Object.job.statistics = Object.job.statistics,TRUE)
  Object.job.path <- .ret[[1]]
  Object.data.big <- .ret[[2]]
  Object.job.statistics <- .ret[[3]]
  print(.ret[[4]])
    #STEP3
  .ret <- start.RDS(Object.data.big = Object.data.big, Object.job.path = Object.job.path, Object.data.kegg = Object.data.kegg, Object.job.statistics = Object.job.statistics, Object.data.refined =  Object.data.refined, object.save.FLAG = FALSE)
  Object.data.big <- .ret[[2]]
  Object.job.statistics <- .ret[[1]]
  Object.data.refined <- .ret[[3]]
  }

  if (File.type == FILETYPE.DNAwoRNA)
  {
    .ret <- start.DNAnoRNA(Object.job.path,Object.data.big,TRUE)
    Object.job.path <- .ret[[1]]
    Object.data.big <- .ret[[2]]
    print(.ret[[3]])
  }
  
  if (File.type == FILETYPE.UproC)
  {
    .ret <- start.UProC(NULL)
  }
  
  if (File.type == FILETYPE.RDS)
  {
    .ret <- start.RDS(Object.job.path)
  }
  
  #set selected conditions
  
  #.ret <- set.selected.Conditions(FALSE,slot())
  
  #.ret <- start.Object()
  
  
  
  
  
  
###build complete object
  slot(slot(Object.Final,'Job'),'Paths') = Object.job.path
  slot(slot(Object.Final,'Job'),'Config') = Object.job.config
  slot(slot(Object.Final,'Job'),'Statistics') = Object.job.statistics
  slot(slot(Object.Final,'DATA'),'BIG') = Object.data.big
  slot(slot(Object.Final,'DATA'),'KEGG') = Object.data.kegg
  slot(slot(Object.Final,'DATA'),'Refined') = Object.data.refined
  ###
  
  
  
return(Object.Final)  
}


meander.resume <- function()
{

}