meander.start <- function(
  #preselected files & types
  File.list = NULL, File.type = NULL,
  #preselected
  TaxID = NULL,
  #preselected
  UPRoCScore = NULL,
  FLAG.plot = TRUE,
  FLAG.tcltk = FALSE,
  #preselected dir
  Dir.in = NULL,
  Dir.out = NULL
  )
{
## load general config & load objects
  job.Object <- .ObjectPaths()
  ##

# TODO put into Object
.inFiles = c("DNA","RNAfilteredDNA","UproC","RDS")

## Where to start?
  # preselected parameters?

  #set input dir if missing
  if( is.null(Dir.in) & is.null(File.list))
  {
  Dir.in = .select.dir(FLAG.tcltk = FALSE,FLAG.create = FALSE ,Message = 'please select input folder :\n')
  }

  #set output dir if missing
  if( is.null(Dir.out) )
  {
  Dir.out = .select.dir(FLAG.tcltk = FALSE,FLAG.create = TRUE ,Message = 'please select output folder :\n')
  }

  # get filelist from Dir.in

  # put data in
    #select files

      if( is.null(File.list) )
      {
      .inFiles = dir(path = Dir.in)
        File.list <- .select.List(FLAG = FALSE, Choices = .inFiles)
      }

      if( is.null(File.type) )
      {
        .inList = c(
          'DNAin',
        'DNAwithoutRNAin',
        'UPRoCin',
        'RDSin')
        File.type <- .select.List(FLAG = FALSE, Choices = .inList)
      }
    #add to objectpart
    job.Object2 <- .setInputdata(job.Object,File.type,File.list)
  print(job.Object2)
  ##
}


meander.resume <- function()
{

}
