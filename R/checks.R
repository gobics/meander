check.DNA <- function(filepath)
{
  con <- file(filepath, "r", blocking = FALSE)
  linn=readLines(con,1)
    if (substr(linn,1,1) != '>')
    {
    close(con = con)
    return(BAD())
    }
  linn=readLines(con,1)
  .nNucl = nchar(linn)
  #count nucleotide occurances
  .Q <- gregexpr("[a,c,t,g,u,A,C,T,G,U]", linn)
  .nMatches = sum(attributes(.Q[[1]])$match.length)
  close(con = con)
  
    if ((.nNucl/.nMatches) < 0.8)
    {
    return(BAD())
    }
  
return(OK())
}

check.RDS <- function(filepath)
{
  .x <- try(readRDS(filepath))
  if ((class(.x)[1] != "try-error"))
  {
  return(OK())
  }
return(BAD())
}

check.uproc <- function(filepath)
{
  if (file.exists(filepath))
  {
    return(OK())
  }
return(BAD())  
}


check.package <- function(test.package)
{
  #check if package is availible
  if(test.package %in% rownames(installed.packages()))
  {
    #load package
    return(library(test.package,character.only = TRUE, logical.return = TRUE))
  }
  return(FALSE)
}



check_RLSQ <- function()
{
  #check if libary is loaded
  #is.loaded("fragmentlength", "test123", "")
  dyn.load("baz.so")
  if (sample(100,1) > 50)
  {
    return(OK())
  }
  return(BAD())
}

# check.uprocversion('/home/hklingen/Tool/uproc-1.1.2_sl/uproc-dna -v')
check.uprocversion <- function(Command)
{
  #checks if the version of UProc is the correct one
  #works on linux and windows
  .Q <- system(Command,intern=TRUE)
  if (.Q[2] == "uproc tax")
  {
    return(TRUE)
  }
  return(FALSE)
}

check.input <- function(SelectedType,PartObject,filepaths)
{
  #takes the selected files "filepaths" and various checks are performed depending on Filetyp.
  # -> TRUE/FALSE
  #checks if the selected input type (DNA, ProteinonlyDNA, UProc, RDS) is processable
  
  .nFiles = length(filepaths)
  
  if (SelectedType == FILETYPE.DNA)
  {
    #test if file format is correct
    for (i in 1:.nFiles)
    {
      .check <- check.DNA(filepaths[[i]])  
    }
  }

  else if (SelectedType == FILETYPE.DNAwoRNA)
  {
    #test if file format is correct
    for (i in 1:.nFiles)
    {
    .check <- check.DNA(filepaths[[i]])  
    }
  }

  else if (SelectedType == FILETYPE.UproC)
  {
    #test against correct version of uproc
    
    #test if file format is correct
    for (i in 1:.nFiles)
    {
    .check <- check.uproc(filepaths[[i]])
    }
  }

  else if (SelectedType == FILETYPE.RDS)
  {
    for (i in 1:.nFiles)
    {
      .check <- check.RDS(filepaths[[i]])
      
    }
    #test if file format is correct

  }
  
  else
  {
  return(BAD())  
  }
  
return(.check)  
}


#test if each directory exists
check.dir <- function(outDir,ListOfDirs)
{
nDirlength = nchar(outDir)
  if (substr(outDir,nDirlength,nDirlength) == .Platform$file.sep)
  {
  outDir = substr(outDir,1,(nDirlength-1))
  }
  
  for (i in 1:length(ListOfDirs))
  {
    if (!file.exists(file.path(outDir,ListOfDirs[[i]])))
    {
    return(BAD())
    }
  }
return(OK())
}


check.someuproc.error <- function(returned.system.string)
{
#checks if uproc gives an error.  
  if (!is.null(attr(returned.system.string,'status')))
  {
    #different errors depending of notice are possible...
    FATAL_ERROR$new('I WILL FIND YOU AND I WILL KILL YOU')$throw()
  }
}

check.testforpackage <- function(test.package,LOAD.FLAG)
{
  #check if package is availible
  if(test.package %in% rownames(installed.packages()))
  {
    #load package
    if (LOAD.FLAG)
    {
      return(library(test.package,character.only = TRUE, logical.return = TRUE))
    }
    
    else
    {
    return(TRUE)
    }
    
  }
  return(FALSE)
}

uproc.working.check <- function()
{
  if (!is.null(._CONFIG$UPROC_DIR) & !is.null(._CONFIG$MODEL_DIR) & !is.null(._CONFIG$UPROC_DB))
  {
  Result = c ("1,X1,199,66,1,1,66,K00099,8.749,980,0",      "2,X2,199,66,3,3,66,K00099,9.927,1032,5",
              "3,X3,199,66,2,2,66,K00099,10.055,980,6",     "4,X4,199,66,1,1,66,K00099,9.551,980,8",     
              "5,X5,199,65,3,3,65,K00099,9.798,1032,4",     "6,X6,199,66,2,2,66,K00099,10.394,980,6",    
              "7,X7,237,78,1,1,78,K00099,11.023,980,4",     "8,y1,199,66,1,1,66,K00991,7.072,2197,0",    
              "9,y2,199,65,3,3,65,K00991,9.355,980,2",      "10,y3,199,66,2,2,66,K00991,9.142,509,2",    
              "11,y4,312,103,1,1,103,K00991,15.376,2197,2")
  .command <- paste0(._CONFIG$UPROC_DIR,' -p -s ',._CONFIG$UPROC_DB,' ',._CONFIG$MODEL_DIR,' ',file.path(DATA_PATH,'reduced_100.fa'))
  print(.command)
  
  .Q <- system(.command,intern=TRUE)
    if (length(.Q) > 0)
    {
      if (sum(.Q[3:13] == Result) == length(Result))
      {
        return(TRUE)
      }      
    }
  }
#reset config to NULL
return(FALSE)
  
}


