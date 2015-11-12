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
check.uprocversion <- function(Command,ObjectPart)
{
  #checks if the version of UProc is the correct one
  #works on linux and windows
  .Q <- system(Command,intern=TRUE)
  if (.Q[2] == "uproc tax")
  {
    return(OK())
  }
  return(BAD())
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




