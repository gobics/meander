.check.DNA <- function(filepath)
{
  con <- file(filepath, "r", blocking = FALSE)
  linn=readLines(con,1)
    if (substr(linn,1,1) != '>')
    {
    return(FALSE)
    }
  linn=readLines(con,1)
gregexpr("[a,c,t,g,u,A,C,T,G,U]", linn)


}

.check.RDS <- function(filepath)
{
  .x <- try(readRDS(filepath))
  if (!isClass(x,"try-error"))
  {
  return(TRUE)
  }
return(FALSE)
}

.check.uproc <- function()
{

}


.check.package <- function(test.package)
{
  #check if package is availible
  if(test.package %in% rownames(installed.packages()))
  {
    #load package
    return(library(test.package,character.only = TRUE, logical.return = TRUE))
  }
  return(FALSE)
}



.check_RLSQ <- function()
{
  #check if libary is loaded
  #is.loaded("fragmentlength", "test123", "")
  dyn.load("baz.so")
  if (sample(100,1) > 50)
  {
    return(TRUE)
  }
  return(FALSE)
}

# check.uprocversion('/home/hklingen/Tool/uproc-1.1.2_sl/uproc-dna -v')
.check.uprocversion <- function(Command,ObjectPart)
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

.check.input <- function(SelectedType,PartObject,filepath)
{
  #takes the selected file "filepath" and various checks are performed depending on Filetyp.
  # -> TRUE/FALSE
  #checks if the selected input type (DNA, ProteinonlyDNA, UProc, RDS) is processable
  if (SelectedType == 'DNAin')
  {
    #test if file format is correct

  }

  else if (SelectedType == 'DNAwithoutRNAin')
  {
    #test if file format is correct
  }

  else if (SelectedType == 'UPRoCin')
  {
    #test against correct version of uproc

    #test if file format is correct
  }

  else if (SelectedType == 'RDSin')
  {
    #test if file format is correct
    if(.check.RDS)
    {
      return(TRUE)
    }

    else
    {

    }
  }
  return(FALSE)
}



.check.combination <- function()
{

}







