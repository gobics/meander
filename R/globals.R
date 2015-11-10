set.globals <- function () {
  #Methods
  assign("method.ONE.A", "DEseq2", envir = .GlobalEnv)
  assign("method.ONE.B", "DEseq", envir = .GlobalEnv)
  assign("method.TWO.A", "edgeR", envir = .GlobalEnv)
  assign("method.THREE.A", "samr", envir = .GlobalEnv)
  
  #Filetypes

}




OBJECT.ERROR.NOCLASSES <- 'no Classes.\n'
OBJECT.ERROR.reqLENGTH <- 'required length not set.\n'
OBJECT.ERROR.LENGTH.CLASSES.reqLENGTH <- "length differ.\n"
OBJECT.ERROR <- "length ClassNames & ClassVec differ.\n"



OBJECT.ERROR.NORESUME <- 'death\n'
OBJECT.ERROR.RESUME <- 'half death\n'
OBJECT.WARNING <- 'all "fine"\n'



FILETYPE.DNA <- "DNA"
FILETYPE.DNAwoRNA <- "RNAfilteredDNA"
FILETYPE.UproC <- "UproC"
FILETYPE.RDS <- "RDS"
FILETYPE.Object <- "Object"


OBJECT.OK <- TRUE
OBJECT.ERROR <- FALSE






MESSAGES <- list(
OBJECT.OK = TRUE,
OBJECT.ERROR = FALSE,
OBJECT.ERROR.NOFASTAHEADER = 'For a FASTA file, first expected symbol is ">".\n',
OBJECT.WARNING.FASTA = 'Short on nucleotides?\n',
OBJECT.ERROR.RDS = 'File is not in RDS format!\n',
OBJECT.ERROR.UPROCVERSION = 'UProC version is wrong.\n',
OBJECT.ERROR.UNKNOWN = 'not in list for checks.\n'
)






.ok.fun <- function()
{
  cat("all is ok.\n")  
}

.bad.fun <- function()
{
  cat("all is bad.\n") 
  stop('rip')
}


NEXT.DNAwoRNA <- setClass	(
  #Name
  "NEXT.DNAwoRNA",
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




