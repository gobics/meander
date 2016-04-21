##  WARNINGS
  #Methods
    WARNING_MESSAGE_NOREPLACEMENT = 'No replacement for method %s found.\n'
    WARNING_MESSAGE_DIFFERENTMETHOD = 'Replacement Method %s selected for %s.\n'
    #
  ##


    
    
    
    
    
############
    THREE_COL_FUNCTION <- (c("pink", "purple","darkblue"))    
    
    
    
    #############
    
    
    
    
    
    
DEBUG.PRINT <- TRUE


FILETYPE.DNA <- "DNA"
FILETYPE.DNAwoRNA <- "RNAfilteredDNA"
FILETYPE.UproC <- "UproC"
FILETYPE.RDS <- "RDS"
FILETYPE.Object <- "Object"


OBJECT.OK <- TRUE
OBJECT.ERROR <- FALSE



INPUTDEPENDENTSTEPS.LIST.ONE = FILETYPE.DNA
INPUTDEPENDENTSTEPS.LIST.TWO = c(INPUTDEPENDENTSTEPS.LIST.ONE,FILETYPE.DNAwoRNA)
INPUTDEPENDENTSTEPS.LIST.THREE = c(INPUTDEPENDENTSTEPS.LIST.TWO,FILETYPE.UproC)
INPUTDEPENDENTSTEPS.LIST.FOUR = c(INPUTDEPENDENTSTEPS.LIST.THREE,FILETYPE.RDS)

PLOTCHOICES = c('ggplot2','plot')

PLOTTYPE = PLOTCHOICES[1]

TAXONOMY.LEVELS = 8

#### METHOD
METHOD.REPLACEMENT <- list(
                    'DESeq2' = c('DESeq2','DESeq'),
                    'edgeR' = c('edgeR','limma'),
                    'samr' = 'samr'
                    )


DESEQ2.CLASS <- new("Class.DESeq2")
DESEQ.CLASS <- new("Class.DESeq")
EDGER.CLASS <- new("Class.edgeR")
LIMMA.CLASS <- new("Class.limma")
SAMR.CLASS <- new("Class.samr")

  #########

METHOD.CALLS <- setClass (
  "METHOD.CALLS",
  slots = c(
    DESeq2 = "Class.DESeq2",
    DESeq = "Class.DESeq",
    edgeR = "Class.edgeR",
    limma = "Class.limma",
    samr = "Class.samr"
  ),
  prototype = list(
    DESeq2 = DESEQ2.CLASS,
    DESeq = DESEQ.CLASS,
    edgeR = EDGER.CLASS,
    limma = LIMMA.CLASS,
    samr = SAMR.CLASS
  )
)



METHOD.LIST <- new("METHOD.CALLS")


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

