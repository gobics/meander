set.globals <- function () {
  #Methods
  assign("method.ONE.A", "DEseq2", envir = .GlobalEnv)
  assign("method.ONE.B", "DEseq", envir = .GlobalEnv)
  assign("method.TWO.A", "edgeR", envir = .GlobalEnv)
  assign("method.THREE.A", "samr", envir = .GlobalEnv)
  
  #Filetypes

}

DEBUG.PRINT <- TRUE


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



INPUTDEPENDENTSTEPS.LIST.ONE = FILETYPE.DNA
INPUTDEPENDENTSTEPS.LIST.TWO = c(INPUTDEPENDENTSTEPS.LIST.ONE,FILETYPE.DNAwoRNA)
INPUTDEPENDENTSTEPS.LIST.THREE = c(INPUTDEPENDENTSTEPS.LIST.TWO,FILETYPE.UproC)
INPUTDEPENDENTSTEPS.LIST.FOUR = c(INPUTDEPENDENTSTEPS.LIST.THREE,FILETYPE.RDS)

PLOTCHOICES = c('ggplot2','plot')

PLOTTYPE = PLOTCHOICES[1]



TAXONOMY.LEVELS = 8


METHOD.REPLACEMENT <- list(
                    'DESeq2' = c('DESeq2','DESeq'),
                    'edgeR' = c('edgeR','limma'),
                    'samr' = 'samr'
                    )

METHOD.REPLACEMENT.DESEQ2 = c('DESeq2','DESeq');
METHOD.REPLACEMENT.EDGER = c('edgeR','limma');
METHOD.REPLACEMENT.SAMR = c('samr')






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

# THIS FILE IS INTENDED TO STORE GLOBAL CONSTANTS NEEDED THROUGHOUT THE HOLE PROGRAMM
# SUCH AS BUT NOT LIMITED TO PATH NAMES DEFAULT FILE EXTENSIONS AND COLOR DESCRIPTORS
# DATE: 2015-11-01

# ******************************************************************************
# BEGIN     :   FILE CONSTANTS
# ******************************************************************************

EXTENSION_SEPERATOR = '.'

DATA_DIR_NAME = 'data'
DATA_PATH = file.path(system.file(package=getPackageName()), DATA_DIR_NAME)

HTML_TEMPLATE_DIR_NAME = 'template'
HTML_TEMPLATE_PATH = file.path(system.file(package=getPackageName()), HTML_TEMPLATE_DIR_NAME)

HTML_SUPPLY_DIR_NAME = 'supply'
HTML_SUPPLY_PATH = file.path(system.file(package = getPackageName()), HTML_SUPPLY_DIR_NAME)

ICON_DIR_NAME = 'icon'
ICON_PATH = file.path(system.file(package=getPackageName()), ICON_DIR_NAME)

RDS_PATH = DATA_PATH
RDS_FILE_EXTENSION = 'Rds'

# ******************************************************************************
# END     :   FILE CONSTANTS
# ******************************************************************************

APPLICATION_TITLE = 'meandeR'

# RUNNING_MODES
RUNNING_MODE_BATCH = 'BATCH'
RUNNING_MODE_INTERACTIVE = 'INTERACTIVE'
RUNNING_MODE_FRONTEND = 'FRONTEND'
