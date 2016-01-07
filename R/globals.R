##  WARNINGS
  #Methods
    WARNING_MESSAGE_NOREPLACEMENT = 'No replacement for method %s found.\n'
    WARNING_MESSAGE_DIFFERENTMETHOD = 'Replacement Method %s selected for %s.\n'
    #
  ##


    
    
    
    
    
############
    THREE_COL_FUNCTION <- colorRampPalette(c("pink", "purple","darkblue"))    
    
    
    
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

# THIS FILE IS INTENDED TO STORE GLOBAL CONSTANTS NEEDED THROUGHOUT THE HOLE PROGRAMM
# SUCH AS BUT NOT LIMITED TO PATH NAMES DEFAULT FILE EXTENSIONS AND COLOR DESCRIPTORS
# DATE: 2015-11-01

# ******************************************************************************
# BEGIN     :   GENERAL       
# ******************************************************************************

APPLICATION_TITLE = 'meandeR'

# RUNNING_MODES
RUNNING_MODE_BATCH = 'BATCH'
RUNNING_MODE_INTERACTIVE = 'INTERACTIVE'
RUNNING_MODE_FRONTEND = 'FRONTEND'


# ******************************************************************************
# > END     :   GENERAL       
# ******************************************************************************

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

HTML_OUTPUT_SUBDIR = 'html'
ORTHOLOG_TABLE_SUBDIR	= 'ortholog'
PATHWAY_TABLE_SUBDIR	= 'pathway'

SVG_OUTPUT_SUBDIR = 'svg'

# ******************************************************************************
# > END     :   FILE CONSTANTS
# ******************************************************************************

# ******************************************************************************
# BEGIN     :   COLORS
# ******************************************************************************

# ORTHOLOG COLOR FLAGS
SHOULD_MAP_FLAG = 1
MAPPED_FLAG = 2
UP_REGULATED_FLAG = 4
SIGNIFICANT_FLAG = 8
COLOR_BLIND_FLAG = 16

DEFAULT_COLOR = '#808080'

ORTHOLOG_COLORS = c(
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#BBE000',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#E08000',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#00DB30',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#DB0030',
#COLOR BLIND MODE
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#BBE000',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#00DB30',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#E08000',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#DB0030'
)


SIGNIFICANT_UP_REGULATED_DEFAULT_COLOR = ORTHOLOG_COLORS[16]
INSIGNIFICANT_UP_REGULATED_DEFAULT_COLOR = ORTHOLOG_COLORS[8]
SIGNIFICANT_DOWN_REGULATED_DEFAULT_COLOR = ORTHOLOG_COLORS[12]
INSIGNIFICANT_DOWN_REGULATED_DEFAULT_COLOR = ORTHOLOG_COLORS[4]
MAPPED_AND_SHOULD_NOT_DEFAULT_COLOR = ORTHOLOG_COLORS[3]
NOT_MAPPED_AND_SHOULD_DEFAULT_COLOR = ORTHOLOG_COLORS[2]
NOT_MAPPED_AND_SHOULD_NOT_DEFAULT_COLOR = ORTHOLOG_COLORS[1]

SIGNIFICANT_UP_REGULATED_COLORBLIND_COLOR = ORTHOLOG_COLORS[32] 
INSIGNIFICANT_UP_REGULATED_COLORBLIND_COLOR = ORTHOLOG_COLORS[24]
SIGNIFICANT_DOWN_REGULATED_COLORBLIND_COLOR = ORTHOLOG_COLORS[28]
INSIGNIFICANT_DOWN_REGULATED_COLORBLIND_COLOR = ORTHOLOG_COLORS[20]
MAPPED_AND_SHOULD_NOT_COLORBLIND_COLOR = ORTHOLOG_COLORS[19]
NOT_MAPPED_AND_SHOULD_COLORBLIND_COLOR = ORTHOLOG_COLORS[18]
NOT_MAPPED_AND_SHOULD_NOT_COLORBLIND_COLOR = ORTHOLOG_COLORS[17]

# ******************************************************************************
# > END     :   COLORS
# ******************************************************************************

# ******************************************************************************
# BEGIN     :   %TEMPLATE%
# ******************************************************************************

# ******************************************************************************
# > END     :   %TEMPLATE%
# ******************************************************************************

