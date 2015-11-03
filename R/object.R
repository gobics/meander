.ObjectPaths <- setClass	(
  #Name
  "ObjectPaths",
  #Sots
  slots =	c(
    DirOut = "character",
    DNAin = "character",
    DNAwithoutRNAin = "vector",
    UPRoCin = "vector",
    RDSin = "vector"
  ),
  # default = empty
  prototype=list(
    DirOut = '',
    DNAin = '',
    DNAwithoutRNAin = '',
    UPRoCin = '',
    RDSin = ''
  )
)

.ObjectDependency <- setClass	(
  #Name
  "ObjectDependency",
  #Sots
  slots =	c(
    UProC = 'logical',
    UProCdb = 'logical',
    RNA = 'logical'
  ),
  # default = empty
  prototype=list(
    UProC = FALSE,
    UProCdb = FALSE,
    RNA = FALSE
  )
)

.ObjectUProCModus <- setClass	(
  #Name
  "ObjectUProCModus",
  #Sots
  slots =	c(
    UProCpath = "character",
    UProCbin = "character",
    UProC = "character",
    UProcLength.mode = 'character',
    Para.Version = 'character'

  ),
  # default = empty
  prototype=list(
    UProCpath = NULL,
    UProCbin = NULL,
    UProC = NULL,
    UProcLength.mode = NULL,
    Para.Version = ' -v'
  )
)


.ObjectRpackages <- setClass (
  #Name
  "ObjectRpackages",
  #slots
  slots = c(
    DESeq2 = "logical",
    DESeq = "logical",
    edger = "logical",
    samr = "logical",
    useRmethods = "vector"
  ),
  # default = empty
  prototype=list(
    DESeq2 = FALSE,
    DESeq = FALSE,
    edger = FALSE,
    samr = FALSE,
    useRmethods = vector()
  )
)

.ObjectDataFrames <- setClass(
  #Name
  "ObjectDataFrames",
  #slots
  slots = c(
    ReadComposition = 'data.frame',#DNA, RNA, no hits, ...
    UProCScores = 'data.frame',
    Taxonomy = 'data.frame',
    PCA = 'data.frame',
    LogFoldChange = 'data.frame',
    Consensus = 'data.frame'
  ),
  # default
  prototype=list(
    ReadComposition = data.frame(Sample = NULL, DNA = NULL, Hits = NULL, noTax = NULL, RNA = NULL),
    UProCScores = data.frame(Sample = NULL, XScores = NULL, YCounts = NULL),
    Taxonomy = data.frame(Sample = NULL, TaxID = NULL, Counts = NULL),
    PCA = data.frame(Sample = NULL),
    LogFoldChange = data.frame(Couns = NULL, Change = NULL),
    Consensus = data.frame(Method = NULL, Stuff = NULL)
  )
)


Object <- setClass (
  #Name
  "Config",
  #slots
  slots = c(
  Paths = "ObjectPaths",
  UProc = "ObjectUProCModus",
  AvailibleMethods = "ObjectDependency",
  AvailiblePackages = "ObjectRpackages"
  )
)

Object2 <- setClass (
  #Name
  "MeandeRObject",
  #slots
  slots = c(
  DATA = 'ObjectDataFrames'
  )

)

##############checks###################
setValidity ("ObjectPaths",
             function ( object ){
               retval <- NULL
               if ( nchar(slot(object,'DirOut')) == 0 & nchar(slot(object,'DNAin')) == 0 & nchar(slot(object,'DNAwithoutRNAin')) == 0 & nchar(slot(object,'UPRoCin')) == 0 & nchar(slot(object,'RDSin')) == 0  )
               {
                 retval <- c ( retval , "no input files selected")
               }



               if (is.null(retval))
                 return(TRUE)
               else
                 return (retval)
             })


setValidity ("ObjectRpackages",
             function ( object ){
               retval <- NULL
               if ( sum(slot(object,'useRmethods') %in% c('DESeq2','DESeq','edger','samr')) < 3 )
               {
                 retval <- c ( retval , "method selection different from default.")
               }

               if (is.null(retval))
                 return(TRUE)
               else
                 return (retval)
             })


#validObject(Z, test = TRUE)
