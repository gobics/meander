.errorZ <- setClass("errorZ",
         representation(Error="character", Warning="character")
         )


.ObjectPaths <- setClass	(
  #Name
  "ObjectPaths",
  #Sots
  slots =	c(
    DirOut = "character",
    DNA = "vector",
    RNAfilteredDNA = "vector",
    UproC = "vector",
    RDS = "vector",
    Type = "character"
  ),
  # default = empty
  prototype=list(
    DirOut = '',
    DNA = vector(),
    RNAfilteredDNA = vector(),
    UproC = vector(),
    RDS = vector(),
    Type = ''
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
    PCA = data.frame(Sample = NULL, PC1 = NULL, PC2 = NULL, Class = NULL),
    LogFoldChange = data.frame(Couns = NULL, Change = NULL),
    Consensus = data.frame(Method = NULL, Stuff = NULL)
  )
)

.ObjectResume <- setClass(
  #Name
  "ObjectResume",
  #slots
  slots = c(
    where = 'character',
    which = 'numeric'
  ),
  # default
  prototype=list(
    where = '',
    which = 0
  )
)


.ObjectJobConfig <- setClass (
  #name
  "ObjectJobConfig",
  #slots
  slots = c(
    ClassVec = 'vector',
    ClassNames = 'vector',
    SelectedClasses = 'vector',
    SelectedTax = 'numeric',
    requiredLength = 'numeric'
    ),
  # default
  prototype=list(
    ClassVec = vector(),
    ClassNames = vector(),
    SelectedClasses = vector(),
    SelectedTax = -1,
    requiredLength = 0
  )
)






##TODO
#nReq <- number of list it must have...number of #Samples etc.

.Object.Global.Config <- setClass(
  "Object.Global.Config",
  slots = c(
    trash = 'character'
  )
)




.Object.DATA.dataframes <- setClass(
  "Object.DATA.dataframes",
  slots = c(
    Read.statistics = 'data.frame',
    Scores.Samples = 'data.frame'
  ),
  prototype = list(
    Read.statistics = data.frame(x = NULL, y = NULL, z = NULL),
    Scores.Samples = data.frame(x = NULL, y = NULL, z = NULL)
  )
)


.Object.DATA.BIG <- setClass (
  #Name
  "Object.DATA.BIG",
  #slots
  slots = c(
    SeqRNA = "list",
    CountDT = "data.table",
    Matrix = "matrix"
  ),
  prototype = list(
    SeqRNA = list(),
    CountDT = data.table(),
    Matrix = matrix
  )
)


.Object.DATA.Refined <- setClass (
  #Name
  "Object.DATA.Refined",
  #slots
  slots = c(
    QuickDT = "data.table",
    CountDT = "data.table"
  ),
  prototype = list(
    QuickDT = data.table(),
    CountDT = data.table()
  )
)

.Object.DATA.KEGG <- setClass (
  "Object.DATA.KEGG",
  slots = c(
    TaxMat = "data.frame"
  )
)


.Object.DATA <- setClass (
  #Name
  "Object.DATA",
  #slots
  slots = c(
  BIG = "Object.DATA.BIG",
  KEGG = "Object.DATA.KEGG",
  Refined = "Object.DATA.Refined",
  DataFrames = "Object.DATA.dataframes"
  )
)

.Object.Job.Statistics <- setClass(
  "Object.Job.Statistics",
  slots = c(
    UProCHits = 'numeric',
    RNA = 'numeric',
    reads = 'numeric',
    filtered.multi = 'numeric',
    filtered.rna = 'numeric',
    filtered.tax = 'numeric',
    filtered.ko = 'numeric',
    filtered.combo = 'numeric',
    filtered.score = 'numeric',
    ScoreCutoff = 'numeric',
    FilteringScore = 'numeric'
  ),
  prototype = list(
    UProCHits = numeric(),
    RNA = numeric(),
    reads = numeric(),
    filtered.multi = numeric(),
    filtered.rna = numeric(),
    filtered.tax = numeric(),
    filtered.ko = numeric(),
    filtered.combo = numeric(),
    filtered.score = numeric(),
    ScoreCutoff = numeric(),
    FilteringScore = numeric()
  )
)


.Object.Job.Paths <- setClass (
  #Name
  "Object.Job.Paths",
  #Sots
  slots =	c(
    DirOut = "character",
    DNA = "vector",
    RNAfilteredDNA = "vector",
    UproC = "vector",
    RDS = "vector",
    Type = "character"
  ),
  # default = empty
  prototype=list(
    DirOut = '',
    DNA = vector(),
    RNAfilteredDNA = vector(),
    UproC = vector(),
    RDS = vector(),
    Type = ''
  )
)

.Object.Job.Config <- setClass (
  #name
  "Object.Job.Config",
  #slots
  slots = c(
    ClassVec = 'vector',
    ClassNames = 'vector',
    SelectedClasses = 'vector',
    SelectedTax = 'numeric',
    requiredLength = 'numeric'
  ),
  # default
  prototype=list(
    ClassVec = vector(),
    ClassNames = vector(),
    SelectedClasses = vector(),
    SelectedTax = -1,
    requiredLength = 0
  )
)

.Object.Job <- setClass (
  #Name
  "Object.Job",
  #Slots
  slots = c(
    Paths = "Object.Job.Paths",
    Config = "Object.Job.Config",
    Statistics = "Object.Job.Statistics"
  )
)





Object <- setClass (
  #Name
  "MeandeRObject",
  #slots
  slots = c(
    DATA = "Object.DATA",
    Job = "Object.Job"
  )
  
)

##############checks###################


#TODO fix this shit!
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
                 retval <- c ( retval , "method selection different from default.\n")
               }

               if (is.null(retval))
                 return(TRUE)
               else
                 return (retval)
             })

setValidity ("Object.Job.Config",
             function ( object ){
               retval <- NULL
               if  (length(slot(object,'ClassVec')) == 0)
               {
                 retval <- c ( retval , 'OBJECT.ERROR.NORESUME')
               }
               
               if (slot(object,'requiredLength') == 0)
               {
                 retval <- c ( retval , 'OBJECT.ERROR.NORESUME2')
               }
               
               if (slot(object,'requiredLength') != length(slot(object,'ClassVec')))
               {
                 retval <- c ( retval , 'OBJECT.ERROR.NORESUME3')
               }
               
               if (length(slot(object,'ClassNames')) != length(unique(slot(object,'ClassVec'))))
               {
                 retval <- c ( retval , OBJECT.ERROR.NORESUME)
               }
               
               if (length(slot(object,'SelectedClasses')) != 2)
               {
                 retval <- c ( retval , OBJECT.ERROR.NORESUME)
               }
               
               if (is.null(retval))
                 return(TRUE)
               else
                 return (retval)
             })

              

#validObject(Z, test = TRUE)
