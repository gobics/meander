# ALL Objects as a Vector
OBJECTS.ALL <- list(
  "Object.Global.Config",
  "Object.DATA.dataframes",
  "Object.DATA.BIG",
  "Object.DATA.Refined",
  "Object.DATA.KEGG",
  "Object.DATA",
  "Object.Job.Statistics",
  "Object.Job.Paths",
  "Object.Job.Config",
  "Object.Job",
  "MeandeRObject"
  )




.Class.DESeq2 <- setClass (
  "Class.DESeq2",
  slots = c(
    DESeq2 = 'numeric'
  )
)

.Class.DESeq <- setClass (
  "Class.DESeq",
  slots = c(
    DESeq = 'numeric'
  )
)

.Class.edgeR <- setClass (
  "Class.edgeR",
  slots = c(
    edgeR = 'numeric'
  )
)

.Class.limma <- setClass (
  "Class.limma",
  slots = c(
    limma = 'numeric'
  )
)

.Class.samr <- setClass (
  "Class.samr",
  slots = c(
    samr = 'numeric'
  )
)



setGeneric("methodPackage", function(x) 
{
  standardGeneric("methodPackage")
}
)


ClassList <- list("Class.DESeq2","Class.DESeq","Class.edgeR","Class.limma","Class.samr")

  for (i in 1:length(ClassList))
  {
    setMethod("methodPackage", ClassList[[i]] ,
          function(x)
          {
            slotNames(x)
          }
          
          )
  }



#### Run edger, DESeq2 etc.

setGeneric("executeMethod",
           function(z,x,y)
           {
             print(z)
             ERROR$new('NO SUCH METHOD')$throw()}
)

setMethod ("executeMethod", "Class.DESeq2",
           function(z, x, y){
             perform.deseq2(x,y)
           })

setMethod ("executeMethod", "Class.DESeq",
           function(z, x, y){
             perform.deseq(x,y)
           })

setMethod ("executeMethod", "Class.edgeR",
           function(z, x, y){
             perform.edger(x,y)
           })

setMethod ("executeMethod", "Class.limma",
           function(z, x, y){
             perform.limma(x,y)
           })

setMethod ("executeMethod", "Class.samr",
           function(z, x, y){
             perform.samr(x,y)
           })



setGeneric("saveObject", function(x,z) 
{
  standardGeneric("saveObject")
}
)


for (i in 1:length(OBJECTS.ALL))
{
  setMethod("saveObject", OBJECTS.ALL[[i]],
          function(x,z)
          {
            print(str(getClass(x)))
            filepath = file.path(slot(z,'DirOut'),'OBJECT',paste0(class(x)[1],'.rds'))
            saveRDS(object = x, file = filepath)
          }
          )
}



