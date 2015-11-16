appendInputdata <- function(ObjectPart,Type,value)
{
  slot(ObjectPart,Type) <- append(slot(ObjectPart,Type),value)
  return(ObjectPart)
}

setInputdata <- function(ObjectPart,Type,value)
{
  slot(ObjectPart,Type) <- value
  return(ObjectPart)
}


storeObjectpart <- function(Object.Part,filename)
{
Sys.chmod(path = filename, mode = '0700')
saveRDS(object = Object.Part, file = filename ) 
Sys.chmod(path = filename, mode = '0400')
}

setObjectposition <- function(Object,type,position)
{
slot(Object,'Step') = type
slot(Object,'Position') = position
return(Object)
}