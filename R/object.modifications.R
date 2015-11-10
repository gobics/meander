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


store.Objectpart <- function(Object.Part,filename)
{
  
}