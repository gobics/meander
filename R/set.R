set.Conditions <- function()
{
  
}

set.selected.Conditions <- function(TCLTKFLAG,Choices)
{
  .loop = TRUE
    while(.loop)
    {
    .ret <- select.multiple.List(TCLTKFLAG,Choices)
      if (length(.ret) == 2)
      {
        .loop = FALSE  
      } 
    
      else
      {
      cat('select 2, you stupid shit!\n');
      }
    }
return(which(.ret %in% Choices))
}