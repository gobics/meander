select.File.list <- function(FLAG)
{
  if (FLAG)
  {

  }

  else
  {

  }
}

select.dir <- function(FLAG.tcltk,FLAG.create,Message)
{
.done = FALSE
.path = ''
  if (FLAG.tcltk)
  {

  }


  else
  {

    while(.done == FALSE)
    {
    .path <- set.string(FLAG.tcltk,Message)
      if (file.exists(file.path(.path)))
      {
      .done = TRUE
      }

      else
      {
      cat('Folder ',.path,' does not exists.\n')
        if (FLAG.create)
        {
        cat('create Folder?\n')
          if (get.yesno(FLAG.tcltk))
          {
          dir.create(.path, showWarnings = TRUE, recursive = TRUE)
            if (!file.exists(file.path(.path)))
            {
            cat("can't create folder.")
            }

            else
            {
            .done = TRUE
            }
          }
        }
      }
    }
  }
return(.path)
}


#.select.File.type(FAG = FALSE, Filetypes = c("A","B","C"))
select.List <- function(FLAG,Choices)
{
.done = FALSE
.nposs.filetypes = length(Choices)
#generate allowed numbers
.ValidInput = as.character(1:.nposs.filetypes)

#tcltk
  if (FLAG)
  {

  }

#no tcltk
  else
  {
  cat('Please select one of the following Choices:\n')
    for (i in 1:.nposs.filetypes)
    {
      cat('[',i,'] ',Choices[i],'\n')
    }

    while (.done == FALSE)
    {
    .Answer = readline(prompt = 'Selected number :')
    #find out if a number was typed

      if (.Answer %in% .ValidInput )
      {
      .ret <- as.integer(.Answer)
      .Answer = .ret
      .done = TRUE
      }
    #find out if number is in range
    }


  }
return(Choices[.Answer])
}

#.select.File.type(FAG = FALSE, Filetypes = c("A","B","C"))
select.multiple.List <- function(FLAG,Choices)
{
  .EZ <- .errorZ()
  .done = FALSE
  .nposs.filetypes = length(Choices)
  #generate allowed numbers
  .ValidInput = as.character(1:.nposs.filetypes)
  .AnswerVec = NULL
  #tcltk
  if (FLAG)
  {
    
  }
  
  #no tcltk
  else
  {
    cat('Please select one of the following Choices:\n')
    for (i in 1:.nposs.filetypes)
    {
      cat('[',i,'] ',Choices[i],'\n')
    }
    
    while (.done == FALSE)
    {
      .Answer = readline(prompt = 'Selected number :')
      .Answer = unlist(strsplit(.Answer,split = '[[:blank:]]+'))
      .nAnswer.length = length(.Answer)
      #find out if a number was typed
      for ( i in 1:.nAnswer.length)
      {
        if (.Answer[i] %in% .ValidInput )
        {
          .ret <- as.integer(.Answer[i])
          .AnswerVec = c(.AnswerVec,.ret)
          
        } 
        
        else
        {
          cat(.Answer[i],'is not a valid choice.\n')
          .AnswerVec = NULL
        }
        
        
      }
      
      if (sum(.Answer %in% .ValidInput) == .nAnswer.length)
      {
      .done = TRUE
      }
      #find out if number is in range
    }
    
    
  }
  return(Choices[.AnswerVec])
}

set.string <- function(FLAG,Message)
{
  #tcltk
  if (FLAG)
  {

  }

  else
  {
  .Answer = readline(prompt = Message)
  }
return(.Answer)
}

get.yesno <- function(FLAG.tcltk)
{
  if (FLAG.tcltk)
  {

  }

  else
  {
    while(1 == 1)
    {
    .Answer = readline(prompt = '[y/N] :')
      if (.Answer == 'y' | .Answer == 'Y' )
      {
      return(TRUE)
      }

      else if (.Answer == 'n' | .Answer == 'N')
      {
      return(FALSE)
      }
    }
  }


}



create.directory <- function(basepath,folders)
{
  for (i in 1:length(folders))
  {
    if (!file.exists(file.path(basepath,folders[i])))
    {
      dir.create(file.path(basepath,folders[i]), showWarnings = TRUE, recursive = TRUE)
      if (!file.exists(file.path(basepath,folders[i])))
      {
        cat(file.path(basepath,folders[i]),"can't create.\n")  
      }
    }
    
    else
    {
    cat(file.path(basepath,folders[i]),'already exists.\n')  
    }
    
  }
  
  

  
  
  
}