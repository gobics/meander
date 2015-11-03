.select.File.list <- function(FLAG)
{
  if (FLAG)
  {

  }

  else
  {

  }
}

.select.dir <- function(FLAG.tcltk,FLAG.create,Message)
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
    .path <- .set.string(FLAG.tcltk,Message)
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
          if (.get.yesno(FLAG.tcltk))
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
.select.List <- function(FLAG,Choices)
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


.set.string <- function(FLAG,Message)
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

.get.yesno <- function(FLAG.tcltk)
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
