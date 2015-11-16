set.Conditions <- function(List,
                           Object.job.config,
                           TCLTKFLAG,Message = 'please enter each unique condition in your experiment.\n(Seperated by SPACE)\n',
                           Message2 = 'you need at least 2 different conditions.\n', 
                           Message3 = 'please select all files for this condition:\n')
{
  .loop = TRUE
  
  .nEntry = length(List)
  
  while(.loop)
  {
    if (TCLTKFLAG)
    {
      
    }
    
    else
    {
      .Answer <- set.string(TCLTKFLAG,Message)
      .Answer = unlist(strsplit(.Answer,split = '[[:blank:]]+'))
      .nAnswer.length = length(.Answer)
        if (.nAnswer.length >= 2)
        {
          for (i in 1:.nAnswer.length)
          {
          cat('[',i,'] ',.Answer[i],'\n')
          } 
        .loop = FALSE  
        }
      
      else
      {
      cat(Message2)
      }

    }    
  }
  .ConditionVec = numeric(length = .nEntry)
  .LogiVec = logical(length = .nEntry)
  .loop = TRUE
  
  .NumericList = 1:.nEntry
  .LogiVec.Condition = vector(mode = 'logical', length = .nAnswer.length)
  
  while(.loop)
  {
    for (i in 1:.nAnswer.length)
    {
    cat('select files for condition: ',.Answer[i],'\n')
      for (j in 1:.nEntry)
      {
        if (.LogiVec[j] == FALSE)
        {
        cat('[',j,'] ',List[[j]],'\n')  
        }
      
      }
      
      .New.Answer <- set.string(TCLTKFLAG,Message3)
      .New.Answer = unlist(strsplit(.New.Answer,split = '[[:blank:]]+'))
      .New.nAnswer.length = length(.New.Answer)   
      for (j in 1:.New.nAnswer.length)
      {
        cat('[',j,'] ',.New.Answer[j],'\n')
        if (length(.New.Answer[j]) != 0)
        {
          if (.New.Answer[j] %in% .NumericList)
          {
          .LogiVec[as.numeric(.New.Answer[j])] = TRUE  
          .LogiVec.Condition[i] = TRUE
          .ConditionVec[as.numeric(.New.Answer[j])] = i
          }
          
          else
          {
            cat('IDIOT.\n')
          }
        }

        else
        {
          cat('IDIOT.\n')
        }
        
      } 
    } 
    
    if (sum(.LogiVec) != .nEntry | sum(.LogiVec.Condition) != .nAnswer.length)
    {
      print(sum(.LogiVec))
      print(.nEntry)
    cat('you missed some files...\n\n')
    .LogiVec = logical(length = .nEntry) 
    }
    
    else
    {
      .loop = FALSE
    }
  }

  
  Object.job.config <- setInputdata(Object.job.config,'ClassVec',.ConditionVec)
  Object.job.config <- setInputdata(Object.job.config,'ClassNames', .Answer)
  return(Object.job.config)
}

set.selected.Conditions <- function(TCLTKFLAG,Object.job.config)
{
  .loop = TRUE
  
  Choices <- slot(Object.job.config,'ClassNames')
  
    while(.loop)
    {
    .ret <- select.multiple.List(TCLTKFLAG,Choices)
      if (length(.ret) == 2)
      {
        .loop = FALSE  
      } 
    
      else
      {
      cat('select 2, or else...\n');
      }
    }
  .Selected = which(.ret %in% Choices)
  Object.job.config <- setInputdata(Object.job.config,'SelectedClasses',.Selected)
return(Object.job.config)
}