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
  .Selected = which(Choices %in% .ret)
  Object.job.config <- setInputdata(Object.job.config,'SelectedClasses',.Selected)
return(Object.job.config)
}



set.methods <- function(O.Job.Config)
{
  #try to load all methods
  
  #find how many diff methods should be used
  nDiff.Methods = length(METHOD.REPLACEMENT)
  
    for (i in 1:nDiff.Methods)
    {
      #look for first existing library
      for (j in 1:length(METHOD.REPLACEMENT[[i]]))
      {
        cat(METHOD.REPLACEMENT[[i]][j],'\n')
        if (METHOD.REPLACEMENT[[i]][j] %in% rownames(installed.packages()))
        {
          #only non-default method availible, if method availible at all
          if (j != 1)
          {
            #Warning
            WARNING_DIFFERENT_METHOD(rep.method = METHOD.REPLACEMENT[[i]][j], ori.method = names(METHOD.REPLACEMENT)[i])$issue(TRUE)
          }
          
          
          O.Job.Config <- appendInputdata(O.Job.Config,'Methods',METHOD.REPLACEMENT[[i]][j])
          library(package = METHOD.REPLACEMENT[[i]][j],character.only = TRUE)
          break  
        }
        
        #NO alternative can be loaded for this methodslot
        if (j  == length(METHOD.REPLACEMENT[[i]]))
        {
          WARNING_NO_REPLACEMENT(names(METHOD.REPLACEMENT)[i])$issue()
        }
      }
    }
  
  
  return(O.Job.Config)
  
    for (i in 1:length(METHOD.REPLACEMENT.DESEQ2))
    {
      if (METHOD.REPLACEMENT.DESEQ2[i] %in% rownames(installed.packages()))
      {
      O.Job.Config <- appendInputdata(O.Job.Config,'Methods',METHOD.REPLACEMENT.DESEQ2[i])
      library(A,character.only = TRUE)
        break  
      }
      
      if (i  == length(METHOD.REPLACEMENT.DESEQ2))
      {
       cat('shiiiiiit\t1\n') 
      }
    }
  #try to get second method
    for (i in 1:length(METHOD.REPLACEMENT.EDGER))
    {
      if (METHOD.REPLACEMENT.EDGER[i] %in% rownames(installed.packages()))
      {
        O.Job.Config <- appendInputdata(O.Job.Config,'Methods',METHOD.REPLACEMENT.EDGER[i])
        break  
      }
      
      if (i  == length(METHOD.REPLACEMENT.EDGER))
      {
        cat('shiiiiiit\t2\n') 
      }
    }
  #try to get third method  
    for (i in 1:length(METHOD.REPLACEMENT.SAMR))
    {
      if (METHOD.REPLACEMENT.SAMR[i] %in% rownames(installed.packages()))
      {
        O.Job.Config <- appendInputdata(O.Job.Config,'Methods',METHOD.REPLACEMENT.SAMR[i])
      break  
      }
      
      if (i  == length(METHOD.REPLACEMENT.SAMR))
      {
        cat('shiiiiiit\t3\n') 
      }
    }
}