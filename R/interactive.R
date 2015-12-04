interactive.Taxa <- function(O.Job.Config, O.Data.Kegg, O.Data.Refined, Window)
{
  .generate.mat <- function(DataTable,Tax)
  {
    .DT2 <- DataTable[Previous == Tax]
    .u.Tax <- unique(.DT2[,TaxID])
    
    .Mat = matrix(0,ncol = length(AllowedSamples), nrow = length(.u.Tax))
    
    for (i in(1:length(AllowedSamples)))
    {
      
      for(j in 1:length(.u.Tax))
      {
        .tmp = .DT2[(TaxID %in% .u.Tax[j]) & Sample == AllowedSamples[i]][,Counts]
        if(length(.tmp) > 0)
        {
          .Mat[j,i] = .tmp
        }
      }
    }
    rownames(.Mat) <- c(.u.Tax)
    return(.Mat)
  }
  
  
  
  ClassVec <- slot(O.Job.Config,'ClassVec')
  SelectedClasses <- slot(O.Job.Config,'SelectedClasses')
  SelectedSamples = ClassVec %in% SelectedClasses
  
  AllowedSamples <- which(SelectedSamples)
  
  .DT <- slot(O.Data.Refined,'QuickDT')[Sample %in% AllowedSamples]
  
  .CondA.I <- ClassVec %in% SelectedClasses[1]
  .CondB.I <- ClassVec %in% SelectedClasses[2]
  
  
  .lowestTaxa = unique(slot(O.Data.Kegg,'TaxMat')[,1])
  
  #set TaqxID to Root
  .Curr.Tax = -1
  .XVec.ROOT <- colSums(.generate.mat(.DT, .Curr.Tax))# in .Xmat.Root[1,] max possible counts
  
  .done = 0
  while(.done == 0)
  {
    
    
    .Xmat <- .generate.mat(.DT, .Curr.Tax)
    .Total = colSums(.Xmat)/.XVec.ROOT
    
    .nTax = dim(.Xmat)[1]
    
    print(.Xmat)
    
    .nTotalA = c(mean(.Total[.CondA.I]),rep(0,.nTax-1))
    .nTotalB = c(mean(.Total[.CondB.I]),rep(0,.nTax-1))
    .nClassA = sapply(1:.nTax, function(x) mean(.Xmat[x,.CondA.I]))
    .nClassB = sapply(1:.nTax, function(x) mean(.Xmat[x,.CondB.I]))
    
    #myDF <- data.frame(FractionA = .nTotalA, ClassA = .nClassA/sum(.nClassA), FractionB = .nTotalB, ClassB = .nClassB/sum(.nClassB))
    
    myMat = cbind(.nTotalA,.nClassA/sum(.nClassA),.nTotalB,.nClassB/sum(.nClassB))
    rownames(myMat) <- rownames(.Xmat)
    colnames(myMat) <- c('FractionA','CondA','FractionB','CondB')
    barplot(myMat)
    print(.Curr.Tax)
    print(myMat)
    .Ret <- dostuff(Window,as.list(rownames(myMat)),as.list(1:length(rownames(myMat))),as.list(myMat[,2]),'select TaxID')
    
    if (.Ret[1] == 'OK')
    {
      O.Job.Config <- setInputdata(O.Job.Config,'SelectedTax',as.numeric(rownames(myMat)[.Ret[[2]]]))
      .done = 1
    }
    
    else if (.Ret[1] == 'Next')
    {
      if (as.numeric(rownames(myMat)[.Ret[[2]]]) %in% .lowestTaxa)
      {
        
      }
      else
      {
        .Curr.Tax = as.numeric(rownames(myMat)[.Ret[[2]]])
      }
      
    }
    
    else if (.Ret[1] == 'Prev')
    {
      if (.Curr.Tax != -1)
      {
        .Curr.Tax = .DT[TaxID == .Curr.Tax,Previous][1]
      }
    }
  }
return(O.Job.Config)  
}


dostuff <- function(MasterWindow,Names.list,Variable.list,Percentage.list,Question.text)
{
  SlaveWindow <- tktoplevel(MasterWindow)
  nStuff <- length(Names.list)
  rb <- list();
  rb2 <- list();
  rb3 <- list();
  rbValue <- tclVar(Variable.list[[1]])
  label <- tklabel(SlaveWindow,text=Question.text)
  label2 <- tklabel(SlaveWindow,text='percentage')
  label3 <- tklabel(SlaveWindow,text='Selection')
  tkgrid(label,label2,label3)
  
  .curr.Tax = -1
  #max counts
  build.DT.Part <- function(DT,rank)
  {
    return(DT[Rank == rank])
  }
  
  for (i in 1:nStuff)
  {
    rb[[i]] <- tkradiobutton(SlaveWindow)
    rb2[[i]] <- tklabel(SlaveWindow,text=Names.list[[i]])
    rb3[[i]] <- tklabel(SlaveWindow,text=Percentage.list[[i]])
    tkconfigure(rb[[i]],variable=rbValue,value=Variable.list[[i]])
    tkgrid(rb2[[i]],rb3[[i]],rb[[i]])
  }
  
  
  OnOK <- function()
  {
    zVal <<- list('OK',as.numeric(tclvalue(rbValue)))
    tkdestroy(SlaveWindow)
  }
  
  OnNext <- function()
  {
    zVal <<- list('Next',as.numeric(tclvalue(rbValue)))
    tkdestroy(SlaveWindow)
  }
  
  OnPrevious <- function()
  {
    #
    zVal <<- list('Prev',as.numeric(tclvalue(rbValue)))
    tkdestroy(SlaveWindow)
  }
  
  Pr.but <- tkbutton(SlaveWindow,text="Previous",command= OnPrevious)
  OK.but <- tkbutton(SlaveWindow,text="OK",command= OnOK)
  Ne.but <- tkbutton(SlaveWindow,text="Next",command= OnNext)
  tkgrid(Pr.but,OK.but,Ne.but)
  tkfocus(SlaveWindow)
  tkwait.window(SlaveWindow)
  return(zVal)
}