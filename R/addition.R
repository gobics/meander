load.kegg.object.parts <- function()
{
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = NEW$Object.data.kegg , Type = 'TaxMat',value = readRDS(file.path(DATA_PATH,'TaxMat.rds')))
  #ko2path
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = NEW$Object.data.kegg , Type = 'KEGG2PATH', value = as.matrix(readRDS(file.path(DATA_PATH,'KEGG2PATH.rds'))))
  #kointax
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = NEW$Object.data.kegg , Type = 'KOinTax', value = readRDS(file.path(DATA_PATH,'KOlist.rds')))
  #...
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = NEW$Object.data.kegg , Type = 'ko2br.pathway', value = readRDS(file.path(DATA_PATH,'ko2br_pathway.rds')))
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = NEW$Object.data.kegg , Type = 'keggmapnames', value = readRDS(file.path(DATA_PATH,'keggmapnames.rds')))
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = NEW$Object.data.kegg , Type = 'pathway.names', value = readRDS(file.path(DATA_PATH,'pathway.names.Rds')))
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = NEW$Object.data.kegg , Type = 'ko_desc', value = readRDS(file.path(DATA_PATH,'ko_desc.rds')))
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = NEW$Object.data.kegg , Type = 'br2pathway', value = readRDS(file.path(DATA_PATH,'br2pathway.rds')))
}

input.object.dummy <- function()
{
  res = ''
  Ret2 <- NULL
  while(is.null(Ret2))
  {  
    Ret <- as.character(tkgetOpenFile(filetypes = "{{MeandeR} {.MeandeR}}
                                      {{All files} *}", multiple = FALSE))
    if (length(Ret) > 0 & Ret[1] != -1)
    {
      #read and check object
      Ret2 <- checkanduse.object(Ret);
      if (is.null(Ret2))
      {
        res <- tcltk.Message.Box(Message = 'File is no MeandeR object. Retry?')  
      }
      
      if (res == "no")
      {
        Ret2 = NaN
      }
    }
    
    
  }
  
  if (inherits(Ret2,"numeric"))
  {
    return(NULL);  
  }
  #set buttons
  
  ret <- change.states.object(NEW$Container.Object.Button)
  NEW$Container.Object.Button <- ret;  
  
  #split object into subparts
  extract.object.environment(Ret2)
  return(NULL);
}


checkanduse.object <- function(FullFilePath)
{
  X <- readRDS(FullFilePath)
  if (inherits(X,'MeandeRObject'))
  {
    return(X)
  }
  return(NULL);
}

extract.object.environment <- function(object.loaded)
{
  for (Slot.One in slotNames(Object()))
  {
    for (Slot.Two in slotNames(slot(Object(),Slot.One)))
    {
      for (Slot.Three in slotNames(slot(slot(Object(),Slot.One),Slot.Two)))
      {
        #check if it is availible in the object...
        if (Slot.One %in% slotNames(object.loaded))
        {
          if (Slot.Two %in% slotNames(slot(object.loaded,Slot.One)))
          {
            if (Slot.Three %in% slotNames(slot(slot(object.loaded,Slot.One),Slot.Two)))
            {
              #cat(Slot.One,Slot.Two,Slot.Three,'\n',sep = ' ')
              x <- slot(slot(slot(object.loaded,Slot.One),Slot.Two),Slot.Three)
              if (inherits(x,'matrix'))
              {
                if (length(x) > 1)
                {
                  set.object.environment(x,Slot.One,Slot.Two,Slot.Three)
                }
                
                else if (!is.na(x))
                {
                  set.object.environment(x,Slot.One,Slot.Two,Slot.Three)
                }
              }
              
              else if (inherits(x,c('matrix','logical','character','list','data.frame', "numeric","integer")))
              {
                if (length(x) > 0)
                {
                  set.object.environment(x,Slot.One,Slot.Two,Slot.Three)
                }
              }
              
              else if (inherits(x,'Object.Data.Pathview)'))
              {
                if (length(NEW$Object.Final@DATA@KEGG@ko2br.pathway@Matrix) > 0)
                {
                  set.object.environment(x,Slot.One,Slot.Two,Slot.Three)
                }
              }
              
            }
          }
        }
      }
    }
  }
  
  return(NULL)
}

set.object.environment <- function(Value,Slot.One,Slot.Two,Slot.Three)
{
  #all data subtrees in object
  if (Slot.One == 'DATA')
  {
    if (Slot.Two == 'BIG')
    {
      slot(NEW$Object.data.big,Slot.Three) <- Value
    }
    
    else if (Slot.Two == 'KEGG')
    {
      slot(NEW$Object.data.kegg,Slot.Three) <- Value
    }
    
    else if (Slot.Two == 'Refined')
    {
      slot(NEW$Object.data.refined,Slot.Three) <- Value
    }
    
    else if (Slot.Two == 'DataFrames')
    {
      slot(NEW$Object.data.dataframes,Slot.Three) <- Value
    }
    
    else
    {
      cat(Slot.Two,' does not exists in Object!\n')
      return(FALSE)
    }
  }
  #all job subtrees in object
  else if (Slot.One == 'Job')
  {
    if (Slot.Two == 'Paths')
    {
      #never overwrite the previous output location...
      if (Slot.Three != 'DirOut')
      {
        slot(NEW$Object.job.path,Slot.Three) <- Value
      }      
    }
    
    else if (Slot.Two == 'Config')
    {
      slot(NEW$Object.job.config,Slot.Three) <- Value
    }
    
    else if (Slot.Two == 'Statistics')
    {
      slot(NEW$Object.job.statistics,Slot.Three) <- Value
    }
    
    else
    {
      cat(Slot.Two,' does not exists in Object!\n')
      return(FALSE)
    }
  }
  
  else
  {
    cat(Slot.One,' does not exists in Object!\n')
    return(FALSE)  
  }
  return(TRUE)
}



change.states.object <- function(Button.Object)
{
  
  #change button skipping when object is input
  #Scores need to be skipped-> directly from Conditions to Taxonomy
  slot(slot(Button.Object,'button.process.conditions'),'interaction.on') <- ALL.BUTTON.NAMES[c(7,9)]
  slot(slot(Button.Object,'button.process.conditions'),'interaction.off') <- ALL.BUTTON.NAMES[c(9:17)]
  return(Button.Object)
}

interactive.score <- function()
{
  #find automated score
  .val <- calc.FilteringScore(Object.job.statistics = NEW$Object.job.statistics) 
  
  #set min and max score
  a <- min(slot(NEW$Object.data.dataframes,'Scores.Samples')$y)
  b <- max(slot(NEW$Object.data.dataframes,'Scores.Samples')$y)
  
  if (.val > 0 & a >= 0 & b > a )
  {
    x11();
    scale.valid_input <- function(...) { 
      val <- as.numeric(tclvalue(charalpha)) 
      if(a <= val & val <= b) { 
        message("set to ", val) 
        tclvalue(varalpha) <- val 
        
        slot(NEW$Object.job.statistics,'FilteringScore') <- val
        plot.uproc.scores(Object.job.statistics = NEW$Object.job.statistics,Object.data.dataframes = NEW$Object.data.dataframes) 
        
        
        return(TRUE)
      } else { 
        message("not valid...") 
        tkfocus(ed) 
        return(FALSE)
      } 
    } 
    
    scale.ok.function <- function()
    {
      if (scale.valid_input())
      {
        slot(NEW$Object.job.statistics,'FilteringScore') <- val
        tkdestroy(tt)
      }
    }
    
    scale.cancel.function <- function()
    {
      tkdestroy(tt)
    }
    
    
    
    tt<-tktoplevel() 
    varalpha <- tclVar(.val) 
    charalpha <- tclVar(as.character(a)) 
    
    
    scale <- tkscale(tt, from=a, to=b, resolution=0.01, label="UProC-TX Score", 
                     variable=varalpha, showvalue=TRUE, orient="horiz", length = 200) 
    ed <- tkentry(tt, textvariable=charalpha) 
    
    button.ok = ttkbutton(tt, text = 'ok', command = scale.ok.function)
    button.default = ttkbutton(tt, text = 'set', command = scale.valid_input)
    button.cancel = ttkbutton(tt, text = 'cancel', command = scale.cancel.function)
    
    tkgrid(ed, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
    tkgrid(button.ok, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
    tkgrid(button.default, row = 1, column = 1, columnspan = 1, sticky = 'nsew')
    tkgrid(button.cancel, row = 1, column = 2, columnspan = 1, sticky = 'nsew')
    tkgrid(scale, row = 2, column = 0, columnspan = 4, sticky = 'nsew')
    
    
    
    ## connect 
    tkconfigure(scale, command=function(...) { 
      tclvalue(charalpha) <- as.character(tclvalue(varalpha)) 
    }) 
    
    
    tkbind(ed, "<Return>", scale.valid_input) 
    #linux for numpad enter... hope this doesn't cause trouble in windows...
    tkbind(ed, "<KP_Enter>", scale.valid_input) 
  }
}