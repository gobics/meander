general.settings <- function(parent.window)
{
  frame.header <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 2, width = 200, height = 100)
  frame.middle <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 10,  width = 200, height = 100)
  frame.bottom <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 2,  width = 200, height = 100)

  general.label = tklabel(frame.header, text = "General Settings" ,background ='#9080F0' ,foreground = '#0ffff0')
  
  general.UProCTX = tklabel(frame.middle, text = 'UProC-TX' ,background ='#9080F0' ,foreground = '#0ffff0')
  general.MeandeR = tklabel(frame.middle, text = 'MeandeR' ,background ='#9080F0' ,foreground = '#0ffff0')
  
  pffff = ttkbutton(frame.middle, text = 'i keel you', command = ls)
  pffff2 = ttkbutton(frame.middle, text = 'me luv u long time', command = ls)
  
  tkgrid(general.label, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid(general.UProCTX, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid(general.MeandeR, row = 10, column = 0, columnspan = 2, sticky = 'nsew')
  
  tkgrid(pffff, row = 2, column = 1, columnspan = 1, sticky = 'nsew')
  tkgrid(pffff2, row = 3, column = 0, columnspan = 1, sticky = 'nsew')
  
  tkgrid(frame.header, row = 0, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(frame.middle, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(frame.bottom, row = 2, column = 0, columnspan = 1, sticky = 'nsew')

  #config header
  tkgrid.columnconfigure( frame.header, 0, weight = 1 )
  tkgrid.rowconfigure( frame.header, 0, weight = 1)
  
  #columnconfig middle
  tkgrid.columnconfigure( frame.middle, 0, weight = 1 )
  tkgrid.columnconfigure( frame.middle, 1, weight = 1 )
  
  #rowconfig middle
  tkgrid.rowconfigure( frame.middle, 0, weight = 1)
  tkgrid.rowconfigure( frame.middle, 1, weight = 1)
  tkgrid.rowconfigure( frame.middle, 10, weight = 1)
  
  #config bottom
  tkgrid.columnconfigure( frame.bottom, 0, weight = 1 )
  tkgrid.rowconfigure( frame.bottom, 0, weight = 1)
  
  tkgrid.columnconfigure( parent.window, 0, weight = 1 )

  tkgrid.rowconfigure( parent.window, 0, weight = 1)
  tkgrid.rowconfigure( parent.window, 1, weight = 1)
  tkgrid.rowconfigure( parent.window, 2, weight = 1)
}


general.settings.consensus <- function(parent.window)
{
  #set title of window
  tkwm.title(parent.window, 'Consensus Settings')
  
  but.function.ok <- function()
  {
    edit.ConfigFile.Keys(CONSENSUS = tclvalue(selection))
    
    ret.Val = tclVar('TRUE')
    tkdestroy(parent.window)
  }
  
  but.function.cancel <- function()
  {
    ret.Val = tclVar('FALSE')
    tkdestroy(parent.window)
  }
  
  selection <- tclVar('Consensus')
  
  frame.header <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 2, width = 200, height = 100)
  frame.middle <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 10,  width = 200, height = 100)
  frame.bottom <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 2,  width = 200, height = 100)
  
  general.label = tklabel(frame.header, text = "Consensus Settings" ,background ='#9080F0' ,foreground = '#0ffff0')
  
  one.label = tklabel(frame.middle, text = "Select one" ,background ='#9080F0' ,foreground = '#0ffff0')
  conseneus.label = tklabel(frame.middle, text = "majority rule" ,background ='#9080F0' ,foreground = '#0ffff0')
  all.label = tklabel(frame.middle, text = "all methods agree" ,background ='#9080F0' ,foreground = '#0ffff0')

  #radio buttons
  nMethods = length(slot(NEW$Object.job.config,'Methods'))
  method.selection.button <- list();                  
    for (i in 1:nMethods)
    {
      method.selection.button[[i]] = tkradiobutton(frame.middle, value = slot(NEW$Object.job.config,'Methods')[i], text = slot(NEW$Object.job.config,'Methods')[i], variable = selection)
    }
  method.selection.button.all = tkradiobutton(frame.middle, value = 'all', text = 'all', variable = selection)
  method.selection.button.consensus = tkradiobutton(frame.middle, value = 'Consensus', text = 'Consensus', variable = selection)
  
  tkgrid(general.label, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid(one.label, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(conseneus.label, row = 2, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(all.label, row = 3, column = 0, columnspan = 1, sticky = 'nsew')
  
  for (i in 1:nMethods)
  {
    tkgrid(method.selection.button[[i]], row = 1, column = i, columnspan = 1)
  }
  
  tkgrid(method.selection.button.consensus, row = 2, column = 1, columnspan = 1)
  tkgrid(method.selection.button.all, row = 3, column = 1, columnspan = 1)
  
  # buttons
  button.ok = ttkbutton(frame.bottom, text = 'OK', command = but.function.ok)
  button.cancel = ttkbutton(frame.bottom, text = 'cancel', command = but.function.cancel)
  
  tkgrid(button.ok, row = 0, column = 0, columnspan = 1)
  tkgrid(button.cancel, row = 0, column = 1, columnspan = 1)
  
  tkgrid(frame.header, row = 0, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(frame.middle, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(frame.bottom, row = 2, column = 0, columnspan = 1, sticky = 'nsew')
  
  tkgrid.columnconfigure( parent.window, 0, weight = 1 )
  
  tkgrid.rowconfigure( parent.window, 0, weight = 1)
  tkgrid.rowconfigure( parent.window, 1, weight = 1)
  tkgrid.rowconfigure( parent.window, 2, weight = 1)
  
  #config header
  tkgrid.columnconfigure( frame.header, 0, weight = 1 )
  tkgrid.rowconfigure( frame.header, 0, weight = 1)
  
  
  #columnconfig middle
  tkgrid.columnconfigure( frame.middle, 0, weight = 1 )
  tkgrid.columnconfigure( frame.middle, 1, weight = 1 )
  
  #rowconfig middle
  tkgrid.rowconfigure( frame.middle, 0, weight = 1)
  tkgrid.rowconfigure( frame.middle, 1, weight = 1)
  tkgrid.rowconfigure( frame.middle, 2, weight = 1)
  
  #config bottom
  tkgrid.columnconfigure( frame.bottom, 0, weight = 1 )
  tkgrid.rowconfigure( frame.bottom, 0, weight = 1)
  
  tkwait.window(parent.window)
  #tclvalue(selection)
}


general.settings.pickvalue <- function(parent.frame)
{
  selection <- tclVar(0.05)
  charalpha <- tclVar()
  
  valid_input <- function(...) 
  { 
    val <- as.numeric(tclvalue(charalpha)) 
    if(val <= 1 & val >= 0) 
    { 
      message("set to ", val) 
      tclvalue(selection) <- val 
      slot(NEW$Object.job.config,'pValThresh') = val
      return(TRUE)
    }
    
    else 
    { 
      message("not valid...") 
      return(FALSE)
    }
  }
  
  
  scale.ok.function <- function()
  {
    val <- as.numeric(tclvalue(charalpha))
    if (valid_input())
    {
      slot(NEW$Object.job.config,'pValThresh') = as.numeric(tclvalue(charalpha))
      tkdestroy(tt)
    }
  }
  
  scale.cancel.function <- function()
  {
    tkdestroy(tt)
  }
  
  charalpha <- tclVar(as.character(0.05)) 
  
  ed <- tkentry(parent.frame, textvariable=charalpha) 
  
  button.ok = ttkbutton(parent.frame, text = 'ok', command = scale.ok.function)
  button.cancel = ttkbutton(parent.frame, text = 'cancel', command = scale.cancel.function)
  
  tkgrid(ed, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid(button.ok, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(button.cancel, row = 1, column = 1, columnspan = 1, sticky = 'nsew')
  
  tkbind(ed, "<Return>", valid_input) 
  #linux for numpad enter... hope this doesn't cause trouble in windows...
  tkbind(ed, "<KP_Enter>", valid_input) 
  tkwait.window(tt)
}

general.setting.uproc <- function(parent.frame)
{
  function.select.file <- function()
  {
  ret <- tkgetOpenFile()
    if (basename(tclvalue(ret)) == 'uproc-dna' | basename(tclvalue(ret)) == 'uproc-dna.exe')
    {
      return(tclvalue(ret))
    }
  return(NULL)
  }
  
  function.select.folder <- function()
  {
  ret <- tkchooseDirectory()
  }
  
}