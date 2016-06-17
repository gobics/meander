COLOR_OPTIONS_BACKGROUND = '#204080';
COLOR_OPTIONS_FOREROUND = '#ffffff';

COLOR_LABEL_BACKGROUND = '#708090'
COLOR_LABEL_FOREGROUND = '#DCDCDC'



general.settings <- function(parent.window)
{
  dummy.selectfile <- function()
  {
    ret <- function.select.file()
    print(ret)
  }
  
  dummy.selectfolder <- function()
  {
    ret <- function.select.folder()
    print(ret)
  }
  
  
  
  
  dummy.uproc.setting <- function()
  {
    
  }
  
  dummy.meander.setting.consensus <- function()
  {
    tt2 <- tktoplevel()
    general.settings.consensus(tt2)
  }
  
  dummy.licver.function <- function()
  {
    edit.ConfigFile.Keys(SHOW_LICENCE_TEXT = tclvalue(license.selection))
    edit.ConfigFile.Keys(SHOW_VERSION = tclvalue(version.selection))
  }
  
  frame.header <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 2, width = 200, height = 100)
  frame.middle <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 10,  width = 200, height = 100)
  frame.bottom <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 2,  width = 200, height = 100)
  
  general.label = tklabel(frame.header, text = "General Settings" ,background ='#9080F0' ,foreground = '#0ffff0')
  tkgrid(general.label, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
  
  #UProC-TX
  general.UProCTX = tklabel(frame.middle, text = 'UProC-TX' ,background ='#9080F0' ,foreground = '#0ffff0')
  
  frame.uproc <- ttkframe(frame.middle, padding = c(1,2,4,8), borderwidth = 2,  width = 200, height = 100)
  
  
  #button
  pffff = ttkbutton(frame.uproc, text = 'persistent', command = ls)
  #label
  label.uproc = tklabel(frame.uproc, text = 'Set Options')
  
  
  #organize in grid
  tkgrid(general.UProCTX, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid(pffff, row = 1, column = 1, columnspan = 1, sticky = 'nsew')
  tkgrid(label.uproc, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
  
  #MeandeR
  general.MeandeR = tklabel(frame.middle, text = 'MeandeR' ,background ='#9080F0' ,foreground = '#0ffff0')  
  
  frame.MeandeR <- ttkframe(frame.middle, padding = c(1,2,4,8), borderwidth = 2,  width = 200, height = 100)
  general.settings.consensus(frame.MeandeR)
  
  
  general.pval = tklabel(frame.middle, text = 'p-value' ,background ='#9080F0' ,foreground = '#0ffff0')
  frame.pval <- ttkframe(frame.middle, padding = c(1,2,4,8), borderwidth = 2,  width = 200, height = 100)
  general.settings.pickvalue(frame.pval)
  
  general.space = tklabel(frame.middle, text = '')

  general.license = tklabel(frame.middle, text = 'show license', background = COLOR_LABEL_BACKGROUND, foreground = COLOR_LABEL_FOREGROUND)
  general.version = tklabel(frame.middle, text = 'show version', background = COLOR_LABEL_BACKGROUND, foreground = COLOR_LABEL_FOREGROUND)
  
  
  licver.selection.button <- ttkbutton(frame.middle, text = 'Set', command = dummy.licver.function)
  
  license.selection = tclVar(._CONFIG$SHOW_LICENCE_TEXT)
  version.selection = tclVar(._CONFIG$SHOW_VERSION)
  
  license.but = tkradiobutton(frame.middle, value = 0, text = 'no', variable = license.selection)  
  version.but = tkradiobutton(frame.middle, value = 0, text = 'no', variable = version.selection)  
  
  license.but2 = tkradiobutton(frame.middle, value = 1, text = 'yes', variable = license.selection)  
  version.but2 = tkradiobutton(frame.middle, value = 1, text = 'yes', variable = version.selection) 

  tkgrid(general.MeandeR, row = 10, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid(frame.MeandeR, row = 11, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid(general.pval, row = 12, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid(frame.pval, row = 13, column = 0, columnspan = 2, sticky = 'nsew')
  
  tkgrid(general.space, row = 14, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(general.license, row = 15, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(general.version, row = 16, column = 0, columnspan = 1, sticky = 'nsew')
  
  tkgrid(license.but, row = 15, column = 1, columnspan = 1, sticky = 'nsew')
  tkgrid(license.but2, row = 15, column = 2, columnspan = 1, sticky = 'nsew')
  
  tkgrid(version.but, row = 16, column = 1, columnspan = 1, sticky = 'nsew')
  tkgrid(version.but2, row = 16, column = 2, columnspan = 1, sticky = 'nsew')

  tkgrid(licver.selection.button, row = 17, column = 1, columnspan = 2, sticky = 'nsew')
  
  
  
    
  tkgrid(frame.header, row = 0, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(frame.middle, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(frame.bottom, row = 2, column = 0, columnspan = 1, sticky = 'nsew')
  
  tkgrid(frame.uproc, row = 3, column = 0, columnspan = 2, sticky = 'nsew')
  tkgrid.columnconfigure( frame.uproc, 0, weight = 1 )
  tkgrid.rowconfigure( frame.uproc, 0, weight = 1)
  
  
  
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
  tkgrid.rowconfigure( frame.middle, 13, weight = 1)
  #config bottom
  tkgrid.columnconfigure( frame.bottom, 0, weight = 1 )
  tkgrid.rowconfigure( frame.bottom, 0, weight = 1)
  
  tkgrid.columnconfigure( parent.window, 0, weight = 1 )
  
  
  
  
  
  tkgrid.rowconfigure( parent.window, 0, weight = 1)
  tkgrid.rowconfigure( parent.window, 1, weight = 1)
  tkgrid.rowconfigure( parent.window, 2, weight = 1)
  
  
  general.setting.uproc(frame.uproc)
}

general.settings.consensus <- function(parent.window)
{
  #set title of window
  #tkwm.title(parent.window, 'Consensus Settings')
  
  but.function.ok <- function()
  {
    edit.ConfigFile.Keys(CONSENSUS = tclvalue(selection))
  }
  
  selection <- tclVar(._CONFIG$CONSENSUS)
  
  frame.header <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 2, width = 200, height = 100)
  frame.middle <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 10,  width = 200, height = 100)
  frame.bottom <- ttkframe(parent.window, padding = c(1,2,4,8), borderwidth = 2,  width = 200, height = 100)
  
  general.label = tklabel(frame.header, text = "Consensus Settings" ,background = COLOR_OPTIONS_BACKGROUND ,foreground = COLOR_OPTIONS_FOREROUND)
  
  one.label = tklabel(frame.middle, text = "Single method" ,background = COLOR_LABEL_BACKGROUND ,foreground = COLOR_LABEL_FOREGROUND)
  conseneus.label = tklabel(frame.middle, text = "majority rule" ,background = COLOR_LABEL_BACKGROUND ,foreground = COLOR_LABEL_FOREGROUND)
  all.label = tklabel(frame.middle, text = "all methods agree" ,background = COLOR_LABEL_BACKGROUND ,foreground = COLOR_LABEL_FOREGROUND)
  
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
  
  tkgrid(method.selection.button.consensus, row = 2, column = 1, columnspan = 3)
  tkgrid(method.selection.button.all, row = 3, column = 1, columnspan = 3)
  
  # buttons
  button.ok = ttkbutton(frame.bottom, text = 'Set Value', command = but.function.ok)
  
  tkgrid(button.ok, row = 0, column = 0, columnspan = 4, sticky = 'nsew')
  
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
  
  #tkwait.variable(ret.Val)
  #tclvalue(selection)
}

general.settings.pickvalue <- function(parent.frame)
{
  selection <- tclVar(slot(NEW$Object.job.config,'pValThresh'))
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
    }
  }
  
  
  charalpha <- tclVar(as.character(slot(NEW$Object.job.config,'pValThresh'))) 
  
  ed <- tkentry(parent.frame, textvariable=charalpha) 
  
  button.ok = ttkbutton(parent.frame, text = 'ok', command = scale.ok.function)
  
  tkgrid(ed, row = 0, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(button.ok, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
  
  tkbind(ed, "<Return>", valid_input) 
  #linux for numpad enter... hope this doesn't cause trouble in windows...
  tkbind(ed, "<KP_Enter>", valid_input) 
  #tkwait.window(tt)
}

OK.FUNCTION <- function(envi)
{
  if (! uproc.working.check())
  {
    edit.ConfigFile.Keys(UPROC_DIR = NULL)
    edit.ConfigFile.Keys(MODEL_DIR = NULL)
    edit.ConfigFile.Keys(UPROC_DB = NULL)
  }
  envi$tcltkval <- tclVar('OK')
}

CANCEL.FUNCTION <- function(envi)
{
  envi$tcltkval <- tclVar('CANCEL')
}


general.setting.uproc <- function(parent.frame)
{
  env.general.setting.uproc <- environment()
  
  dummy.selectfile <- function()
  {
  ret <- function.select.file()
    if (!is.null(ret))
    {
      edit.ConfigFile.Keys(UPROC_DIR = ret)
    }
  }
  
  dummy.selectfolder.db <- function()
  {
    ret <- function.select.folder()
      if (!is.null(ret))
      {
        edit.ConfigFile.Keys(UPROC_DB = ret)
      }
  }
  
  dummy.selectfolder.model <- function()
  {
    ret <- function.select.folder()
      if (!is.null(ret))
      {
        edit.ConfigFile.Keys(MODEL_DIR = ret)
      }
  }
  
  function.select.file <- function()
  {
  ret <- tkgetOpenFile()
    if (basename(tclvalue(ret)) == 'uproc-dna' | basename(tclvalue(ret)) == 'uproc-dna.exe')
    {
      if (check.uprocversion(paste0(tclvalue(ret),' -v')))
      {
        return(tclvalue(ret))
      }
    }
  tkmessageBox(title = "UProC-TX Settings", message = "correct file >> uproc-dna[.exe] << selected?", icon = "warning", type = "ok")
  return(NULL)
  }
  
  function.select.folder <- function()
  {
  ret <- tkchooseDirectory()
    if (tclvalue(ret) == '')
    {
    return(NULL)  
    }
  return(tclvalue(ret))    
  }
  
  function.uprocworks <- function()
  {
    if (! uproc.working.check())
    {
      edit.ConfigFile.Keys(UPROC_DIR = NULL)
      edit.ConfigFile.Keys(MODEL_DIR = NULL)
      edit.ConfigFile.Keys(UPROC_DB = NULL)
      tkmessageBox(title = "UProC-TX Settings", message = "UProC-TX is not set up correctly.", icon = "error", type = "ok")
      
    }
    
    else
    {
      tkmessageBox(title = "UProC-TX Settings", message = "UProC-TX seems to be configured correctly.", icon = "info", type = "ok")
    }
  }


  
  
  button.factory(c('select executable','select folder', 'select folder', 'run test'),c('UProC-dna[.exe]','UProC-TX model', 'UProC-TX db', 'check UProC-TX configuration'),list(dummy.selectfile,dummy.selectfolder.model,dummy.selectfolder.db,function.uprocworks),parent.frame)
  
  tcltkval = tclVar()
  
  
  
  #
  
  #tkwait.variable(tcltkval)
}


button.factory <- function(button.names,label.names,button.functions,parent.frame,envir)
{
 if (length(button.names) == length(label.names) & length(button.names) == length(button.functions))
 {
  nPos = length(button.names)
   #make labels
  labels <- list()
    for (i in 1:nPos)
    {
      labels[[i]] = tklabel(parent.frame, text = label.names[i])
    }
  
    #make buttons
  buttons = list()
  for (i in 1:nPos)
  {
    buttons[[i]] = ttkbutton(parent.frame, text = button.names[i], command = button.functions[[i]])
  }
  
    #arrange it
  for (i in 1:nPos)
  {
    tkgrid(labels[[i]], row = i, column = 0, columnspan = 1, sticky = 'nsew')
    tkgrid(buttons[[i]], row = i, column = 1, columnspan = 1, sticky = 'nsew')
  }
  
  ok.but <- ttkbutton(parent.frame, text = 'OK', command = button.functions[[i]])
  cancel.but <-
  
  return(TRUE)
 }
  
  else
  {
    print('RIP')
    return(NULL)
  }
}

if (TRUE == FALSE)
{
#permanent
SHOW_LICENCE_TEXT
SHOW_VERSION
#non permanent
p-value
}