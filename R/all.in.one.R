#ADDITIONS

load.kegg.object.parts <- function()
{

  #prepare basepath

  #load X

  #load y

return(Object.Part.Kegg)
}

resume.object <- function(object.location)
{

  Complete.Object <- NULL
  #load object

  #set buttons according

return(Complete.Object)
}

resume.object.buttonsetup <- function(Button.Settings,Full.Object)
{
#MeandR object will always start after the Score Selection step.

  #check if entries are availible
  ret <- check.states.object(Full.Object)
    if (!is.null(ret))
    {
      Button.Settings <- ret
    }
  #set button to desired states


  #return Object in parts
  return(list(Button.Settings,Full.Object))
}

check.states.object <- function()
{

}

#Find out if RNA is something...
#Vec.List <- our 16 and 28 rrna
detect.RNA <- function(Ret,Vec.List,fraglength = 300,nStepsize = 100, repetitions = 1,MinVec = c(2,2), MaxVec = c(5,7))
{

  QQQQ <- c()

    nRet.seqlength = width(Ret)
    nRet.length = length(Ret)

  minMax.LengthVec <- seq(min(nRet.seqlength),max(nRet.seqlength)+nStepsize,100)
  IOrder <- order(nRet.seqlength)

  reordered.Seqlength <- nRet.seqlength[IOrder]

  TransLateVec = c();
  TransLateVec[as.integer(charToRaw('aAcCtTgGuU'))] = c(0,0,1,1,2,2,3,3,2,2);

  AllRNA <- vector(mode = 'numeric', length = nRet.length)
  AllScores.List <- list();

  for (k in 1:length(MinVec))
  {

    AllScores <- c()

      for (i in 1:(length(minMax.LengthVec)-1))
      {

    if (is.null(fraglength))
    {
      V <- TransLateVec[as.integer(charToRaw(paste0(as.character(Ret),collapse='')))]

      tmp.SeqLength = nRet.seqlength
      test.start = c(1,cumsum(tmp.SeqLength)+1)
      test.start <- test.start[1:(length(test.start)-1)]
      test.end <- cumsum(tmp.SeqLength)

      Scores <- .Call("rlsq_classify", PACKAGE = 'RLSQ', as.integer(V), test.start-1, test.end-1, as.integer(tmp.SeqLength), Vec.List[[k]][1:(length(Vec.List[[k]])-1)], Vec.List[[k]][length(Vec.List[[k]])], MinVec[k], MaxVec[k])
      AllScores <- c(AllScores,Scores)
      AllRNA = AllRNA + (Scores > 0)
    }

    else
    {

        if (fraglength > minMax.LengthVec[i])
        {
          fraglength = minMax.LengthVec[i]-1
        }

        Ind <- reordered.Seqlength >= minMax.LengthVec[i] & reordered.Seqlength < minMax.LengthVec[i+1]
        Ord.Ind <- IOrder[Ind]
        RNA.Vec <- vector(mode = 'numeric', length = sum(Ind))
  if (sum(Ind) > 0)
  {
        for (j in 1:repetitions)
        {
          y = sample(minMax.LengthVec[i]-fraglength,1)
          cat(fraglength,'\t',y,'\n')
          Ret.Part <- subseq(Ret[Ord.Ind],y,y+fraglength-1)
          V <- TransLateVec[as.integer(charToRaw(paste0(as.character(Ret.Part),collapse='')))]

                    nFrags = sum(Ind);
          tmp.SeqLength = rep(fraglength,nFrags)
          test.start = c(1,cumsum(tmp.SeqLength)+1)
          test.start <- test.start[1:(length(test.start)-1)]
          test.end <- cumsum(tmp.SeqLength)
          Scores <- .Call("rlsq_classify", PACKAGE = 'RLSQ', as.integer(V), test.start-1, test.end-1, as.integer(tmp.SeqLength), Vec.List[[k]][1:(length(Vec.List[[k]])-1)], Vec.List[[k]][length(Vec.List[[k]])], MinVec[k], MaxVec[k])
          AllScores <- c(AllScores,Scores)
          IX <- which(Scores > 0)
          AllRNA[Ord.Ind[IX]] = AllRNA[Ord.Ind[IX]] + 1
          QQQQ <- c(QQQQ,Ord.Ind)
        }
  }

    }
    }

    AllScores.List[[k]] <-  AllScores
  }
  return(list(AllScores.List,AllRNA,QQQQ))
}



rRNA.object <- setClass(
  "rRNA.object",

  # Define the slots
  slots = c(
    r16s = "list",
    r23s = "list",
    length = 'vector'
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    r16s = list(),
    r23s = list(),
    length = vector()
  )
)
#initialize and do random stuff
Obj <- rRNA.object()
Obj@r16s <- list(sample(1000,100), sample(1000,100), sample(1000,100))
Obj@r23s <- list(sample(1000,50), sample(1000,50), sample(1000,50))
Obj@length <- c(100,500,1000)

detect.RNA.multi <- function(Sequence.Object,rRNA.object, repetitions = 1)
{
  #find my max length
  nLength.Vec <- slot(rRNA.object,'length')
  rRNA.16s <- slot(rRNA.object,'r16s')
  rRNA.23s <- slot(rRNA.object,'r23s')

    if (length(rRNA.16s) != length(nLength.Vec) | length(rRNA.23s) != length(nLength.Vec))
    {
      return(NULL)
    }

  nRet.seqlength = width(Sequence.Object)
  nRet.length = length(Sequence.Object)


  IOrder <- order(nRet.seqlength)

  reordered.Seqlength <- nRet.seqlength[IOrder]

  TransLateVec = c();
  TransLateVec[as.integer(charToRaw('aAcCtTgGuU'))] = c(0,0,1,1,2,2,3,3,2,2);

  AllRNA <- vector(mode = 'numeric', length = nRet.length)
  AllScores.List <- list();

  A <- sapply(c(1:(length(nLength.Vec)-1)), function(x) mean(nLength.Vec[c(x,x+1)]))
  A <- c(A,nLength.Vec[length(nLength.Vec)])
return(A)
}

#Button function\
process.taxonomy.dummy <- function()
{
slot(NEW$Object.job.config,'SelectedTax') <- as.numeric(tax.Select(obj.data= NEW$Object.data.big, obj.refined= NEW$Object.data.refined, obj.config= NEW$Object.job.config))
}

input.fasta.dummy <- function()
{

#set input files
Ret <- select.multiple.files('FASTA')
slot(NEW$Object.job.path,FILETYPE.DNA) <- Ret
  .ret <- start.DNA(Object.job.path = NEW$Object.job.path, Object.data.big = NEW$Object.data.big, Object.job.statistics = NEW$Object.job.statistics, object.save.FLAG = FALSE)
  NEW$Object.job.path <- .ret[[1]]
  NEW$Object.data.big <- .ret[[2]]
  NEW$Object.job.statistics <- .ret[[3]]
  print(.ret[[4]])
input.fastanorrna.dummy()
input.uproc.dummy()
#mark this rRNA


}


input.fastanorrna.dummy <- function()
{

#run UProC
    cat('STEP2\n')
    if (length(slot(NEW$Object.job.path,FILETYPE.DNAwoRNA)) == 0)
    {
    Ret <- select.multiple.files('FASTA')
    slot(NEW$Object.job.path,FILETYPE.DNAwoRNA) <- Ret
    }

  .ret <- start.DNAnoRNA(Object.job.path = NEW$Object.job.path,TRUE)
  NEW$Object.job.path <- .ret[[1]]
  print(.ret[[2]])
  input.uproc.dummy()
}

input.uproc.dummy <- function()
{
    if (length(slot(NEW$Object.job.path,FILETYPE.UproC)) == 0)
    {
    Ret <- select.multiple.files('UProC')
    slot(NEW$Object.job.path,FILETYPE.UproC) <- Ret
    }

  .ret <- start.UProC(NEW$Object.job.path,NEW$Object.job.statistics,NEW$Object.data.big,NEW$Object.data.dataframes)
  NEW$Object.job.path = .ret[[1]];    NEW$Object.job.statistics = .ret[[2]];    NEW$Object.data.big = .ret[[3]];    NEW$Object.data.dataframes = .ret[[4]]
}

process.score.dummy <-function()
{
  .ret <- change.uprocscorethreshold(NEW$Object.job.statistics,NEW$Object.data.dataframes)
  NEW$Object.job.statistics <- .ret[[1]]

  .ret <- start.RDS(Object.data.big = NEW$Object.data.big, Object.job.path = NEW$Object.job.path, Object.data.kegg = NEW$Object.data.kegg, Object.job.statistics = NEW$Object.job.statistics, Object.data.refined =  NEW$Object.data.refined, object.save.FLAG = FALSE)
  NEW$Object.data.big <- .ret[[2]];  NEW$Object.job.statistics <- .ret[[1]];  NEW$Object.data.refined <- .ret[[3]]
}


analyse.methods.dummy <- function()
{
  .Ret <- create.matrix(Object.DATA.BIG = NEW$Object.data.big,Object.Job.Config = NEW$Object.job.config)

  NEW$Object.data.refined <- setInputdata(NEW$Object.data.refined,'Matrix',.Ret[[1]])
  NEW$Object.data.refined <- setInputdata(NEW$Object.data.refined,'Matrix.label',.Ret[[2]])
  NEW$Object.data.big <- setInputdata(NEW$Object.data.big,'Matrix',.Ret[[1]])

#consensus methods
  set.seed(12345)
  .X <- start.consensus(NEW$Object.data.big, NEW$Object.job.config)
  NEW$Object.data.refined <- setInputdata(NEW$Object.data.refined,'ConsensusMat',.X)
  .ret <- perform.consensusselecion(Type = 'Consensus', O.Job.Config = NEW$Object.job.config, O.DATA.Refined = NEW$Object.data.refined)
  NEW$Object.data.refined <- .ret
  NEW$Object.data.refined <- prepare.svgvectors.colour(NEW$Object.data.refined,NEW$Object.data.kegg,NEW$Object.job.config)
}

analyse.venn.dummy <- function()
{
xxx <- plot.generate.vennreplacement(Method.Vec = slot(NEW$Object.job.config,'Methods'), Mat.pVal = NEW$Object.data.refined@ConsensusMat, threshold = 0.05)
}

analyse.br.dummy <- function()
{
df <- ko2br.path.counts(NEW$Object.job.config,NEW$Object.data.kegg,NEW$Object.data.refined)
df2 <- data.frame(Counts = c(df$SigOver,df$SigUnder), x = rep(df$Name,2), y = c(df$SigOver/df$TotalCounts,df$SigUnder/df$TotalCounts), z = c(rep('SigOver',length(df$SigOver)),rep('SigUnder',length(df$SigOver))))
positions <- df[order((df$SignificantCounts/df$TotalCounts) * df$SignificantCounts, decreasing=FALSE),'Name']
print(ggplot(df2, aes(x = x, y = y, color = factor(z), group = factor(z), fill = Counts)) + scale_x_discrete(limits = positions) + geom_bar(position = "dodge",stat="identity") + coord_flip() + scale_fill_gradient(low = "lightblue", high = "darkblue") +
ylab('fraction significant') +
xlab('kegg br categories'))

}


analyse.pca.dummy <- function()
{
df <- plot.pca(NEW$Object.job.config, NEW$Object.job.statistics,NEW$Object.data.big, minCount = 5)
}



analyse.pathway.dummy <- function()
{
#create svg
  NEW$REEEEED <- perform.pathwaydetection(NEW$Object.job.config,NEW$Object.data.kegg,NEW$Object.data.refined)

  NEW$Object.data.refined <- prepare.svgvectors.colour(NEW$Object.data.refined,NEW$Object.data.kegg,NEW$Object.job.config)
  NEW$.df <- perform.SVGcreation(NEW$Object.data.refined,NEW$Object.job.path,NEW$Object.job.config)

  .path = file.path(slot(NEW$Object.job.path,'DirOut'),'HTML',slot(NEW$Object.job.config, 'SelectedTax'))

  sebastian.rekt.names <- c('# functions annotated','tax-specific','covered','significant','p.value')
  sebastian.rekt.df <- data.frame(NEW$REEEEED[[5]], NEW$REEEEED[[6]], NEW$REEEEED[[4]]/NEW$REEEEED[[6]], NEW$REEEEED[[3]], NEW$REEEEED[[2]])

  colnames(sebastian.rekt.df) <- sebastian.rekt.names
  PWTH=data.frame(string=sebastian.rekt.names, type=c('i','i','r','i','f'), stringsAsFactors=FALSE)



  write.html.files(sebastian.rekt.df,PWTH,readRDS('/home/hklingen/projects/meander/data/keggmapnames.rds'),readRDS('/home/hklingen/projects/meander/data/pathway.names.Rds'),readRDS('/home/hklingen/projects/meander/data/ko_desc.rds'),slot(NEW$Object.data.refined,'FlagVec'),perform.pvalcalc(NEW$Object.data.refined),slot(NEW$Object.data.kegg,'KEGG2PATH'),.path)

#create HTML

}

output.svghtml.dummy <- function()
{
  .path = file.path(slot(NEW$Object.job.path,'DirOut'),'HTML',slot(NEW$Object.job.config, 'SelectedTax'),'RESULTS.html')


browseURL(.path, browser = getOption("browser"),encodeIfNeeded = FALSE)
}

button.dummy <- function()
{
emptyfunction();
}


button.dummy.input.fasta<- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.input.fastanorrna <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.input.fastanorrna,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}


button.dummy.input.uproc <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.input.uproc,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.input.object <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.input.object,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.process.output <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.process.output,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.process.category <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.process.category,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.process.conditions <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.process.conditions,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.process.score <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.process.score,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.process.taxonomy <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.process.taxonomy,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.analyse.methods <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.analyse.methods,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.analyse.pca <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.analyse.pca,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.analyse.br <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.analyse.br,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}


button.dummy.analyse.venn <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.analyse.venn,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.analyse.pathway <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.analyse.pathway,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}



button.dummy.output.svghtml <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.output.svghtml,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.output.csv <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.output.csv,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}

button.dummy.output.figures <- function()
{
BUTTONS.ON.OFF(NEW$Container.Object.Button@button.output.figures,NEW,NULL,NULL)
#lock.unlock.buttons(NEW$Container.Object.Button@button.input.fasta,NEW,NULL,NULL)
}



window.next.to.parent <- function(window.parent,window.child)
{
tcl('update')
#calculate parent
tmp.String <- tclvalue(tkwinfo('geometry', window.parent))
dim.vec.one <- as.numeric(unlist(regmatches(tmp.String,gregexpr('[0-9]+', tmp.String))))
#<Tcl> 200x200+825+361

tmp.String <- tclvalue(tkwm.geometry(window.parent))
dim.vec.two <- as.numeric(unlist(regmatches(tmp.String,gregexpr('[0-9]+', tmp.String))))
#<Tcl> 200x200+821+336

yMove = dim.vec.two[3]+dim.vec.two[1]+(2*(dim.vec.one[3]-dim.vec.two[3]))
xMove = dim.vec.two[4]


#calculate child, only require window size so the first two.
tmp.String <- tclvalue(tkwm.geometry(window.child))
dim.vec.child <- as.numeric(unlist(regmatches(tmp.String,gregexpr('[0-9]+', tmp.String))))

tkwm.geometry(window.child, paste0(dim.vec.child[1],'x',dim.vec.child[2],'+',yMove,'+',xMove))
}


Message.waiting <-function(frame.window,function.to.execute,Message,Environment)
{
E.part <- environment()
  encapsulate.function <- function()
  {
  tkconfigure(button.message.run, state = 'disabled')
  function.to.execute()
  tkconfigure(button.message.ok, state = 'enabled')
  }

  button.ok <- function()
  {
  return(tclvalue(tcltk.variable) <- 'OK')
  }

  button.cancel <- function()
  {
  return(tclvalue(tcltk.variable) <- 'Cancel')
  }

  button.func.ok <- function()
  {
  tcltk.variable <- button.ok()
  tkdestroy(frame.window)
  }

  button.func.cancel <- function()
  {
  tcltk.variable <- button.cancel()
  tkdestroy(frame.window)
  }

tcltk.variable = tclVar(-1)

frame.buttons <- ttkframe(frame.window, padding = c(1,2,4,8), borderwidth = 10, relief = 'ridge', width = 100, height = 100)

# Label
label.message = tklabel(frame.buttons, text = Message ,background ='#9080F0' ,foreground = '#0ffff0')
# Sep
sep.horizontal = ttkseparator(frame.buttons, orient= 'horizontal')
# Buttons
button.message.run = ttkbutton(frame.buttons, text = 'run', command = encapsulate.function)
button.message.ok = ttkbutton(frame.buttons, text = 'OK', command = button.func.ok)
button.message.cancel = ttkbutton(frame.buttons, text = 'Cancel', command = button.func.cancel)

tkgrid(label.message, row = 0, column = 0, columnspan = 3, sticky = 'nsew')
tkgrid(sep.horizontal, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(button.message.run, row = 2, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(button.message.ok, row = 2, column = 1, columnspan = 1, sticky = 'nsew')
tkgrid(button.message.cancel, row = 2, column = 2, columnspan = 1, sticky = 'nsew')





tkpack(frame.buttons, expand = 1, fill = 'both')
tkconfigure(button.message.ok, state = 'disabled')


window.next.to.parent(Environment$ttMain,frame.window)

tkwait.window(frame.window)


  if (tclvalue(tcltk.variable) == 'OK')
  {
  print("noice!")
  }

  else
  {
  print("shice!")
  }

return(tclvalue(tcltk.variable))
}

#core
  #object
  .BadPractice <- setClass(
    #Name
    "BadPractice",
    #Slots for Folder & Files (Classes, too)
    slots = c(
      selectedTax = 'numeric',
      output = 'character',
      Button1 = 'logical',
      Button2 = 'logical',
      Button3 = 'logical',
      Button4 = 'logical',
      Button5 = 'logical',
      ButtonVec = 'vector'
    ),
    # default = empty
    prototype=list(
      selectedTax = -1,
      output = '',
      Button1 = FALSE,
      Button2 = FALSE,
      Button3 = FALSE,
      Button4 = FALSE,
      Button5 = FALSE,
      ButtonVec = rep(FALSE,5)
    )
  )

split.object <- function(Object)
{
A <- slot(slot(Object,'DATA'),'Refined')
B <- slot(slot(Object,'DATA'),'BIG')
C <- slot(slot(Object,'DATA'),'KEGG')
D <- slot(slot(Object,'Job'),'Paths')
return(list(A,B,C,D))
}



Name.Global.Object <- function()
{

  Obj.Bad.Practice <<- .BadPractice()
  QQ2 <- .BadPractice()
  return(QQ2)
}



NAME.interactive.Output <- function()
{
  #select output folder
  .Val <- tkchooseDirectory(initialdir = './')
  return(tclvalue(.Val))
}


NAME.interactive.PCA <- function()
{

}


NAME.interactive.Taxa <- function(Object,Window)
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


  AllowedSamples = which(Object@Results@SelectedSamples)
  #use only Samples in selected Samples
  .DT <- Object@Results@QuickDataTable[Sample %in% AllowedSamples]
  #reduce vector to olny used samples
  .ClassVec <- Object@Input@ClassVec[AllowedSamples]
  #create 2 Classes
  .uClassVec <- unique(.ClassVec)
  .CondA.I <- .ClassVec %in% .uClassVec[1]
  .CondB.I <- .ClassVec %in% .uClassVec[2]

  .lowestTaxa = unique(.DT[Rank == 1,TaxID])

  #set TaqxID to Root
  .Curr.Tax = -1

  #get assigned Counts at top level
  .XVec.ROOT <- colSums(.generate.mat(.DT, .Curr.Tax))# in .Xmat.Root[1,] max possible counts


  .done = 0
  x11()
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

    .Ret <- dostuff(Window,as.list(rownames(myMat)),as.list(1:length(rownames(myMat))),as.list(myMat[,2]),'select TaxID')

    if (.Ret[1] == 'OK')
    {
      slot(Obj.Bad.Practice,'selectedTax') = as.numeric(rownames(myMat)[.Ret[[2]]])
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

  Obj2 <- NAME.addData(Object = Object, LEVEL1 = 'Parameter', LEVEL2 = 'R', LEVEL3 = 'SelectedTaxa', value = slot(Obj.Bad.Practice,'selectedTax'))

  return(Obj2)
}

NAME.interactive.MainWindow <- function()
{

}


NAME.Button.set <- function(ButtonList,ButtonNameList)
{
  nLength = length(ButtonList)
  if (nLength != length(ButtonNameList))
  {
    return(FALSE)
  }

  else
  {
    for (i in 1:nLength)
    {
      if (slot(Obj.Bad.Practice,ButtonNameList[[i]]))
      {
        tkconfigure(widget = ButtonList[[i]], state = 'normal')
      }

      else
      {
        tkconfigure(widget = ButtonList[[i]], state = 'disabled')
      }
    }
  }

}

makepresser=function(ButtonWid,ButtonName)
{
  force(ButtonWid);
  force(ButtonName);
  function()
  {
    NAME.Button.set(ButtonWid,ButtonName)
  }


}



NAME.interactive.tcltk2 <- function(Object)
{
  MY.SCOPE = environment()

List.Object.Parts <- split.object(Object)

#A <- slot(slot(Object,'DATA'),'Refined')
#B <- slot(slot(Object,'DATA'),'BIG')
#C <- slot(slot(Object,'DATA'),'KEGG')
#D <- slot(slot(Object,'Job'),'Paths')



  But.func <- function()
  {
    .Ret <- NAME.interactive.Output()
    if (.Ret != '')
    {
      print(.Ret)
      #MY.SCOPE$Object <- NAME.addData(Object = Object, LEVEL1 = 'Results', LEVEL2 = 'HTML', value = .Ret)
      slot(List.Object.Parts[[4]],'DirOut') <- .Ret
      slot(Obj.Bad.Practice,'Button1') <<- TRUE
      #set big Object
    }

    else
    {
      slot(Obj.Bad.Practice,'Button1') <<- FALSE
    }

    NAME.Button.set(ButtonList.Widget,ButtonList.Names)
  }

  But.func2 <- function()
  {
    MY.SCOPE$Object <- NAME.interactive.Taxa(Object,ttMain)
    slot(Obj.Bad.Practice,'Button2') <<- TRUE
    NAME.Button.set(ButtonList.Widget,ButtonList.Names)
  }

  But.func3 <- function()
  {

    MY.SCOPE$Object <- NAME.make.pca2(Object)
    slot(Obj.Bad.Practice,'Button3') <<- TRUE
    NAME.Button.set(ButtonList.Widget,ButtonList.Names)
    plot(Object@Results@PCA$x[,1],Object@Results@PCA$x[,2])
  }


  But.func4 <- function()
  {
    MY.SCOPE$Object <- NAME.perform.multianalysis(Object)
    MY.SCOPE$Object <- NAME.select.Consensus(Object, 'Consensus')
    MY.SCOPE$Object <- NAME.perform.pathwaydetection(Object)
    slot(Obj.Bad.Practice,'Button4') <<- TRUE
    NAME.Button.set(ButtonList.Widget,ButtonList.Names)
  }

  But.func5 <- function()
  {
    print(NAME.getData(Object = Object, LEVEL1 = 'Results', LEVEL2 = 'HTML'))
    NAME.perform.HTMLcreation(Object)
    NAME.perform.SVGcreation(Object)
  }

  But.Restart <- function()
  {
    Name.Global.Object()
    NAME.Button.set(ButtonList.Widget,ButtonList.Names)
  }

  But.Quit <- function()
  {
    Obj <<- Object
    tkdestroy(ttMain)
  }




  ttMain <- tktoplevel()
  tktitle(ttMain) <- "ttMain"
  #get info of window pos
  tkwm.geometry(ttMain)
  #change window pos
  tkwm.geometry(ttMain, "500x500+1000+500")


  A.button <- tkbutton(ttMain, height = 2, width = 20, text = "Set Output Dir", command = But.func)
  B.button <- tkbutton(ttMain, height = 2, width = 20, text = "Select Taxonomy", command = But.func2)
  C.button <- tkbutton(ttMain, height = 2, width = 20, text = "Perform PCA", command = But.func3)
  D.button <- tkbutton(ttMain, height = 2, width = 20, text = "run methods", command = But.func4)
  E.button <- tkbutton(ttMain, height = 2, width = 20, text = "create output", command = But.func5)


  ButtonList.Widget <- list(B.button,C.button,D.button,E.button)
  ButtonList.Names <- list('Button1','Button2','Button3','Button4')


  Restart.button <- tkbutton(ttMain, height = 2, text = "Restart", command = But.Restart)
  Quit.button <- tkbutton(ttMain, height = 2, text = "Quit", command = But.Quit)



  tkconfigure(widget = B.button, state = 'disabled')
  tkconfigure(widget = C.button, state = 'disabled')
  tkconfigure(widget = D.button, state = 'disabled')
  tkconfigure(widget = E.button, state = 'disabled')





  tkgrid(A.button,B.button)
  tkgrid(C.button,D.button)
  tkgrid(E.button)
  tkgrid(Restart.button,Quit.button)
}

#functions
tcltk.listbox <- function(parent.window = NULL,child.window = NULL, question = 'how much is the fish?', selection = c('1 Euro', '2 Euros', 'free'), preselect = NULL)
{
xMax = 60;
  if (max(nchar(selection)) < xMax)
  {
  xMax <- max(nchar(selection))
  }

yMax = 10;
  if (length(selection) < yMax)
  {
  yMax <- length(selection)
  }



Env <- environment()
Env$Choice <- '16'
VAR <- tclVar(-1)
  if (is.null(child.window))
  {
    if (is.null(parent.window))
    {
    child.window <- tktoplevel()
    }

    else
    {
    child.window <- tktoplevel(parent.window)
    }
  }

    scroll.Bar = ttkscrollbar(
        child.window,
        orient = 'vertical',
        command = function( ... ) tkyview( child.window$env$lst, ... )
        )

    scroll.Bar.h = ttkscrollbar(
        child.window,
        orient = 'horizontal',
        command = function( ... ) tkxview( child.window$env$lst, ... )
        )


child.window$env$lst <- tklistbox(child.window, height = 5, selectmode = "multiple", yscrollcommand = function( ... ) tkset( scroll.Bar, ... ), xscrollcommand = function( ... ) tkset( scroll.Bar.h, ... ), width = xMax, height = yMax)
tkgrid(tklabel(child.window, text = question, justify = "left"),
  padx = 10, pady =c(15, 5), sticky = "w")
tkgrid(child.window$env$lst, padx = 10, pady = c(5, 10))
#put in the list elements
  for (element in selection)
  {
  tkinsert(child.window$env$lst, "end", element)
  }

#if preselection is availible, set it
  if (!is.null(preselect))
  {
    for (i in 1:length(preselect))
    {
    tkselection.set(child.window$env$lst, preselect[i])
    }
  }


  onOK.dummy <- function()
  {
  Env$Choice <- onOK()
  tclvalue(VAR) = 2;
  }

  onOK <- function()
  {
  Choice = selection[as.numeric(tkcurselection(child.window$env$lst)) + 1]
  #msg <- paste0("Good choice! ", Choice)
  #tkmessageBox(message = msg)
  return(Choice)

  }

tkgrid( child.window$env$lst, sticky = 'nsew' )
tkgrid( scroll.Bar, row = 1, column = 1, sticky = 'nsew' )
tkgrid( scroll.Bar.h, row = 2, column = 0, sticky = 'nsew' )

child.window$env$butOK <-tkbutton(child.window, text = "OK", width = -6, command = onOK.dummy)
tkgrid(child.window$env$butOK, padx = 10, pady = c(5, 15))


  if (!is.null(parent.window))
  {
  window.next.to.parent(parent.window,child.window)
  }

tkwait.variable(VAR)
print(Env$Choice)
tkdestroy(child.window)
return(Env$Choice)
}




tcltk.Message.Box <- function(Message = 'no message')
{
return(tclvalue(tkmessageBox(message = Message, icon = "question", type = "yesno", default = "yes")))
}

tcltk.input.box <- function(child.window = NULL,boxname = 'bin', labelname = 'empty', toplabel = NULL, entryval = NULL)
{
done <- tclVar(0)

enV <- environment()
  if (is.null(child.window))
  {
  tt <- tktoplevel(NEW$ttMain)
  }

  else
  {
  tt <- child.window
  }

tkwm.title(tt, boxname)
entries <- list()
tclvars <- list()
result <- NULL

i = 1
tclvars[[i]] <- tclVar("")
  if (!is.null(entryval))
  {
  tclvars[[i]] <- tclVar(entryval)
  }


entries[[i]] <- tkentry(tt, textvariable=tclvars[[i]])



  if(!is.null(toplabel))
  {
  tkgrid(tklabel(tt, text=toplabel), pady=10, padx=10, columnspan=2)
  }

tkgrid(tklabel(tt, text=labelname), entries[[i]], pady=10, padx=10, columnspan=1)



  cancel <- function()
  {
    tclvalue(done) <- 2
  }
cancel.but <- tkbutton(tt, text='Done', command=cancel)

  submit <- function()
  {
      tryCatch( {
	enV$result <- tclvalue(tclvars[[i]])
	tclvalue(done) <- 1
      },
      error = function(e) { tkmessageBox(message=geterrmessage()) },
      finally = { }
      )
  }
submit.but <- tkbutton(tt, text="Submit", command=submit)


tkgrid(submit.but, cancel.but, pady=10, padx=10, columnspan=2)
window.next.to.parent(NEW$ttMain,tt)
tkfocus(tt)



tkwait.variable(done)

  if(tclvalue(done) != 1)
  {
    result <- NULL
  }

  if (is.null(child.window))
  {
  tkdestroy(tt)
  }
return(result)
}



tcltk.multiple.input.boxes <- function()
{
staph <- 'Q'
iter = 1
Vector = c();

nClasses <- length(slot(NEW$Object.job.config,'ClassNames'))
print(nClasses)
    if (nClasses > 0)
    {
      for (i in 1:nClasses)
      {
      Vector[i] <- slot(NEW$Object.job.config,'ClassNames')[i]
      }

    }


    while(!is.null(staph))
    {
    #tt.child <- tktoplevel(NEW$ttMain)
    #check if object exists with entries...


    #work accordingly
      if (nClasses > 0 & nClasses >= iter)
      {
      staph <- tcltk.input.box(boxname = 'bin', labelname = 'Condition name: ', toplabel = paste0('Please enter name to add category number ',iter, ' and press "Submit" and "Done" with an empty entry to finish input.' ), entryval = Vector[iter])
      Vector[iter] <- paste0(sample(letters, 10, replace = TRUE), collapse='')
      print(staph)
      }


      else
      {
      staph <- tcltk.input.box(boxname = 'bin', labelname = 'Condition name: ', toplabel = paste0('Please enter name to add category number ',iter, ' and press "Submit" and "Done" with an empty entry to finish input.' ))
      print(staph)
      }
    #tkwait.window(tt.child)
      if (!is.null(staph))
      {
	if (staph %in% Vector)
	{
	#name already exists...
	print(Vector)
	staph = NULL
	}

	else
	{
	Vector[iter] <- staph
	iter = iter + 1
	}
      }
    }
#trim vector if less elements are chosen in a later step
Vector <- Vector[1:(iter-1)]
return(Vector)
}

tcltk.assign.files2category <- function(listoffilenames,listofcategories)
{

nFiles = length(listoffilenames)
nCategories = length(listofcategories)


Vector = vector(mode = 'numeric', length = nFiles)
not.done = TRUE
listoffilenames.copy = listoffilenames;
  while (not.done)
  {
    for (i in 1:nCategories)
    {
    ret <- c()
      if (length(listoffilenames.copy) > 0)
      {
      child.window <- tktoplevel(NEW$ttMain)
      ret <- tcltk.listbox(child.window = child.window,parent.window = NEW$ttMain, question = paste0('Please select all files for the condition: ',listofcategories[i]), selection = listoffilenames.copy)
      }
	if (length(ret) == 0)
	{
	break;
	}

    Idx <- listoffilenames.copy %in% ret
    listoffilenames.copy <- listoffilenames.copy[Idx == FALSE]


    Idx <- listoffilenames %in% ret
    Vector[Idx] = i
    }
    if ((length(listoffilenames.copy) == 0) & (length(unique(Vector)) == nCategories))
    {
    not.done = FALSE
    }

    else
    {

    message = 'unknown error!';
      if (length(listoffilenames.copy) != 0)
      {
      message = 'Not every File got an assigned condition! Start over?'

      }

      else if (length(unique(Vector)) != nCategories)
      {
      message = 'Not all conditions were assigned! Start over?'
      }

    res <- tcltk.Message.Box(Message = message)

      if (res == 'yes')
      {
      listoffilenames.copy = listoffilenames;
      }

      else
      {
      return(NULL)
      }
    }
  }
return(Vector)
}


tcltk.select.category <- function(listofcategories)
{
not.done = TRUE
  while(not.done)
  {
  #ret <- tk_select.list(listofcategories, preselect = NULL, multiple = TRUE, title = 'Please select the two categories for comparison:')
  ret <- tcltk.listbox(parent.window = NEW$ttMain, question = 'Please select two condition to compare: ', selection = listofcategories)



    if (length(ret) == 2)
    {
    return(which(listofcategories %in% ret))
    }

    else if (length(ret) == 0)
    {
    return(NULL)
    }
  }
}

do.bad <- function()
{
#ClassNames
Class.names.Vec <- tcltk.multiple.input.boxes()
#ClassVec
Vector <- tcltk.assign.files2category(c('a file','another file','just a file, man'),Class.names.Vec)
#SelectedClasses
Vector2 <- tcltk.select.category(Class.names.Vec)
return(list(Class.names.Vec,Vector,Vector2));
}


select.multiple.files <- function(type = 'ALL')
{
keep.running = TRUE

  while(keep.running)
  {
    if (type == 'All')
    {
    fullFiles = as.character(tkgetOpenFile(filetypes = "{{FASTA} {.fna}}
    {{FASTA} {.fa}}
    {{FASTA} {.FASTA}}
    {{FASTA} {.FA}}
    {{FASTA} {.FNA}}
    {{FASTA} {.fasta}}
    {{UProC} {.uproc}}
    {{All files} *}", multiple = TRUE))
    }

    else if (type == 'FASTA')
    {
    fullFiles = as.character(tkgetOpenFile(filetypes = "{{FASTA} {.fna}}
    {{FASTA} {.fa}}
    {{FASTA} {.FASTA}}
    {{FASTA} {.FA}}
    {{FASTA} {.FNA}}
    {{FASTA} {.fasta}}
    {{All files} *}", multiple = TRUE))
    }

    else if (type == 'UProC')
    {
    fullFiles = as.character(tkgetOpenFile(filetypes = "{{UProC} {.uproc}}
    {{All files} *}", multiple = TRUE))
    }


    if (length(fullFiles) < 2)
    {
    ret = spawn.retrycancel.messagebox('WARNING: less than two files selected.', the.details = 'use "shift" or "control" to select multiple files')
    print(ret)
      if (ret == "cancel")
      {
      return(-1)
      }
    }

    else
    {
    keep.running = FALSE
    }
  }
return(fullFiles)
}


br.selection <- function(br.names,Environment)
{
BASE_BR_SELECTION <- c(2:24, 28, 30:32,37,40)
nButtons = length(br.names)
template <- vector(mode = 'numeric', length = nButtons)
template[BASE_BR_SELECTION] = 1;
tcl.Var.Vec <- buttonbuilder.bytemplate(nButtons,template)

  rand.dis.shit <- function()
  {
    for (i in 1:nButtons)
    {
    tclvalue(tcl.Var.Vec[[i]]) = sample(0:1,1)
    }
  }

  sub.ok.function <- function()
  {
  print('mhhhh...');
  tclvalue(tcl.Var.Vec[[nButtons+1]]) = 'OK'
  }

  sub.cancel.function <- function()
  {
  print('...mhhhh');
  tclvalue(tcl.Var.Vec[[nButtons+1]]) = 'CANCEL'
  }

tt.ko2br <- tktoplevel(Environment$ttMain)
frame.process <- ttkframe(tt.ko2br, padding = c(1,2,4,8))

Ret <- checkboxlist.builder(tcl.Var.Vec,br.names,frame.process)

#give an additional value for the ok/cancel

tcl.Var.Vec[[nButtons+1]] = tclVar(0)


  for (i in 1:length(Ret))
  {
  tkgrid(Ret[[i]], row = i, column = 0, columnspan = 2, sticky = 'nsew')
  }

  button.ok = ttkbutton(frame.process, text = 'ok', command = sub.ok.function)
  button.default = ttkbutton(frame.process, text = 'default', command = rand.dis.shit)
  button.cancel = ttkbutton(frame.process, text = 'cancel', command = sub.cancel.function)

  tkgrid(button.ok, row = length(Ret)+1, column = 0, columnspan = 1, sticky = 'nsew')
  tkgrid(button.default, row = length(Ret)+1, column = 1, columnspan = 1, sticky = 'nsew')
  tkgrid(button.cancel, row = length(Ret)+1, column = 2, columnspan = 1, sticky = 'nsew')

tkpack(frame.process, expand = 1, fill = 'both')

window.next.to.parent(NEW$ttMain,tt.ko2br)

tkwait.variable(tcl.Var.Vec[[nButtons+1]])


#tkconfigure(widget = die, state = 'enabled')
tkdestroy(tt.ko2br)

TrueFalseVec <- sapply(1:nButtons, function(x) as.logical(as.numeric(tclvalue(tcl.Var.Vec[[x]]))))
return(TrueFalseVec)
}



getfiles.for.selection <- function(Object.Part)
{
  if (length(slot(Object.Part,FILETYPE.DNA)) > 0)
  {
    return(slot(Object.Part,FILETYPE.DNA))
  }

  else if (length(slot(Object.Part,FILETYPE.DNAwoRNA)) > 0)
  {
    return(slot(Object.Part,FILETYPE.DNAwoRNA))
  }

  else if (length(slot(Object.Part,FILETYPE.UproC)) > 0)
  {
    return(slot(Object.Part,FILETYPE.UproC))
  }

  else
  {
    return(c('all','is','empty'))
  }
}

#S4

ALL.BUTTON.NAMES <- c(
    'button.process.output',

    'button.input.fasta',
    'button.input.fastanorrna',
    'button.input.uproc',
    'button.input.object',

    'button.process.category',
    'button.process.conditions',
    'button.process.score',
    'button.process.taxonomy',

    'button.analyse.methods',
    'button.analyse.pca',
    'button.analyse.br',
    'button.analyse.venn',
    'button.analyse.pathway',

    'button.output.svghtml',
    'button.output.csv',
    'button.output.figures'
    )



set.objectinput.object <- function(Object)
{
  print('wot?')
  slot(slot(Object,"button.input.object"),'interaction.on') <- ALL.BUTTON.NAMES[c(4,5)]
  slot(slot(Object,"button.input.object"),'interaction.off') <- ALL.BUTTON.NAMES[c(1:3,6:14)]
  return(Object)
}

  set.objectinput.conditions <- function(Object)
{
  print('wot?')
  slot(slot(Object,"button.process.conditions"),'interaction.on') <- ALL.BUTTON.NAMES[c(7,9)]
  slot(slot(Object,"button.process.conditions"),'interaction.off') <- ALL.BUTTON.NAMES[c(8,10:14)]
  return(Object)
}

swtich.function <- function(Object,Part.Object)
{
  Switch.on.Vec <- slot(slot(Object,slot(Part.Object,'name')),'interaction.on')
  Switch.off.Vec <- slot(slot(Object,slot(Part.Object,'name')),'interaction.off')

    for (curr.Name in slotNames(Object))
    {
#turn on
      if (curr.Name %in% Switch.on.Vec)
      {
      slot(slot(Object,curr.Name), 'state') <- TRUE
      }
	  #turn off
      else if (curr.Name %in% Switch.off.Vec)
      {
      slot(slot(Object,curr.Name), 'state') <- FALSE
      }
    }
return(Object)
}

sleepfunction <- function()
{
Sys.sleep(5)
}

emptyfunction <- function()
{
print('i am empty inside!');
return(1+1)
}

tkwin <- setClass(
"tkwin",
  slots = c(
  dummy = 'logical'
  )
)

button.container <- setClass(
  "button.container",
    slots = c(
    name = 'vector',
    state = 'logical',
    interaction.on = 'vector',
    interaction.off = 'vector',
    exec.function = 'function',
    tcldata = "tkwin"
  ),
  prototype = list(
    name = NULL,
    state = FALSE,
    interaction.on = vector(),
    interaction.off = vector(),
    exec.function = emptyfunction,
    tcldata = NULL
  )
)

#INPUT
class.button.input.fasta <- setClass(
  "class.button.input.fasta",
  contains = 'button.container'
)
class.button.input.fastanorrna <- setClass(
  "class.button.input.fastanorrna",
  contains = 'button.container'
)
class.button.input.uproc <- setClass(
  "class.button.input.uproc",
  contains = 'button.container'
)
class.button.input.object <- setClass(
  "class.button.input.object",
  contains = 'button.container'
)

#PROCESS
class.button.process.output <- setClass(
  "class.button.process.output",
  contains = 'button.container'
)
class.button.process.category <- setClass(
  "class.button.process.category",
  contains = 'button.container'
)
class.button.process.conditions <- setClass(
  "class.button.process.conditions",
  contains = 'button.container'
)
class.button.process.score <- setClass(
  "class.button.process.score",
  contains = 'button.container'
)
class.button.process.taxonomy <- setClass(
  "class.button.process.taxonomy",
  contains = 'button.container'
)

#Analyze
class.button.analyse.methods <- setClass(
  "class.button.analyse.methods",
  contains = 'button.container'
)
class.button.analyse.pca <- setClass(
  "class.button.analyse.pca",
  contains = 'button.container'
)
class.button.analyse.br <- setClass(
  "class.button.analyse.br",
  contains = 'button.container'
)
class.button.analyse.venn <- setClass(
  "class.button.analyse.venn",
  contains = 'button.container'
)
class.button.analyse.pathway <- setClass(
  "class.button.analyse.pathway",
  contains = 'button.container'
)

#Output
class.button.output.svghtml <- setClass(
  "class.button.output.svghtml",
  contains = 'button.container'
)

class.button.output.csv <- setClass(
  "class.button.output.csv",
  contains = 'button.container'
)

class.button.output.figures <- setClass(
  "class.button.output.figures",
  contains = 'button.container'
)

class.button.main.saveobject <- setClass(
  "class.button.main.saveobject",
  contains = 'button.container'
)

class.button.main.ok <- setClass(
  "class.button.main.ok",
  contains = 'button.container'
)

class.button.main.reset <- setClass(
  "class.button.main.reset",
  contains = 'button.container'
)

class.button.main.quit <- setClass(
  "class.button.main.quit",
  contains = 'button.container'
)

Class.Object.Buttons <- setClass(
  "Class.Object.Buttons",
  slots = c(
    button.input.fasta = "class.button.input.fasta",
    button.input.fastanorrna = "class.button.input.fastanorrna",
    button.input.uproc = "class.button.input.uproc",
    button.input.object = "class.button.input.object",

    button.process.output = "class.button.process.output",
    button.process.category = "class.button.process.category",
    button.process.conditions = "class.button.process.conditions",
    button.process.score = "class.button.process.score",
    button.process.taxonomy = "class.button.process.taxonomy",

    button.analyse.methods = "class.button.analyse.methods",
    button.analyse.pca = "class.button.analyse.pca",
    button.analyse.br = "class.button.analyse.br",
    button.analyse.venn = "class.button.analyse.venn",
    button.analyse.pathway = "class.button.analyse.pathway",

    button.output.svghtml = "class.button.output.svghtml",
    button.output.csv = "class.button.output.csv",
    button.output.figures = "class.button.output.figures",

    button.main.saveobject = "class.button.main.saveobject",
    button.main.ok = "class.button.main.ok",
    button.main.reset = "class.button.main.reset",
    button.main.quit = "class.button.main.quit"
  )
)

check.state <- function(Object,Button.Names)
{
	    for (curr.Name in Button.Names)
	    {

	      if (slot(slot(Object,curr.Name),'state'))
	      {
	      tkconfigure(widget = slot(slot(Object,curr.Name),'tcldata'), state = 'enabled')
	      }

	      else
	      {
	      tkconfigure(widget = slot(slot(Object,curr.Name),'tcldata'), state = 'disabled')
	      }
	    }
return(Object)
}

BUTTONS.ON.OFF <- function(Object.Part, Environment, x, y)
{
           Object.Copy <- Environment$Container.Object.Button
           #set all to off
           Button.Names <- slotNames(Object.Copy)



	    for (curr.Name in Button.Names)
	    {
	    tkconfigure(widget = slot(slot(Object.Copy,curr.Name),'tcldata'), state = 'disabled')
	    }
	   tkconfigure(widget = slot(slot(Object.Copy,'button.main.quit'),'tcldata'), state = 'disabled')

           Environment$Container.Object.Button <- Object.Copy



           #do something based on object class

           tcltk.variable = tclVar(-1)





           ret <- button.execute(Object.Part, Environment, Environment$ttMain, tcltk.variable)

           #apply changes to following buttons
           #Object.Copy <- set.condition.object(Object.Copy)
	    if (ret == 'OK')
	    {
	    Object.Copy <- set.interaction.on(slot(Object.Copy,slot(Object.Part,'name')),Object.Copy,slot(Object.Part,'name'),NULL)
	    Object.Copy <- button.set.states(Object.Part,Object.Copy,NULL,NULL)
	    }

	    else
	    {
	    print('problem123')
	    #Object.Copy <- button.set.states(Object.Part,Object.Copy,NULL,NULL)
	    }


           # #################################

           #set old values with change based on button

	   Object.Copy <- check.state(Object.Copy,Button.Names)

	   tkconfigure(widget = slot(slot(Object.Copy,'button.main.quit'),'tcldata'), state = 'enabled')
	   tkconfigure(widget = slot(slot(Object.Copy,'button.main.reset'),'tcldata'), state = 'enabled')
	   Environment$Container.Object.Button <- Object.Copy

}


initialize.Button.Object <- function()
{
  #create empty dummy object
  Object <- Class.Object.Buttons()
  #perform on each object....
  Button.Names <- slotNames(Object)
  for (curr.Name in Button.Names)
  {
  slot(slot(Object,curr.Name),'name') <- curr.Name
    if (curr.Name == "button.input.fasta")
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == "button.input.fastanorrna")
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == "button.input.uproc")
    {
    #set buttons on/off
    Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == "button.input.object")
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }


    else if (curr.Name == 'button.process.output')
    {
    #set buttons on/off
    slot(slot(Object,curr.Name),'state') <- TRUE
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)


    }

    else if (curr.Name == 'button.process.category')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)

    }

    else if (curr.Name == 'button.process.conditions')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }


    else if (curr.Name == 'button.process.score')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }


    else if (curr.Name == 'button.process.taxonomy')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == 'button.analyse.methods')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == 'button.analyse.pca')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == 'button.analyse.br')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == 'button.analyse.venn')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }


    else if (curr.Name == 'button.analyse.pathway')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == 'button.output.svghtml')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == 'button.output.csv')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }

    else if (curr.Name == 'button.output.figures')
    {
    #set buttons on/off
   Object <- set.interaction.on(slot(Object,curr.Name),Object,curr.Name,NULL)
    }


    else if (curr.Name == "button.main.saveobject")
    {

    }

    else if (curr.Name == "class.button.main.ok")
    {
    slot(slot(Object,curr.Name),'state') <- TRUE
    }

    else if (curr.Name == "class.button.main.reset")
    {
    slot(slot(Object,curr.Name),'state') <- TRUE
    }

    else if (curr.Name == "class.button.main.quit")
    {
    slot(slot(Object,curr.Name),'state') <- TRUE
    }


  }
return(Object)
}







# #######################METHODS




setGeneric("button.execute",
           function(Object, Environment,x,y)
           {Eprint('NO SUCH METHOD')}
)

setMethod ("button.execute", "class.button.input.fasta",
           function(Object, Environment, x, y){
           print('mhhhh?')

           #lock all buttons
	   child.window <- tktoplevel(x);
	   ret <- Message.waiting(child.window,input.fasta.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)
	   #unlock new buttons
           })

setMethod ("button.execute", "class.button.input.fastanorrna",
           function(Object, Environment, x, y){
	   print('fastanorrna!')

	   child.window <- tktoplevel(x);
	   ret <- Message.waiting(child.window,input.fastanorrna.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)
	   print('done waiting...')
	   return('OK')
           })



setMethod ("button.execute", "class.button.input.uproc",
           function(Object, Environment, x, y){
	   print('fastanorrna!')
	   #select uproc files


	   child.window <- tktoplevel(x);

	   ret <- Message.waiting(child.window,input.uproc.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)

	   #transform uproc to RDS

	   #store in object

	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   print('done waiting...')
	   return('OK')
           })
setMethod ("button.execute", "class.button.input.object",
           function(Object, Environment, x, y){
	   print('fastanorrna!')

	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   print('done waiting...')
	   return('FALSE')
           })
setMethod ("button.execute", "class.button.process.output",
           function(Object, Environment, x, y){
	   print('fastanorrna!')


	   AAA <- tk_choose.dir()

	    if (!is.na(AAA))
	    {
	    slot(NEW$Object.job.path,"DirOut") <- AAA
	    create.directory(AAA,c('UPROC','RDS','HTML','OBJECT'))
	    }

	   else
	   {
	   return('fark!')
	   }

	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   print('done waiting...')
	   return('OK')
           })
setMethod ("button.execute", "class.button.process.category",
           function(Object, Environment, x, y){
	   print('fastanorrna!')

	   xx <- br.selection(unlist(Environment$ko2br@Names),Environment)
	   slot(Environment$Object.job.config,'SelectedBR') <- xx
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   print('done waiting...')
	   return('OK')
           })
setMethod ("button.execute", "class.button.process.conditions",
           function(Object, Environment, x, y){
	   print('class.button.process.conditions!')

	   #HACK##############



#ClassNames
Class.names.Vec <- tcltk.multiple.input.boxes()
slot(Environment$Object.job.config,'ClassNames') <- Class.names.Vec

Ret <- getfiles.for.selection(NEW$Object.job.path)

#ClassVec
Vector <- tcltk.assign.files2category(Ret,Class.names.Vec)
slot(Environment$Object.job.config,'ClassVec') <- Vector
#SelectedClasses
Vector2 <- tcltk.select.category(Class.names.Vec)
slot(Environment$Object.job.config,'SelectedClasses') <- Vector2
	   print(Vector)
	   print(Vector2)
	   ##############HACK#
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';


	   print('done waiting...')
	   return('OK')
           })
setMethod ("button.execute", "class.button.process.score",
           function(Object, Environment, x, y){
	   print('fastanorrna!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';

	   child.window <- tktoplevel(x);

	   ret <- Message.waiting(child.window,process.score.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)


	   print('done waiting...')
	   return('OK')
           })
setMethod ("button.execute", "class.button.process.taxonomy",
           function(Object, Environment, x, y){
	   print('class.button.process.taxonomy!')

	   child.window <- tktoplevel(x);

	   ret <- Message.waiting(child.window,process.taxonomy.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   print('done waiting...')
	   return('OK')
           })

setMethod ("button.execute", "class.button.analyse.methods",
           function(Object, Environment, x, y){
	   print('fastanorrna!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';


	   child.window <- tktoplevel(x);
	   ret <- Message.waiting(child.window,analyse.methods.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)

	   print('done waiting...')
	   return('OK')
           })
setMethod ("button.execute", "class.button.analyse.pca",
           function(Object, Environment, x, y){
	   print('fastanorrna!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';

	   child.window <- tktoplevel(x);
	   ret <- Message.waiting(child.window,analyse.pca.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)





	   print('done waiting...')
	   return('OK')
           })
setMethod ("button.execute", "class.button.analyse.br",
           function(Object, Environment, x, y){
	   print('fastanorrna!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   child.window <- tktoplevel(x);
	   ret <- Message.waiting(child.window,analyse.br.dummy,"Please press 'run' and wait for process to end.",Environment)


	   print('done waiting...')
	   return('OK')
           })

setMethod ("button.execute", "class.button.analyse.venn",
           function(Object, Environment, x, y){
	   print('fastanorrna!')

	   child.window <- tktoplevel(x);
	   ret <- Message.waiting(child.window,analyse.venn.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)


	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   print('done waiting...')
	   return('OK')
           })

setMethod ("button.execute", "class.button.analyse.pathway",
           function(Object, Environment, x, y){
	   print('fastanorrna!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';

	   child.window <- tktoplevel(x);
	   ret <- Message.waiting(child.window,analyse.pathway.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)


	   print('done waiting...')
	   return('OK')
           })


setMethod ("button.execute", "class.button.output.svghtml",
           function(Object, Environment, x, y){
	   print('button.output.svghtml!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';

	   child.window <- tktoplevel(x);
	   ret <- Message.waiting(child.window,output.svghtml.dummy,"Please press 'run' and wait for process to end.",Environment)
	   print(ret)

	   print('done waiting...')
	   return(y)
           })


setMethod ("button.execute", "class.button.output.csv",
           function(Object, Environment, x, y){
	   print('button.output.csv!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   print('done waiting...')
	   return(y)
           })

setMethod ("button.execute", "class.button.output.figures",
           function(Object, Environment, x, y){
	   print('button.output.figures!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	   print('done waiting...')
	   return(y)
           })



setGeneric("button.set.states",
           function(Part.Object, Object,x,y)
           {ERROR$new('NO SUCH METHOD')$throw()}
)

setMethod ("button.set.states", "class.button.input.fasta",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button

	   Object <- swtich.function(Object,Part.Object)

	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.input.fastanorrna",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })




setMethod ("button.set.states", "class.button.input.uproc",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.input.object",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	   Object <- set.objectinput.object(Object)
	   Object <- set.objectinput.conditions(Object)
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.process.output",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';

	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.process.category",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.process.conditions",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';
	    if (slot(slot(Object,'button.input.object'),'state'))
	    {
	    Object <- set.objectinput.conditions(Object)
	    }


	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.process.score",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';

	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.process.taxonomy",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	   #tclvalue(Environment$tcltk.variable) = 'QQ';

	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })


setMethod ("button.set.states", "class.button.analyse.methods",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button\

	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.analyse.pca",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.analyse.br",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.analyse.venn",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.analyse.pathway",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })


setMethod ("button.set.states", "class.button.output.svghtml",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.output.csv",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })

setMethod ("button.set.states", "class.button.output.figures",
           function(Part.Object, Object,x,y){
	   print('set buttons for fasta!')
	   #Environment$Container.Object.Button
	    Object <- swtich.function(Object,Part.Object)
	   print('done...')
	   return(Object)
           })





setGeneric("set.interaction.on",
           function(Part.Object, Object,x,y)
           {ERROR$new('NO SUCH METHOD')$throw()}
)

setMethod ("set.interaction.on", "class.button.input.fasta",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(2,6)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(3:5,7:17)]
	   return(Object)
           })


setMethod ("set.interaction.on", "class.button.input.fastanorrna",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(3,6)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(1,2,4:5,7:17)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.input.uproc",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(4,6)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(1:3,5,7:17)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.input.object",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(5,6)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(2:4,7:17)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.process.output",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(1:5)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(6:17)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.process.category",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(6,7)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(8:17)]
	   return(Object)
           })

           #do nothing at it is not known on creation...
setMethod ("set.interaction.on", "class.button.process.conditions",
           function(Part.Object, Object,x,y){
	   slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(7,8)]
	   slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(9:17)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.process.score",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(8,9)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(10:17)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.process.taxonomy",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(9,10)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(12:17)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.analyse.methods",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(10:14)]
	    slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(15:17)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.analyse.pca",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(17)]
	    #slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(2,3,4)]
	   return(Object)
           })


setMethod ("set.interaction.on", "class.button.analyse.br",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(17)]
	    #slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(2,3,4)]
	   return(Object)
           })


setMethod ("set.interaction.on", "class.button.analyse.venn",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(17)]
	    #slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(2,3,4)]
	   return(Object)
           })


setMethod ("set.interaction.on", "class.button.analyse.pathway",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(15)]
	    #slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(2,3,4)]
	   return(Object)
           })


setMethod ("set.interaction.on", "class.button.output.svghtml",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(15)]
	    #slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(2,3,4)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.output.csv",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(15)]
	    #slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(2,3,4)]
	   return(Object)
           })

setMethod ("set.interaction.on", "class.button.output.figures",
           function(Part.Object, Object,x,y){
	    slot(slot(Object,x),'interaction.on') <- ALL.BUTTON.NAMES[c(15)]
	    #slot(slot(Object,x),'interaction.off') <- ALL.BUTTON.NAMES[c(2,3,4)]
	   return(Object)
           })

#test_buttons

#source('test_buttons.R', echo = TRUE)
# S4######

#tclServiceMode(FALSE)

window.center <- function(tkwindow)
{
tkwm.protocol(tkwindow, 'WM_DELETE_WINDOW', cancel.function)

#tcl('wm', 'attributes', ttMain, '-zoomed',1)

tkwinfo('geometry', tkwindow)
tkwm.geometry(tkwindow)
#set size of the window

screen.h <- as.integer(tkwinfo('screenheight', tkwindow))
screen.w <- as.integer(tkwinfo('screenwidth', tkwindow))

screen.h.wanted <- 640
screen.w.wanted <- 400

Y.Move <- floor(screen.h/2) - floor(screen.h.wanted/2)
X.Move <- floor(screen.w/2) - floor(screen.w.wanted/2)

string = paste0(screen.w.wanted,'x',screen.h.wanted,'+',X.Move,'+',Y.Move)

tkwm.geometry(tkwindow, string)


tkfocus(tkwindow)
}





input.button.fasta.func <- function()
{
buttonOFF(class.button.input.fasta())
fullFile = c();
do.retry = TRUE
  while(do.retry)
  {
  fullFile = as.character(tkgetOpenFile(filetypes = "{{Meander object files} {.MeandeR}} {{All files} *}", multiple = TRUE))
  print(length(fullFile))
  nRetry = nRetry + 1
  spawn.retrycancel.messagebox
  }

  if (Val == 'continue')
  {
  buttonON(class.button.input.fasta())
  }

  else
  {

  }
}



buttonbuilder <- function(nButtons)
{
Ret <- lapply(1:nButtons, function(x) tclVar(sample(0:1,1)))
return(Ret)
}

loadobject.function <- function(Environment)
{
fullFile = as.character(tkgetOpenFile(filetypes = "{{Meander object files} {.MeandeR}} {{All files} *}"))


Object <- readRDS(fullFile);
  if (class(Object)[1] == "MeandeRObject")
  {
  #put Object into Environment
  Environment$MeandeR.Object <- Object
  }

  else
  {
  stop("You call that a MeandeR object?!... THIS is a MeandeR object!")
  }

}


buttonbuilder.bytemplate <- function(nButtons,template)
{
  if (length(template) < nButtons)
  {
  tclvalue(A[[4]]) <- 'CRASH'
  stop("nButtons < template!");
  }
Ret <- lapply(1:nButtons, function(x) tclVar(template[x]))
return(Ret)
}


checkboxlist.builder <- function(tclvar.vec,Names,Frame)
{
nLength = length(tclvar.vec)
  if (length(Names) == nLength)
  {
  Stuff <- lapply(1:nLength, function(x) ttkcheckbutton(Frame, text = Names[x], variable = tclvar.vec[[x]], onvalue = TRUE))
  }

  else
  {
  stop('RIP!');
  }
return(Stuff);
}

ok.function <- function()
{
tclvalue(NEW$tclvar.main.okcancel) <- 'OK'
}

cancel.function <- function()
{
tclvalue(NEW$tclvar.main.okcancel) <- 'CANCEL'
}


reset.function <- function()
{
tkdestroy(NEW$ttMain)
NEW$ttMain <- tktoplevel()

window.center(NEW$ttMain)

Obj <- spawn.buttons(NEW$ttMain)
Obj <- check.state(Obj,slotNames(Obj))
NEW$Container.Object.Button <- Obj
}


fakefunction <- function()
{
tkconfigure(widget = die, state = 'disabled')
ko2br.selectionbuilder(unlist(ko2br@Names),NEW)

}


dummy.loadobject.function <- function()
{
loadobject.function(NEW)
}


stupidfunction <- function(ENV)
{
tclvalue(ENV$A[[1]]) <- 1;
}






spawn.retrycancel.messagebox <- function(the.message, the.details = NULL)
{
  if (is.null(the.details))
  {
  return(as.character(tkmessageBox(message = the.message, type = 'retrycancel')))

  }
return(as.character(tkmessageBox(message = the.message, detail = the.details, type = 'retrycancel')))
}


#Sel.Vec <- c(2:24, 28, 30:32,37,40)
#AAAA <- tk_select.list(unlist(ko2br@Names), preselect = unlist(ko2br@Names)[Sel.Vec], multiple = TRUE,title = 'nimm2')



spawn.buttons <- function(parent)
{
Container.Object.Button <- initialize.Button.Object()
frame.first <- ttkframe(parent, padding = c(1,2,4,8), borderwidth = 10, relief = 'ridge', width = 200, height = 100)
frame.input <- ttkframe(parent, padding = c(1,2,4,8), borderwidth = 10, relief = 'ridge', width = 200, height = 100)
frame.process <- ttkframe(parent, padding = c(1,2,4,8), borderwidth = 10, relief = 'groove', width = 200, height = 100)
frame.analyse <- ttkframe(parent, padding = c(1,2,4,8), borderwidth = 10, relief = 'groove', width = 200, height = 100)
frame.output <- ttkframe(parent, padding = c(1,2,4,8), borderwidth = 10, relief = 'groove', width = 200, height = 100)
frame.save <- ttkframe(parent, padding = c(1,2,4,8), borderwidth = 10, relief = 'groove', width = 200, height = 100)


namelbl <- ttklabel(frame.input, text = 'NAAAAAAAME')
name = ttkentry(frame.input)



#	#########################BUTTONS################################


slot(slot(Container.Object.Button,'button.process.output'),'tcldata') = ttkbutton(frame.first, text = '1Output Folder', command = button.dummy.process.output)
tkgrid(slot(slot(Container.Object.Button,'button.process.output'),'tcldata'), row = 2, column = 0, columnspan = 2, sticky = 'nsew')



# Input #####################################

# label
input.label = tklabel(frame.input, text = "Input" ,background ='#9080F0' ,foreground = '#0ffff0')
# Sep
input.sep.horizontal = ttkseparator(frame.input, orient= 'horizontal')
input.sep.horizontal2 = ttkseparator(frame.input, orient= 'horizontal')
# Buttons
slot(slot(Container.Object.Button,'button.input.fasta'),'tcldata') = ttkbutton(frame.input, text = '2FASTA', command = button.dummy.input.fasta)
slot(slot(Container.Object.Button,'button.input.fastanorrna'),'tcldata') = ttkbutton(frame.input, text = '3FASTA w/o rRNA', command = button.dummy.input.fastanorrna)
slot(slot(Container.Object.Button,'button.input.uproc'),'tcldata') = ttkbutton(frame.input, text = '4UPROC', command = button.dummy.input.uproc)
slot(slot(Container.Object.Button,'button.input.object'),'tcldata') = ttkbutton(frame.input, text = '5Object', command = button.dummy.input.object)


# layout
tkgrid(input.label, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
tkgrid(input.sep.horizontal, row = 1, columnspan=2, sticky = 'ew')
tkgrid(slot(slot(Container.Object.Button,'button.input.fasta'),'tcldata'), row = 2, column = 0, columnspan = 1, sticky = 'ew')
tkgrid(slot(slot(Container.Object.Button,'button.input.fastanorrna'),'tcldata'), row = 2, column = 1, columnspan = 1, sticky = 'ew')
tkgrid(slot(slot(Container.Object.Button,'button.input.uproc'),'tcldata'), row = 3, column = 0, columnspan = 2, sticky = 'ew')
tkgrid(slot(slot(Container.Object.Button,'button.input.object'),'tcldata'), row = 4, column = 0, columnspan = 2, sticky = 'ew')


# ######################################Input

# Process ###################################
# label

process.label = tklabel(frame.process, text = "Selection" ,background ='#808080' ,foreground = '#f0f0f0')
# Sep

process.sep.horizontal = ttkseparator(frame.process, orient= 'horizontal')
# Buttons


slot(slot(Container.Object.Button,'button.process.category'),'tcldata') = ttkbutton(frame.process, text = '6Categories', command = button.dummy.process.category)
slot(slot(Container.Object.Button,'button.process.conditions'),'tcldata') = ttkbutton(frame.process, text = '7Conditions', command = button.dummy.process.conditions)
slot(slot(Container.Object.Button,'button.process.score'),'tcldata') = ttkbutton(frame.process, text = '8Score', command = button.dummy.process.score)
slot(slot(Container.Object.Button,'button.process.taxonomy'),'tcldata') = ttkbutton(frame.process, text = '9Taxonomy', command = button.dummy.process.taxonomy)

# layout

tkgrid(process.label, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
tkgrid(process.sep.horizontal, row = 1, columnspan = 2, sticky = 'ew')


tkgrid(slot(slot(Container.Object.Button,'button.process.category'),'tcldata'), row = 3, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.process.conditions'),'tcldata'), row = 3, column = 1, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.process.score'),'tcldata'), row = 4, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.process.taxonomy'),'tcldata'), row = 4, column = 1, columnspan = 1, sticky = 'nsew')


# ####################################Process

# Analyse ###################################
# label

analyse.label = tklabel(frame.analyse, text = "Analyse" ,background ='#BBE000' ,foreground = '#ffffff')
# sep

analyse.sep.horizontal = ttkseparator(frame.analyse, orient= 'horizontal')
# buttons



slot(slot(Container.Object.Button,'button.analyse.methods'),'tcldata') = ttkbutton(frame.analyse, text = '10run Methods', command = button.dummy.analyse.methods)
slot(slot(Container.Object.Button,'button.analyse.pca'),'tcldata') = ttkbutton(frame.analyse, text = '11PCA', command = button.dummy.analyse.pca)
slot(slot(Container.Object.Button,'button.analyse.br'),'tcldata') = ttkbutton(frame.analyse, text = '12br functions', command = button.dummy.analyse.br)
slot(slot(Container.Object.Button,'button.analyse.venn'),'tcldata') = ttkbutton(frame.analyse, text = '13Venn', command = button.dummy.analyse.venn)
slot(slot(Container.Object.Button,'button.analyse.pathway'),'tcldata') = ttkbutton(frame.analyse, text = '14Pathway analysis', command = button.dummy.analyse.pathway)


# layput
tkgrid(analyse.label, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.analyse.methods'),'tcldata'), row = 1, column = 0, columnspan = 2, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.analyse.pca'),'tcldata'), row = 2, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.analyse.br'),'tcldata'), row = 2, column = 1, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.analyse.venn'),'tcldata'), row = 3, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.analyse.pathway'),'tcldata'), row = 3, column = 1, columnspan = 1, sticky = 'nsew')


#tkgrid(analyse.label, row = 0, column = 0, columnspan = 2, sticky = 'nsew')
#tkgrid(analyse.button.methods, row = 1, column = 0, columnspan = 2, sticky = 'nsew')
#tkgrid(analyse.button.pca, row = 2, column = 0, columnspan = 1, sticky = 'nsew')
#tkgrid(analyse.button.br, row = 2, column = 1, columnspan = 1, sticky = 'nsew')
#tkgrid(analyse.button.venn, row = 3, column = 0, columnspan = 1, sticky = 'nsew')
#tkgrid(analyse.button.pathway, row = 3, column = 1, columnspan = 1, sticky = 'nsew')
# ####################################Analyse

# Output ####################################
# label

output.label = tklabel(frame.output, text = "Output" ,background ='#000000' ,foreground = '#ffffff')
# sep

output.sep.horizontal = ttkseparator(frame.output, orient= 'horizontal')
# buttons

slot(slot(Container.Object.Button,'button.output.svghtml'),'tcldata') = ttkbutton(frame.output, text = '15SVG/HTML', command = button.dummy.output.svghtml)
slot(slot(Container.Object.Button,'button.output.csv'),'tcldata') = ttkbutton(frame.output, text = '16CSV', command = emptyfunction)
slot(slot(Container.Object.Button,'button.output.figures'),'tcldata') = ttkbutton(frame.output, text = '17figures', command = emptyfunction)


# layput

tkgrid(output.label, row = 0, column = 0, columnspan = 4, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.output.svghtml'),'tcldata'), row = 1, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.output.csv'),'tcldata'), row = 1, column = 1, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.output.figures'),'tcldata'), row = 2, column = 0, columnspan = 2, sticky = 'nsew')

# #####################################Output

# Save ######################################
# label

# sep

# buttons
slot(slot(Container.Object.Button,'button.main.saveobject'),'tcldata') = ttkbutton(frame.save, text = '19save Object', command = emptyfunction)
slot(slot(Container.Object.Button,'button.main.reset'),'tcldata') = ttkbutton(frame.save, text = '18Reset', command = reset.function)
slot(slot(Container.Object.Button,'button.main.ok'),'tcldata') = ttkbutton(frame.save, text = '20OK', command = ok.function)
slot(slot(Container.Object.Button,'button.main.quit'),'tcldata') = ttkbutton(frame.save, text = '21cancel', command = cancel.function)

# layput


tkgrid(slot(slot(Container.Object.Button,'button.main.reset'),'tcldata'), row = 0, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.main.saveobject'),'tcldata'), row = 0, column = 1, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.main.ok'),'tcldata'), row = 0, column = 2, columnspan = 1, sticky = 'nsew')
tkgrid(slot(slot(Container.Object.Button,'button.main.quit'),'tcldata'), row = 0, column = 3, columnspan = 1, sticky = 'nsew')
# #######################################Save

#			BUTTONS########################################

tkgrid.columnconfigure( frame.first, 0, weight = 1 )
tkgrid.columnconfigure( frame.first, 1, weight = 1 )


tkgrid.columnconfigure( frame.input, 0, weight = 1 )
tkgrid.columnconfigure( frame.input, 1, weight = 1 )
#tkgrid.columnconfigure( frame.input, 5, weight = 1 )
tkgrid.rowconfigure( frame.input, 0, weight = 1)
tkgrid.rowconfigure( frame.input, 1, weight = 1)
#tkgrid.rowconfigure( frame.input, 7, weight = 1)

tkgrid.columnconfigure( frame.process, 0, weight = 1 )
tkgrid.columnconfigure( frame.process, 1, weight = 1 )
tkgrid.rowconfigure( frame.process, 0, weight = 1)
tkgrid.rowconfigure( frame.process, 1, weight = 1)

tkgrid.columnconfigure( frame.analyse, 0, weight = 1 )
tkgrid.columnconfigure( frame.analyse, 1, weight = 1 )
tkgrid.rowconfigure( frame.analyse, 0, weight = 1)

tkgrid.columnconfigure( frame.output, 0, weight = 1 )
tkgrid.columnconfigure( frame.output, 1, weight = 1 )
tkgrid.rowconfigure( frame.output, 0, weight = 1)
#	Second Part####################

tkgrid(frame.first, row = 0, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(frame.input, row = 0, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(frame.process, row = 1, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(frame.analyse, row = 2, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(frame.output, row = 3, column = 0, columnspan = 1, sticky = 'nsew')
tkgrid(frame.save, row = 4, column = 0, columnspan = 1, sticky = 'nsew')

tkpack(frame.first,frame.input,frame.process,frame.analyse,frame.output,frame.save, expand = 1, fill = 'both')
return(Container.Object.Button)
}

save.object.all <- function(Env)
{
  slot(slot(Env$Object.Final,'Job'),'Paths') = Object.job.path
  slot(slot(Env$Object.Final,'Job'),'Config') = Object.job.config
  slot(slot(Env$Object.Final,'Job'),'Statistics') = Object.job.statistics

  slot(slot(Env$Object.Final,'DATA'),'BIG') = Object.data.big
  slot(slot(Env$Object.Final,'DATA'),'KEGG') = Object.data.kegg
  slot(slot(Env$Object.Final,'DATA'),'Refined') = Object.data.refined
  slot(slot(Env$Object.Final,'DATA'),'DataFrames') =  Object.data.dataframes
}



main.interface <- function()
{

Q <- readRDS('~/projects/meander/new_method.rds')

ONE <<- 'QQQQQQQQQQQQQQQ'
ko2br <- readRDS('~/projects/meander/data/ko2br_pathway.rds')
NEW <<- environment();
tclvar.main.okcancel <- tclVar('empty')





  NEW$Object.job.path <- .Object.Job.Paths()
  NEW$Object.job.config <- .Object.Job.Config()
  NEW$Object.job.statistics <- .Object.Job.Statistics()

  NEW$Object.data.big <- .Object.DATA.BIG()
  NEW$Object.data.kegg <- .Object.DATA.KEGG()
  NEW$Object.data.refined <- .Object.DATA.Refined()
  NEW$Object.data.dataframes <- .Object.DATA.dataframes()
  ##load fixed data
  #tax Mat

  #Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'TaxMat',value = readRDS('./data/TaxMat.rds'))
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'TaxMat',value = readRDS(file.path(DATA_PATH,'TaxMat.rds')))
  #ko2path
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'KEGG2PATH', value = as.matrix(readRDS(file.path(DATA_PATH,'KEGG2PATH.rds'))))
  #kointax
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'KOinTax', value = readRDS(file.path(DATA_PATH,'KOlist.rds')))
  #...
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'ko2br.pathway', value = readRDS(file.path(DATA_PATH,'ko2br_pathway.rds')))

  NEW$Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'keggmapnames', value = readRDS(file.path(DATA_PATH,'keggmapnames.rds')))
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'pathway.names', value = readRDS(file.path(DATA_PATH,'pathway.names.Rds')))
  NEW$Object.data.kegg  <- setInputdata(ObjectPart = Object.data.kegg , Type = 'ko_desc', value = readRDS(file.path(DATA_PATH,'ko_desc.rds')))

  
  
#Keggmapnames
#pathway.names
#ko_descr





  #  .QDT <- perform.quickdatatable(slot(NEW$Object.data.big,'CountDT'))

  #NEW$Object.data.refined <- setInputdata(NEW$Object.data.refined,'QuickDT',.QDT)



  attemptExecution(Object.job.config <- set.methods(Object.job.config))
  print(Object.job.config)
  #png
    ##

  ##
  Object.Final <- Object();

NEW$ttMain <- tktoplevel()
#tell window it cancels, if closed
window.center(ttMain)


#load general settings


Container.Object.Button <- spawn.buttons(NEW$ttMain)

#set buttons to initial setting
Container.Object.Button <- check.state(Container.Object.Button,slotNames(Container.Object.Button))
#tclServiceMode(TRUE)
#wait for final buttonpress
print('main waiting...')



tkwait.variable(NEW$tclvar.main.okcancel)

tkdestroy(NEW$ttMain)
}
