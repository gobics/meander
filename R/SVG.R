NAME.SVG.rect <- function(xPos,yPos,xDim,yDim,col,opac)
{
  return(paste('<rect x="',xPos,'" y="',yPos,'" width="',xDim,'" height="',yDim,'" style="fill:',col,';fill-opacity:',opac,'; "/>\n',sep=''))
}

NAME.SVG.link <- function(ID,xPos,yPos,xDim,yDim)
{
  .String = paste('<g id="',ID,'">\n',sep='')
  .String = paste(.String,'<a xlink:href= "../html/K',sprintf('%05d',ID),'.html" target="sebi">\n',sep='')
  .String = paste(.String,NAME.SVG.rect(xPos,yPos,xDim,yDim,'#000000',0),sep='')
  .String = paste(.String,'</a>\n','</g>\n',sep='')
  return(.String)
}


NAME.SVG.header <- function(xVal,yVal)
{
  .string <- "<?xml version=\"1.0\" encoding=\"windows-1252\" standalone=\"no\"?>\n"
  .string <- paste(.string,"<svg width=\"",xVal,"\" height=\"",yVal,"\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" stroke-width=\"0\">\n",sep = "")
  return(.string)
}

NAME.SVG.includepic <- function(xPos,yPos,xDim,yDim,base64pic)
{
  .string <- paste("<image x=\"",xPos,"\" y=\"",yPos,"\" width=\"",xDim,"\" height=\"",yDim,"\" xlink:href=\"data:image/png;base64,",sep = "");
  .string <- c(.string,paste(base64pic,'\n\"/>\n'))
  return(.string)
}


perform.SVGcreation <- function(O.data.refined)
{
  #get the color vector
  ColVec <- slot(O.data.refined,'ColorVec')
  
df <- data.frame(x = NULL, y = NULL, z = NULL)
  #in object later
  XXX.KEGGmapnames <- readRDS('./data/keggmapnames.rds')
  XXX.SVG <- readRDS('./data/SVG_positions.rds') 
  XXX.PNG <- readRDS('./data/path_png.rds') 
  
  for (i in XXX.KEGGmapnames)
  {
    #get png alpha [1] & normal [2] & dimensions [x = 3, y = 4]
  .PNG <- XXX.PNG[[i]]
  .String = NAME.SVG.header(.PNG[3],.PNG[4])
  .X <- NAME.SVG.includepic(0,0,.PNG[3],.PNG[4],.PNG[2])
  .String = c(.String,.X)
  .Tmp.svg <- XXX.SVG[MAP == paste0(i,'.conf')]
  
    if (dim(.Tmp.svg)[1] != 0)
    {
    .KOz <- as.numeric(substr(.Tmp.svg[,KO],2,10))
    
    df = rbind(df,data.frame(x = unique(.KOz), y = rep(i,length(unique(.KOz))), z = rep(666,length(unique(.KOz)))))
    
    .xPos <- .Tmp.svg[,xPos]
    .yPos <- .Tmp.svg[,yPos]
    .width <- .Tmp.svg[,width]
        for (j in 1:length(.KOz))
        {
          .X <- NAME.SVG.rect(.xPos[j],.yPos[j],.width[j],18,ColVec[.KOz[j]],1)
          .String = c(.String,.X)        
        }
    
    .X <- NAME.SVG.includepic(0,0,.PNG[3],.PNG[4],.PNG[1])
    .String = c(.String,.X)
    
      for (j in 1:length(.KOz))
      {
        .X <- NAME.SVG.link(.KOz[j],.xPos[j],.yPos[j],.width[j],18)
        .String = c(.String,.X)
      }
      #write to file
          
    }
  cat(c(.String,'</svg>') , file = paste(i,'.svg',sep=''), sep = "\n", fill = FALSE, labels = NULL, append = FALSE)
  
  
  }
return(df)
}