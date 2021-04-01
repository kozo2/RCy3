# list available commands for annotations
#commandsHelp("annotation")
#commandsHelp("annotation add text")

# demo functions to be added to RCy3

#' @title Add Text Annotation
#' @return A named list of annotation properties, including uuid
#' @examples 
#' addAnnotationText("test1")
#' addAnnotationText("test2", 1000, 1000, name="T2")
#' addAnnotationText("test3", 1200, 1000, 30, "Helvetica", "bold", "#990000",40,name="T3", canvas="foreground",z=4)
#' 
addAnnotationText<-function(text = NULL, x.pos = NULL, y.pos = NULL,
                            fontSize = NULL, fontFamily = NULL, fontStyle = NULL,
                            color = NULL, angle = NULL, name = NULL,
                            canvas = NULL, z.order = NULL,
                            network = NULL, base.url = .defaultBaseUrl){

  cmd.string <- 'annotation add text'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view="SUID:',view.SUID,'"')

  
  # text to add
  if(is.null(text))
    stop(simpleError("Must provide the text string to add."))
  cmd.string <- paste0(cmd.string,' text="',text,'"')
  
  # x and y position
  if(is.null(x.pos))
    x.pos <- getNetworkCenter(net.SUID, base.url)$x
  if(is.null(y.pos))
    y.pos <- getNetworkCenter(net.SUID, base.url)$y
  cmd.string <- paste0(cmd.string,' x="',x.pos,'" y="',y.pos,'"')
  
  # optional params
  if(!is.null(fontSize)){
    .checkPositive(fontSize)
    cmd.string <- paste0(cmd.string,' fontSize="',fontSize,'"')
  }
  if(!is.null(fontFamily)){
    cmd.string <- paste0(cmd.string,' fontFamily="',fontFamily,'"')
  }
  if(!is.null(fontStyle)){
    .checkFontStyle(fontStyle)
    cmd.string <- paste0(cmd.string,' fontStyle="',fontStyle,'"')
  }
  if(!is.null(color)){
    .checkHexColor(color)
    cmd.string <- paste0(cmd.string,' color="',color,'"')
  }
  if(!is.null(angle)){
    rotation <- .normalizeRotation(angle)
    cmd.string <- paste0(cmd.string,' angle="',rotation,'"')
  }
  if(!is.null(name)){
    .checkUnique(name, sapply(getAnnotationList(), '[[', 'name'))
    cmd.string <- paste0(cmd.string,' name="',name,'"')
  }
  if(!is.null(canvas)){
    .checkCanvas(canvas)
    cmd.string <- paste0(cmd.string,' canvas="',canvas,'"')
  }
  if(!is.null(z.order)){
    if(!is.numeric(z.order))
      stop (simpleError(sprintf ('%d is invalid. Z order must be an number', z.order)))
    cmd.string <- paste0(cmd.string,' z="',z.order,'"')
  }
  
  # execute command
  res <- commandsPOST(cmd.string, base.url)
  return(as.list(res))
}

#' @title Delete Annotation
#' @return None
#' @examples 
#' deleteAnnotation("016a4af1-69bc-4b99-8183-d6f118847f96")
#' deleteAnnotation(c("6c0fe87b-51e1-439e-865b-87ad4e656efc","069800fb-7a6e-4408-a0b2-cd61c391790b"))
#' deleteAnnotation(sapply(getAnnotationList(), '[[', 'uuid'))
#' 
deleteAnnotation<-function(uuid = NULL, base.url = .defaultBaseUrl){
  if(is.null(uuid))
    stop('Must provide the UUID (or list of UUIDs) to delete')
  
  if(is.vector(uuid) ){
    lapply(uuid, function(u){
      commandsPOST(paste0('annotation delete uuid="',u,'"'), base.url)
    })
    invisible()
  }
  
  invisible(commandsPOST(paste0('annotation delete uuid="',uuid,'"'), base.url))
}

#' @title Get Annotation List
#' @return A list of named lists with annotation information
#' @details You can obtain a list of UUIDs by applying a subset function
#' like so: sapply(getAnnotationList(), '[[', 'uuid')
#' @examples 
#' getAnnotationList()
#' 
getAnnotationList<-function(network = NULL, base.url = .defaultBaseUrl){
  cmd.string <- 'annotation list'  # a good start
  
  net.SUID = getNetworkSuid(network,base.url)
  view.SUID = getNetworkViewSuid(net.SUID, base.url)
  
  # add view
  cmd.string <- paste0(cmd.string,' view=SUID:"',view.SUID,'"')
  
  commandsPOST(cmd.string, base.url)
}
