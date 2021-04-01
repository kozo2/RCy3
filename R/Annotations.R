## Testing automation of annotations

 library(RCy3)
 
 ## stuff already in RCy3 package
 .defaultBaseUrl <- 'http://localhost:1234/v1'
 .checkHexColor <- function(color){
   if ((substring(color, 1, 1) != "#") || (nchar(color) !=7)) {
     stop (simpleError(sprintf ('%s is not a valid hexadecimal color, e.g. #FD39B8.', color)))
   }
 }
 .checkOpacity <- function(opacity){
   if(is.numeric(opacity)){
     if(opacity%%1 != 0){
       stop(simpleError('Opacity must be an integer between 0 and 255.'))
     }
   } else {
     stop(simpleError('Opacity must be an integer between 0 and 255.'))
   }
   if (opacity < 0 || opacity > 255){
     stop (simpleError(sprintf ('%i is invalid. Opacity must be between 0 and 255.', opacity)))
   } 
 }

 # Additional checks to add to RCy3-utils.R
 .normalizeRotation <- function(degree){
   if(!is.numeric(degree))
       stop(simpleError('Angle must be a number.'))
   while (degree <= -180) 
     degree <- degree + 360
   while (degree > 180)
     degree <- degree - 360
   return(degree) #-180 to +180 range to match GUI
 }
 .checkUnique <- function(value, existing.values){
   if(value %in% existing.values)
     stop(simpleError(sprintf ('%s is not unique. Please provide a unique value.', as.character(value))))
 }
 .checkPositive <- function(number){
   if(!is.numeric(number))
     stop(simpleError('Value must be a positive number.'))
   if (number <= 0){
     stop (simpleError(sprintf ('%s is invalid. Number must be positive.', as.character(number))))
   } 
 }
 .checkFontStyle <- function(style){
   if(!style %in% c("plain","bold","italic","bolditalic"))
     stop (simpleError(sprintf ('%s is invalid. Use "plain", "bold", "italic" or "bolditalic"', style)))
 }
 .checkCanvas <- function(canvas){
   if(!canvas %in% c("foreground","background"))
     stop (simpleError(sprintf ('%s is invalid. Use "foreground" or "background"', canvas)))
 }
 
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


#############
# TESTS

openSession("/Applications/Cytoscape_v3.8.1/sampleData/sessions/Yeast Perturbation.cys")

#addAnnotationText("test1")
#addAnnotationText("test2", 1000, 1000, name="T2")
addAnnotationText("test!@#$%^3", 1000, 1000, 30, "Helvetica", "bolditalic", "#990000",680,name="T3", canvas="background",z=10)
addAnnotationText("test\n2", 1200, 1000, 30, "Courier New", "bold", "#009900",0,name="T2", canvas="foreground",z=1)
addAnnotationText("test\t1", 1400, 1000, 30, "Comic sans MS", "italic", "#000099",40,name="T1", canvas="foreground",z=0)

ann.list <- getAnnotationList()
ann.uuids <- sapply(ann.list, '[[', 'uuid')

#deleteAnnotation(ann.uuids[1])
#deleteAnnotation(sapply(ann.list, '[[', 'uuid'))
