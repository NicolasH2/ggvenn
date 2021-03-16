geom_euler <- function(setlist, xlim=c(0,1), ylim=c(0,1), numbersize=4, numbercolor="black", textsize=4, textcolor="black", fillCol=TRUE, borderCol=FALSE, fixedCoords=TRUE, ...){
  # find overlap between sets, using a ggeuler function
  overlap <- ggeuler::findOverlap(setlist, xlim=xlim, ylim=ylim)
  df <- overlap[["table"]] #table contains labels and numbers with coordinates
  ellipses <- overlap[["ellipses"]] #table contains coordinates for the ellipses to display
  
  #======================================================
  ## layer 1: ellipses
  venn <- ggplot2::layer(
    data = ellipses,
    mapping = ggplot2::aes(x=x, y=y, fill=group, color=group),
    geom = "polygon",
    stat = "identity",
    position = "identity",
    inherit.aes = FALSE,
    params=list(...)
  )
  # control opacity (alpha) and the fillcolor and bordercolor
  userInput <- names(as.list(match.call())) #get all parameters set by the user (to know whether the user set alpha)
  if(!"alpha" %in% userInput & fillCol) venn$aes_params[["alpha"]] <- .4
  if(!fillCol) venn$mapping[["fill"]] <- NULL
  if(!borderCol) venn$mapping[["colour"]] <- NULL
  
  #======================================================
  ## layer 2: numbers
  numbers <- ggplot2::layer(
    data = df,
    mapping = ggplot2::aes(x=x, y=y, label=size),
    geom = "text",
    stat = "identity",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params=list(...)
  )
  # size and color parameters are set to the function parameters (would otherwise be set with the "...")
  # this is necessary to adjust color and size for ellipses, numbers and labels separately
  numbers$aes_params[["size"]] <- textsize
  numbers$aes_params[["colour"]] <- textcolor
  
  #======================================================
  ## layer 3: labels
  df <- df[!is.na(df$label),]
  circellabels <- ggplot2::layer(
    data = df,
    mapping = ggplot2::aes(x=labx, y=laby, label=label),
    geom = "text",
    stat = "identity",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params=list(...)
  )
  # size and color parameters are set to the function parameters (would otherwise be set with the "...")
  # this is necessary to adjust color and size for ellipses, numbers and labels separately
  circellabels$aes_params[["size"]] <- textsize
  circellabels$aes_params[["colour"]] <- textcolor
  
  #======================================================
  ## bring all layers together
  venn <- c(venn, numbers, circellabels)
  
  ## adjust the plot ratio, depending on the number of ellipses. The ratio if only applied if the user leaves fixesCoords=TRUE
  ratio <- abs(diff(xlim)/diff(ylim))
  ratio <- ifelse(length(setlist)==2, ratio/1.5, ifelse(length(setlist)==4, ratio/1.2, ratio))
  if(fixedCoords) venn <- c(venn, ggplot2::coord_fixed(ratio))
  
  return(venn)
}
