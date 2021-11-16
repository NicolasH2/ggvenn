# devtools::document()

#' create a venn diagram in ggplot2
#'
#' Imports:
#' ggplot2
#'
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_text
#' @import ggplot2
#'
#' @param setlist list of character vectors.
#' @param textsize integer, defining the text size of the numbers in the graph.
#' @param xlim vector with 2 numbers, x axis limits for the venn diagram.
#' @param ylim vector with 2 numbers, y axis limits for the venn diagram.
#' @param fillCol boolean, should the fill color indicate the list items?
#' @param borderCol boolean, should the border color indicate the list items?
#' @param fixedCoords boolean, should coord_fixed be applied? If TRUE, makes the graph into a square.
#' @return a ggplot2 layer objects (geom_polyon for the venn diagram) that can directly be added to a ggplot2 object
#' @details the function uses geom_polygon for the dendrogram, so ... takes all arguements that geom_polygon would also take, such as color, size, alpha, etc.
#' @export
#' @examples
#' library(ggvenn)
#' library(ggplot2)
#'
#' set1 <- rownames(subset(mtcars, mpg>18))
#' set2 <- rownames(subset(mtcars, qsec>18))
#' set3 <- rownames(subset(mtcars, cyl<5))
#' carset <- list(highMpg=set1, highQsec=set2, lowCyl=set3)
#'
#' ggplot2::ggplot() + geom_venn(carset)
#'
geom_venn <- function(setlist, xlim=c(0,1), ylim=c(0,1), numbersize=4, numbercolor="black", textsize=4, textcolor="black", fillCol=TRUE, borderCol=FALSE, fixedCoords=TRUE, ...){
  # find overlap between sets, using a ggvenn function
  overlap <- ggvenn::findOverlap(setlist, xlim=xlim, ylim=ylim)
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
