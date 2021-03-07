
# devtools::document()

#' find overlap between sets in a list
#'
#' @param setlist list of character vectors.
#' @param xlim vector with 2 numbers, x axis limits for the venn diagram.
#' @param ylim vector with 2 numbers, y axis limits for the venn diagram.
#' @return list with 2 items: a data.frame with information about the groups (sizes, coordinates, etc.), and a data.frame containing the x and y coordinates for the venn diagram
#' @details the function uses geom_path for the dendrogram, so ... takes all arguements that geom_path would also take, such as color, size, etc.
#' @export
#' @examples
#' library(ggeuler)
#' library(ggplot2)
#'
#' set1 <- rownames(subset(mtcars, mpg>18))
#' set2 <- rownames(subset(mtcars, qsec>18))
#' set3 <- rownames(subset(mtcars, cyl<5))
#' carset <- list(highMpg=set1, highQsec=set2, lowCyl=set3)
#'
#' findOverlap(carset)
#'
findOverlap <- function(setlist, xlim, ylim){
  overlap <- setlist
  for(i in 1:length(setlist)){
    overlap <- lapply(setlist, function(x) lapply(overlap, function(y) x[x %in% y])) #compare everything
    overlap <- unlist(overlap, recursive = F) #flatten the list
    betterNames <- lapply(names(overlap), function(x) sort(unlist(strsplit(x, "\\."))) )
    betterNames <- lapply(betterNames, function(x) paste( x[!duplicated(x)] , collapse="." ) ) #remove redundant comparisons
    betternames <- unlist(betterNames)
    overlap <- overlap[!duplicated(betterNames)]
    betterNames <- unlist(betterNames[!duplicated(betterNames)])
    names(overlap) <- betterNames
  }

  contents <- sapply(overlap, function(x) paste0(x, collapse=","))
  sizes <- sapply(overlap, length)
  groups <- unlist( lapply(betterNames, function(x) strsplit(x, "\\.")), recursive=F)

  overlap2 <- overlap
  for(i in length(setlist):1){
    groupi <- groups[sapply(groups, length)==i]
    groupsmalleri <- groups[sapply(groups, length)<i]
    groupi2 <- unlist(lapply(groupi, function(x) paste(x, collapse=".")))
    groupsmalleri2 <- unlist(lapply(groupsmalleri, function(x) paste(x, collapse=".")))

    for(z in 1:length(groupi2)){

      overlap2 <- mapply(function(x,y){
        if(y %in% groupsmalleri2){
          setdiff(x, overlap[[ groupi2[[z]] ]])
        }else{
          x
        }
      }, x=overlap2, y=names(overlap2), SIMPLIFY=FALSE )

    }
  }

  contents <- sapply(overlap2, function(x) paste0(x, collapse=","))
  sizes <- sapply(overlap2, length)
  labels <- sapply(names(sizes), function(x) if(x %in% names(setlist)){x}else{NA})
  # groups <- unlist( lapply(betterNames, function(x) strsplit(x, "\\.")), recursive=F)

  coordinates <- switch(
    as.character(length(setlist)),
    "2"=data.frame(x = c(a1=-0.5, a12=0.5, a2=1.5),
                   y = c(a1=0,    a12=0,   a2=0)),
    "3"=data.frame(x = c(a1=-0.5, a12=0.5,  a13=-.1, a123=0.5, a2=1.5, a23=1.1, a3=0.5),
                   y = c(a1=0,    a12=-1/4, a13=0.7, a123=1/3, a2=0,   a23=0.7, a3=1.5)),
    "4"=data.frame(x = c(a1=-1.7, a12=0, a13=-1, a14=-.9, a123=.4, a124=-.4, a134=-.5, a1234=0, a2=1.7, a23=1,   a24=.9, a234=.5, a3=-.7, a34=0,   a4=.7),
                   y = c(a1=.5, a12=-1.3, a13=1, a14=-.7, a123=-1, a124=-1, a134=.5, a1234=-.2, a2=.5,  a23=-.7, a24=1,  a234=.5, a3=1.6, a34=1.2, a4=1.6)),
    "5"=data.frame(x = c(a1=0, a12=.6, a13=-.2, a14=-.2, a15=-.85, a123=.15, a124=.75, a125=-.9, a134=.3, a135=-.6, a145=-.5, a1234=.5, a1235=-.5, a1245=-.7, a1345=.4, a12345=.1, a2=1.7, a23=1.4, a24=1.05, a25=-1.06, a234=1.1, a235=1.25, a245=-.99, a2345=1.1, a3=1.2, a34=.54, a35=1.1, a345=.85, a4=-.6, a45=-1, a5=-1.4),
                   y = c(a1=1.3, a12=1, a13=.98, a14=-1.37, a15=.6, a123=.9, a124=.84, a125=0, a134=-1.35, a135=.7, a145=-1.06, a1234=.75, a1235=.5, a1245=-.6, a1345=-1.14, a12345=-.2, a2=.4, a23=-.3, a24=.65, a25=-.26, a234=.3, a235=-.55, a245=-.6, a2345=-.3, a3=-1.35, a34=-1.4, a35=-.9, a345=-1, a4=-1.66, a45=-.8, a5=.2)),
    "6"=data.frame(x = c(a1=0, a12=0, a13=0, a14=0, a15=0, a123=0, a124=0, a125=0, a134=0, a135=0, a145=0, a1234=0, a1235=0, a1245=0, a1345=0, a12345=0, a2=0, a23=0, a24=0, a25=0, a234=0, a235=0, a245=0, a2345=0, a3=-0, a34=0, a35=0, a345=0, a4=0, a45=0, a5=0),
                   y = c(a1=0, a12=0, a13=0, a14=0, a15=0, a123=0, a124=0, a125=0, a134=0, a135=0, a145=0, a1234=0, a1235=0, a1245=0, a1345=0, a12345=0, a2=0, a23=0, a24=0, a25=0, a234=0, a235=0, a245=0, a2345=0, a3=-0, a34=0, a35=0, a345=0, a4=0, a45=0, a5=0))
  )

  rot4 <- .9
  circleParams <- switch(
    as.character(length(setlist)),
    "2"=list(x=c(0,1), y=c(0,0), l=1, r=0),
    "3"=list(x=c(0,1,0.5), y=c(0,0,1), l=1, r=0),
    "4"=list(x=c(-.7,.7,-.1,.1), y=c(0,0,.3,.3), l=1.8, r=c(-rot4, rot4, -rot4, rot4)),
    "5"=list(x=c(0,.4,.4,.1,-.18), y=c(0,0,-.3,-.5,-.2), l=1.6, r=c(.5*pi, .1*pi, .7*pi, .3*pi, .9*pi))
  )
  dfcircles <- mapply(function(label,x,y,l,r) .ellipse(group=label,x=x,y=y,r=r,radiusX=l) , label=names(setlist),
                      x=circleParams$x, y=circleParams$y,
                      l=circleParams$l, r=circleParams$r,
                      SIMPLIFY=FALSE)
  dfcircles <- do.call(rbind, dfcircles)
  numbers <- data.frame(group=names(sizes), label=labels, size=sizes, content=contents, coordinates)

  #resize everything
  xdelta <- abs(diff(xlim))
  ydelta <- abs(diff(ylim))

  numbers$x <- numbers$x - min(dfcircles$x)
  numbers$y <- numbers$y - min(dfcircles$y)
  dfcircles$x <- dfcircles$x - min(dfcircles$x)
  dfcircles$y <- dfcircles$y - min(dfcircles$y)
  numbers$x <- numbers$x / max(dfcircles$x) * xdelta + min(xlim)
  numbers$y <- numbers$y / max(dfcircles$y) * ydelta + min(ylim)
  dfcircles$x <- dfcircles$x / max(dfcircles$x) * xdelta + min(xlim)
  dfcircles$y <- dfcircles$y / max(dfcircles$y) * ydelta + min(ylim)

  return(list(table=numbers, ellipses=dfcircles))
}

.ellipse <- function(group, x=0, y=0, r=0, npoints=100, radiusX=1, radiusY=1){
  positions <- seq(0,2*pi, length.out=npoints)
  output <- data.frame(
    group=as.character(group),
    x=radiusX*cos(positions),
    y=radiusY*sin(positions)
  )
  xcoord <- output$x
  ycoord <- output$y
  output$x <- (xcoord * cos(r) - ycoord * sin(r)) +x
  output$y <- (ycoord * cos(r) + xcoord * sin(r)) +y

  return( output )
}
