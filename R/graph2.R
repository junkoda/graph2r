#
# Functions for publication quality graphs
#

#
# Log Axis Ticks
#

# common styles
cex <- 1.2     # character size
cex.pt <- 0.8  # graph point size
cex.axis.default <- 1.4*cex

# short name for common functions

#' Write to a PDF file based on the script file name hoge.R -> hoge.pdf
#'
#' @param argv = commandArgs()
#' @param width = 6
#' @param height = width
#' @param log = "xy"
#' @examples
#' graph.pdf(commandArgs())
#' @export
graph.pdf <- function(argv, width=6, height=width) {
  iarg <- charmatch("--file=", argv)
  filename <- paste(substring(argv[iarg], 8,
                    nchar(argv[iarg])-1), "pdf", sep="")
  pdf(file=filename, paper="special",
      onefile=T, width=width, height=height)
}

#' Create a frame of the graph
#' @param xrange = c(xmin, xmax)
#' @param yrange = c(ymin, ymax)
#' @export
graph.frame <- function(xrange, yrange, ...) {
  par(las=0)
  plot(1, type="n", xlim=xrange, ylim=yrange,
     xaxt="n", yaxt="n", xlab="", ylab="", ...
     )
}

#' Create axes
#' @param xrange = c(xmin, xmax)
#' @param yrange = c(ymin, ymax)
#' @param xstep, ystep = "F", "log", "exp", "10big"
#' @export
graph.axes <- function(xrange, yrange, xstep, ystep, cex.axis=cex.axis.default, ...) {
  # ystep <- c( )     -- linear axis
  # ystep <- "F"      -- log no axis numbers
  #          "log"    --     default numbers
  #          "exp"    --     exponent only n
  #          "10big"  --     big exponent 10^n
  par(las=1)

  # x-axis
  if(is.character(xstep)) { # log
    plot.axis.log(1, xrange, labels=xstep, cex.axis=cex.axis, ...)
    plot.axis.log(3, xrange, labels=F, ...)
  }
  else {
    if(length(xstep) == 3 && xstep[3] == F)
      plot.axis(1, xrange, step=xstep, labels=F, ...)
    else
      plot.axis(1, xrange, step=xstep, cex.axis=cex.axis, ...)

    plot.axis(3, xrange, step=xstep, labels=F, ...)
  }

  # y-axis
  if(is.character(ystep)) {
    plot.axis.log(2, yrange, labels=ystep, cex.axis=cex.axis, ...)
    plot.axis.log(4, yrange, labels=F, ...)
  }
  else {
    if(length(ystep) == 3 && ystep[3] == F)
      plot.axis(2, yrange, step=ystep, labels=F, ...)
    else
      plot.axis(2, yrange, step=ystep, cex.axis=cex.axis, ...)
    
    plot.axis(4, yrange, step=ystep, labels=F, ...)
  }
}

plot.axis.log <- function(side, range, tck=c(0.01,0.02), labels="log",
                          add.labels=c(), label.shift=0, ...) {
  small.ticks <- c()
  big.ticks <- c()
  for(n in floor(log10(range[1])):ceiling(log10(range[2]))) {
    small.ticks <- c(small.ticks, (2:9)*10^n)
    big.ticks <- c(big.ticks, 10^n)
  }

  small.labels=F
  
  axis(side=side, at=small.ticks, labels=F,
       lwd=0, lwd.ticks=1, tck=tck[1], ...)

  if(length(add.labels) > 0) {
    axis(side=side, at=add.labels, labels=formatC(add.labels),
         lwd=0, lwd.ticks=0, ...)
  }

  if(labels == "F")
    labels <- F
  else if(labels == "log")
    labels <- formatC(big.ticks)
  else if(labels == "exp")
    labels <- formatC(log10(big.ticks))
  else if(labels == "10")
    labels <- sapply(log10(big.ticks),
                     function(i)(eval(substitute(expression(10^n),list(n=i)))))
  else if(labels == "10big")
    labels <- sapply(log10(big.ticks),
                 function(i)(eval(substitute(expression({}[10]*n),list(n=i)))))

  axis(side=side, at=big.ticks, labels=F,
       lwd=0, lwd.ticks=1, tck=tck[2], ...)
  axis(side=side, at=big.ticks, labels=labels,
       lwd=0, lwd.ticks=-1, tck=tck[2], line=label.shift, ...)
  
}

plot.axis <- function(side, range, step=c(1,10), tck=c(0.01,0.02),
                      labels=T, scale=1, label.shift=0, ...)
{
  t.start <- floor(range[1]/step[1])*step[1]
  t.end   <- ceiling(range[2]/step[1])*step[1]
  small.ticks <- seq(t.start, t.end, step[1])

  t.start <- floor(range[1]/step[2])*step[2]
  t.end   <- ceiling(range[2]/step[2])*step[2]
  big.ticks   <- seq(t.start, t.end, step[2])
  
  small.ticks <- small.ticks[ ! small.ticks %in% big.ticks ]

  axis(side=side, at=small.ticks*scale, labels=F, lwd=0, lwd.ticks=1,
       tck=tck[1], ...)

  if(labels == "T")
    labels <- formatC(big.ticks)
  else if(labels == "exp")
    labels <- formatC(log10(big.ticks))
  else if(labels == "10")
    labels <- sapply(log10(big.ticks),
                     function(i)(eval(substitute(expression(10^n),list(n=i)))))
  
  axis(side=side, at=big.ticks*scale, labels=F,
       lwd=0, lwd.ticks=1, tck=tck[2], ...)

  axis(side=side, lwd.ticks=-1, at=big.ticks*scale, labels=labels,
       lwd=0, line=label.shift, ...)
  
}

plot.axis.at <- function(side, d, step=10, tck=c(0.01,0.02),
                        labels=T)
{
  d.big <- d[d$label %% step == 0 | d$label %% step > 0.999*step,]
  big.ticks <- d.big$at

  small.ticks <- d$at[ ! d$at %in% big.ticks ]

  axis(side=side, at=small.ticks, labels=F, lwd=0, lwd.ticks=1, tck=tck[1])

  if(labels)
    labels <- formatC(d.big$label)
  
  axis(side=side, at=big.ticks, labels=labels,
       lwd=0, lwd.ticks=1, tck=tck[2], padj= 1)
}
