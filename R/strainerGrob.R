#' @title Creates stan file
#' @import grid
#' @importFrom scales rescale

strainerGrob <- function(range = c(-1,2), midpoint=0.35,
                         vpos=unit(5,"mm"),
                         pad = unit(1.5,"mm"),
                         gp=gpar(lty=2, lwd=2)){
  labels <- as.character(c(round(range[1],2), round(midpoint,2), round(range[2],2)))
  xpos <- c(0, scales::rescale(midpoint, from=range, to=c(0,1)), 1)
  sg <- segmentsGrob(0, unit(1,"npc") - vpos, 1, unit(1,"npc") - vpos, gp=gp)
  tg <- textGrob(labels, x = unit(xpos, "npc") + c(-1,0,1)*pad,
                 hjust = c(1,0.5,0),
                 vjust=c(0.5,0,0.5), y=unit(1,"npc") - vpos + c(0,1,0)*pad)
  pg <- pointsGrob(x=xpos[2], y=unit(1,"npc") - vpos, pch = 19, gp = gpar(cex=0.5))

  grobTree(sg, pg, tg)
}


# wrapper to ensure that both geom and grob are in sync with x values
custom_range <- function(range = c(-1,2), midpoint=0.35, ...){
  sg <- strainerGrob(range=range, midpoint=midpoint, ...)
  annotation_custom(sg, xmin = range[1], xmax = range[2], ymin=-Inf, ymax=0)
}
