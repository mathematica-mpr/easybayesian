#' @title lm stan clustered
#' @export
#' @import scales

posteriorplot <- function(model, parameter, cutoff=0, 
                          credibleIntervalWidth=0.95, lessthan=FALSE){
  posteriorSamples <- model$posteriorSamples$posteriorSamplesBeta
  whichParameter <- which(names(posteriorSamples)==parameter)
  posteriorDraws <- posteriorSamples[,whichParameter] # Input
  pointEstimate <- colMeans(posteriorSamples)
  
  df.plot <- data.frame(density = posteriorDraws)
  if(lessthan){
    posteriorProbability <- apply(posteriorSamples, 2, function(x) return(mean(x<cutoff)))
    mid <- cutoff/2
    ds <- density(df.plot$density, from = min(df.plot$density), to = cutoff)
  }else{
    posteriorProbability <- apply(posteriorSamples, 2, function(x) return(mean(x>cutoff)))
    mid <- cutoff + (max(df.plot$density) - cutoff) / 2
    ds <- density(df.plot$density, from = cutoff, to = max(df.plot$density))
  }
  prob <- scales::percent(posteriorProbability[whichParameter])
  ds_data <- data.frame(x = ds$x, y = ds$y)
  credibleInterval <- apply(posteriorSamples, 2, quantile,
                            c((1 - credibleIntervalWidth) / 2,
                              1 - (1 - credibleIntervalWidth) / 2))
  p <- ggplot(df.plot, aes(density)) + geom_density() +
    geom_vline(xintercept = cutoff, linetype = "dotdash") +
    geom_area(data = ds_data,
              aes(x = x, y = y),
              alpha = 0.5,
              fill = "darkgreen") +
    custom_range(c(credibleInterval[1, whichParameter],
                   credibleInterval[2, whichParameter]),
                 pointEstimate[whichParameter]) +
    annotate("text", x=mid, y=(density(df.plot$density, from=mid, to=mid, n=1)$y)/2,
             label=prob, fontface="bold", family="sans") +
    theme_mpr() +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
    xlab("Impact") + ylab("")
  r <- (print(p))
  
  p <- p +
    coord_cartesian(ylim = c(r$panel$ranges[[1]]$y.range[1]*1.05, r$panel$ranges[[1]]$y.range[2]))
}
