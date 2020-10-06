knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      fig.align = "center")

knitr::opts_hooks$set(echo = function(options) {
  if (options$fig.width < options$fig.height) {
    options$fig.width <- options$fig.height
  }
  options
})

library(sdamr)
library(ggplot2)
scale_colour_discrete <- scale_colour_viridis_d
scale_fill_discrete <- scale_fill_viridis_d
scale_colour_continuous <- scale_colour_viridis_c
scale_fill_continuous <- scale_fill_viridis_c
scale_colour_binned <- scale_colour_viridis_b
scale_fill_binned <- scale_fill_viridis_b

my_theme <- ggplot2::theme_get()

options(digits=3)

### Functions
pretNum <- function(x) {
  prettyNum(x, big.mark=",")
}

pvalue <- function(x, limit = FALSE, limit_below = 1e-3) {
  if(x > 1 | x < 0) stop("that's not a p-value matey")
  if(limit & x < limit_below) return(paste0("<", pvalue(limit_below, limit=FALSE)))
  x <- as.character(prettyNum(x,scientific=FALSE, digits=options()$digits))
  return(sub("0.",".", x))
}