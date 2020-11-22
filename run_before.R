#options(knitr.kable.NA = "")

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

m_scale_colour_viridis_d <- function(..., alpha = 1, begin = 0, end = 0.9, direction = 1, 
                                     option = "D", aesthetics = "colour") {
  scale_colour_viridis_d(..., alpha = alpha, begin = begin, end = end, direction = direction, 
                         option = option, aesthetics = aesthetics)
}

m_scale_fill_viridis_d <- function(..., alpha = 1, begin = 0, end = 0.9, direction = 1, 
                                     option = "D", aesthetics = "fill") {
  scale_fill_viridis_d(..., alpha = alpha, begin = begin, end = end, direction = direction, 
                         option = option, aesthetics = aesthetics)
}

scale_colour_discrete <- m_scale_colour_viridis_d
scale_fill_discrete <- m_scale_fill_viridis_d
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

write_GLM_equation <- function(mod, digits=NULL, include_sde = TRUE, dv_name, iv_names) {
  if(missing(dv_name)) dv_name <- attr(mod$terms,"variables")[[2]]
  coefs <- coefficients(mod)
  intercept_included <- names(coefs)[1] == "(Intercept)"
  if(missing(iv_names) & length(coefs) > as.numeric(intercept_included)) {
    iv_names <- names(coefs)
    if(intercept_included) iv_names <- iv_names[-1]
  }
  sd_e <- sqrt(sum(residuals(mod)^2)/(nrow(mod$model) - length(coefs)))
  if(is.null(digits)) digits <- options("digits")$digits
  out <- paste0("\\texttt{",dv_name,"}_i = ")
  idx <- 1
  if(intercept_included) {
    out <- paste0(out, format(coefs[idx],digits=digits))
    idx <- idx + 1
  }
  if(intercept_included & length(coefs) > 1 | !intercept_included & length(coefs) > 0) {
    for(i in idx:length(coefs)) {
      if(i == 1) {
        out <- paste0(out, ifelse(coefs[i] > 0,""," - "),format(abs(coefs[i]),digits=digits), " \\times \\texttt{", iv_names[ifelse(intercept_included, i - 1, i)], "}_i ")
      } else {
        out <- paste0(out, ifelse(coefs[i] > 0," + "," - "),format(abs(coefs[i]),digits=digits), " \\times \\texttt{", iv_names[ifelse(intercept_included, i - 1, i)], "}_i ")
      }
    }
  }
  
  out <- paste0(out," + \\hat{\\epsilon}_i ")
  if(include_sde) {
    out <- paste0(out, "\\quad \\quad \\hat{\\epsilon}_i \\sim \\mathbf{Normal}(0, ", format(sd_e,digits=digits), ")")
  }
  out
}

#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}
