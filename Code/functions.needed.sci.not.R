sci.not <- function(data, precision=2, sep = c("$\\pm$"," ± ")[2]){
  
  # change options so that there is a penalty for exponents
  options("scipen"=100)
  
  # round the standard deviation to the wanted precision
  SD <- signif(sd(data),precision)
  
  # translate into character, so that significant digits are easier to conserve
  # split string into single character vector
  sd.string <- strsplit(as.character(SD), "")[[1]]
  
  # stop if there still is an exponent in the string
  if("e" %in% sd.string){
    stop("You need to increase the penalty for exponents.")
  }
  
  if(SD > 1){
    
    # find position of comma
    comma <- which(sd.string == ".")
    # remove the digits after the comma for simplicity (if there is a comma)
    if(length(comma) > 0){
      sd.string <- sd.string[1:(comma-1)]
    }
    # find position of the first digit (-3 corresponds to 100, -2 to 10, -1 to 1)
    pos <- -tail(which(rev(sd.string) != 0),1)
    
  } else if(SD < 1 & SD > 0){
    
    # find position of comma
    comma <- which(sd.string == ".")
    # find positions of the non-zero characters
    notzero <- which(sd.string != 0)
    # find position of the first digit after the comma
    pos <- notzero[notzero != comma][1] - comma -1
    
  } else {
    
    # if the standard deviation is 0, give only one significant number
    return(paste0(signif(mean(data),1),sep,signif(sd(data),1)))
    
  }
  
  # round the mean to one figure higher precision than the standard deviation 
  ############################### REVISED ####################################### 3 sig. fig. for mean, 2 sig. fig. for sd 
  MEAN <- signif(mean(data),precision+1)

  # give result
  stringtoreturn <- paste0(MEAN,sep,SD)
  
  # change options back
  options("scipen"=0)
  
  stringtoreturn
}
