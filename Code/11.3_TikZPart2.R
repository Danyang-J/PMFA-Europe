## FINAL FORMATTING FOR TIKZ ###################################################################
Names.agg <- unique(c(names(Flow.List[[mat]]),unlist(sapply(Flow.List[[mat]], names))))

attribute.width <- function(mean){
  
  if(mean > 10^1){
    width <- 10
  } else if(mean > 10^0){
    width <- 8
  } else if(mean > 10^-1){
    width <- 6
  } else if(mean > 10^-2){
    width <- 4
  } else if(mean > 10^-3){
    width <- 2
  } else if(mean == 0){
    width <- 0
  } else {
    width <- 1
  }
  
  return(width)
}


For.TikZ <- matrix(0, length(Names.agg)*(length(Names.agg)+1), 2*10,
                   dimnames = list(paste0(rep(c(Names.agg, "External"), each = length(Names.agg))," to ",Names.agg),
                                   c(paste0("Text_",c(Materials,"Plastic","AllPE","AllPS")),
                                     paste0("Width_", c(Materials,"Plastic","AllPE","AllPS")))))

# copy the matrix for checking without rounding
NoRounding <- For.TikZ

for(comp in Names.agg){
  
  if(all(is.null(Flow.List[["LDPE"]][[comp]]),
         is.null(Flow.List[["HDPE"]][[comp]]),
         is.null(Flow.List[["PP"]][[comp]]),
         is.null(Flow.List[["PS"]][[comp]]),
         is.null(Flow.List[["EPS"]][[comp]]),
         is.null(Flow.List[["PVC"]][[comp]]),
         is.null(Flow.List[["PET"]][[comp]]))){
    next
  }
  
  for(dest in Names.agg){
    
    # normal materials
    for(mat in Materials){
      if(!is.null(Flow.List[[mat]][[comp]][[dest]])){
        
        For.TikZ[paste(comp,"to",dest),paste0("Text_",mat)] <-
          sci.not(Flow.List[[mat]][[comp]][[dest]]*1000, sep = "$\\pm$")
        
        For.TikZ[paste(comp,"to",dest),paste0("Width_",mat)] <-
          attribute.width(mean(Flow.List[[mat]][[comp]][[dest]]))
        
        
        NoRounding[paste(comp,"to",dest),paste0("Text_",mat)] <-
          mean(Flow.List[[mat]][[comp]][[dest]])*1000
        
        NoRounding[paste(comp,"to",dest),paste0("Width_",mat)] <-
          For.TikZ[paste(comp,"to",dest),paste0("Width_",mat)]
        
      }
    }
    
    # aggregated over all polymers
    to.save <- ( if(is.null(Flow.List[["LDPE"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["LDPE"]][[comp]][[dest]] } +
                   if(is.null(Flow.List[["HDPE"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["HDPE"]][[comp]][[dest]] } +
                   if(is.null(Flow.List[["PP"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["PP"]][[comp]][[dest]] } +
                   if(is.null(Flow.List[["PS"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["PS"]][[comp]][[dest]] } +
                   if(is.null(Flow.List[["EPS"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["EPS"]][[comp]][[dest]] } +
                   if(is.null(Flow.List[["PVC"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["PVC"]][[comp]][[dest]] } +
                   if(is.null(Flow.List[["PET"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["PET"]][[comp]][[dest]] } ) 
    
    For.TikZ[paste(comp,"to",dest),"Text_Plastic"] <- sci.not(to.save*1000, sep = "$\\pm$")
    For.TikZ[paste(comp,"to",dest),"Width_Plastic"] <- attribute.width(mean(to.save))
    
    NoRounding[paste(comp,"to",dest),"Text_Plastic"] <- mean(to.save)*1000
    NoRounding[paste(comp,"to",dest),"Width_Plastic"] <- For.TikZ[paste(comp,"to",dest),"Width_Plastic"]
    
    
    # aggregated for some
    to.save <- ( if(is.null(Flow.List[["LDPE"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["LDPE"]][[comp]][[dest]] } +
                   if(is.null(Flow.List[["HDPE"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["HDPE"]][[comp]][[dest]] } ) 
    
    For.TikZ[paste(comp,"to",dest),"Text_AllPE"] <- sci.not(to.save*1000, sep = "$\\pm$")
    For.TikZ[paste(comp,"to",dest),"Width_AllPE"] <- attribute.width(mean(to.save))
    
    NoRounding[paste(comp,"to",dest),"Text_AllPE"] <- mean(to.save)*1000
    NoRounding[paste(comp,"to",dest),"Width_AllPE"] <- For.TikZ[paste(comp,"to",dest),"Width_AllPE"]
    
    
    to.save <- ( if(is.null(Flow.List[["PS"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["PS"]][[comp]][[dest]] } +
                   if(is.null(Flow.List[["EPS"]][[comp]][[dest]])){ rep(0,SIM) } else { Flow.List[["EPS"]][[comp]][[dest]] } )
    
    For.TikZ[paste(comp,"to",dest),"Text_AllPS"] <- sci.not(to.save, sep = "$\\pm$")
    For.TikZ[paste(comp,"to",dest),"Width_AllPS"] <- attribute.width(mean(to.save))
    
    NoRounding[paste(comp,"to",dest),"Text_AllPS"] <- mean(to.save)*1000
    NoRounding[paste(comp,"to",dest),"Width_AllPS"] <- For.TikZ[paste(comp,"to",dest),"Width_AllPS"]
    
  }
  
  # include the width of the external flows for testing in the tex file
  for(mat in Materials){
    
    # if any flows exist out of comp, attribute width 1 (just for testing in tex file, nothing else)
    if(!all(apply(do.call("rbind",Flow.List[[mat]][[comp]]),2,sum) == 0)){
      For.TikZ[paste("External","to",comp),paste0("Width_", mat)] <- 1
      NoRounding[paste("External","to",comp),paste0("Width_", mat)] <- 1
    }
    
  }
  
  # for aggregated
  if(For.TikZ[paste("External","to",comp),"Width_PS"] == "1" | For.TikZ[paste("External","to",comp),"Width_EPS"] == "1"){
    For.TikZ[paste("External","to",comp),"Width_AllPS"] <- 1  
    NoRounding[paste("External","to",comp),"Width_AllPS"] <- 1  
  }
  
  if(For.TikZ[paste("External","to",comp),"Width_LDPE"] == "1" | For.TikZ[paste("External","to",comp),"Width_HDPE"] == "1"){
    For.TikZ[paste("External","to",comp),"Width_AllPE"] <- 1  
    NoRounding[paste("External","to",comp),"Width_AllPE"] <- 1  
  }
  
  # no need for testing for plastic
  For.TikZ[paste("External","to",comp),"Width_Plastic"] <- 1  
  NoRounding[paste("External","to",comp),"Width_Plastic"] <- 1  
  
}

# remove unexisting flows
For.TikZ <- For.TikZ[!apply(For.TikZ[,-1],1,function(x) all(x %in% c("0", "0$\\pm$0"))),]
NoRounding <- NoRounding[!apply(NoRounding[,-1],1,function(x) all(x %in% c("0", "0$\\pm$0"))),]

NoRounding <- apply(NoRounding,c(1,2), function(x) round(as.numeric(x),digits = 2))

write(t(rbind(c("Hoi",colnames(For.TikZ)),cbind(rownames(For.TikZ),For.TikZ))),
      ncolumns = ncol(For.TikZ)+1, sep = ",",
      file = paste0("Charts/flow_",c,".txt"))

write(t(rbind(c("Hoi",colnames(NoRounding)),cbind(rownames(NoRounding),NoRounding))),
      ncolumns = ncol(NoRounding)+1, sep = ",",
      file = paste0("Charts/flow_norounding_",c,".txt"))