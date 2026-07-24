##### INTRODUCTION ################################################################################
source("Code/functions.needed.analysis.R")
source("Code/functions.needed.sci.not.R")
library(xlsx)

plot_family <- "sans"

soil.comp <- c("UrbanSoilMicro",
               "UrbanSoilMacro",
               "AgriculturalSoilMacro",
               "AgriculturalSoilMicro",
               "RuralSoilMacro",
               "RuralSoilMicro",
               "RoadSideMacro",
               "DeepUrbanSoilMicro",
               "BeachSoilMacro",
               "BeachSoilMicro")

water.comp <- c("SurfaceWaterMicro",
                "SurfaceWaterMacro",
                "CoastalWaterMicro",
                "CoastalWaterMacro",
                "OceanMacro",
                "OceanMicro")

NiceNames <- as.matrix(read.xlsx(paste0("Input/",excel.file), sheetName = "Label"))
rownames(NiceNames) <- NiceNames[,"Name"]

for (c in Systems) {
  # import input data
  load(paste0("Input/InputReady.Mod2_",c,".Rdata"))
  
  # product categories
  PC <- unique(c(names(TC.Norm[["PP"]][["Packaging"]]),
                 names(TC.Norm[["PP"]][["BuildingConstruction"]]),
                 names(TC.Norm[["PP"]][["Automotive"]]),
                 names(TC.Norm[["PP"]][["EEE"]]),
                 names(TC.Norm[["PP"]][["Agriculture"]]),
                 names(TC.Norm[["PP"]][["Other"]]),
                 names(TC.Norm[["PP"]][["Apparel"]]),
                 names(TC.Norm[["PP"]][["HouseholdTextiles"]]),
                 names(TC.Norm[["PP"]][["TechnicalTextiles"]]),
                 names(TC.Norm[["PP"]][["Fishery"]]),
                 names(TC.Norm[["PP"]][["Aquaculture"]]),
                 names(TC.Norm[["PP"]][["FisheryTextiles"]]),
                 names(TC.Norm[["PP"]][["AquacultureTextiles"]])))
  PC <- PC[!PC == "Export"]
  
  # pre-consumer processes
  comp.pre <- c("PrimaryProduction",
                "RecyclateRepelletizing",
                "Transport",
                "FibreProduction",
                "NonTextileManufacturing",
                "TextileManufacturing")
  
  # post-consumer processes
  comp.post <- c("PCPlasticCollection",
                 "PCFibreCollection",
                 "PackagingCollection",
                 "MixedCollection",
                 "CDCollection",
                 "CDIncinerableCollection",
                 "ELVCollection",
                 "ELVTextilesCollection",
                 "WEEECollection",
                 "AgricultureCollection",
                 "TextileCollection",
                 "FishingGearCollection",
                 "PackagingRecycling",
                 "CDRecycling",
                 "LargePartRecycling",
                 "ASRRecycling",
                 "WEEPRecycling",               
                 "AgricultureRecycling",
                 "Incineration")
  
  Mass.mat <- array(NA, c(length(Materials), length(Names), SIM),
                    dimnames = list(Materials, Names, NULL))
  
  for(mat in Materials){
    load(paste0("Results/Emissions/OutputMass_",c,"_",mat,".Rdata"))
    for(comp in Names){
      Mass.mat[mat,comp,] <- Mass[comp,]  
    }
  }
  
  ### SET UP VARIABLES ##############################################################################
  
  # compartments after which to stop assessing emissions
  PC.stop <- comp.post
  
  # create a list to store the emissions
  EF <- sapply(c("Pre-consumer processes", PC, "Post-consumer processes"), function(x) NULL)
  for(i in 1:length(PC)){
    EF[[i]] <- sapply(Materials, function(x) NULL)
  }
  
  EF.mat <- array(0, c(4,length(Materials),length(EF),SIM),
                  dimnames = list(c("Soil", "Soil (MP)", "Water", "Water (MP)"), Materials, names(EF), NULL))
  
  ### EMISSIONS FROM PRODUCTS #######################################################################
  
  for(comp in PC){
    for(mat in Materials){
      EF[[comp]][[mat]] <- find.fc(comp = comp,
                                   mass = Mass.mat[mat,comp,],
                                   TC.Distr = TC.Norm[[mat]],
                                   verbose = F,
                                   stop.at = PC.stop)
      
      for(dest in c("UrbanSoilMicro","RuralSoilMicro","RoadSideMicro","AgriculturalSoilMicro", "DeepUrbanSoilMicro","BeachSoilMicro")){
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Soil (MP)",mat,comp,] <- EF.mat["Soil (MP)",mat,comp,] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for(dest in c("UrbanSoilMacro","RuralSoilMacro","RoadSideMacro","AgriculturalSoilMacro","BeachSoilMacro")){
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Soil",mat,comp,] <- EF.mat["Soil",mat,comp,] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for (dest in c("SurfaceWaterMicro","CoastalWaterMicro","OceanMicro")) {
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Water (MP)",mat,comp,] <- EF.mat["Water (MP)",mat,comp,] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for (dest in c("SurfaceWaterMacro","CoastalWaterMacro","OceanMacro")) {
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Water",mat,comp,] <- EF.mat["Water",mat,comp,] + EF[[comp]][[mat]][[dest]]
        }
      }
      
    }
  }
  
  ### EMISSIONS OUTSIDE OF CONSUMPTION ##############################################################
  
  ### PRODUCTION AND MANUFACTURING
  
  for(comp in comp.pre){
    
    # remove the compartment of interest from the stop compartments
    comp.stop <- c(comp.pre, PC, comp.post)
    comp.stop <- comp.stop[-which(comp.stop == comp)]
    
    for(mat in Materials){
      
      EF[[comp]][[mat]] <- find.fc(comp = comp,
                                   mass = Mass.mat[mat,comp,],
                                   TC.Distr = TC.Norm[[mat]],
                                   verbose = F,
                                   stop.at = comp.stop)
      
      for(dest in c("UrbanSoilMicro","RuralSoilMicro","RoadSideMicro","AgriculturalSoilMicro","DeepUrbanSoilMicro","BeachSoilMicro")){
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Soil (MP)",mat,"Pre-consumer processes",] <- EF.mat["Soil (MP)",mat,"Pre-consumer processes",] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for(dest in c("UrbanSoilMacro","RuralSoilMacro","RoadSideMacro","AgriculturalSoilMacro","BeachSoilMacro")){
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Soil",mat,"Pre-consumer processes",] <- EF.mat["Soil",mat,"Pre-consumer processes",] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for (dest in c("SurfaceWaterMicro","CoastalWaterMicro","OceanMicro")) {
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Water (MP)",mat,"Pre-consumer processes",] <- EF.mat["Water (MP)",mat,"Pre-consumer processes",] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for (dest in c("SurfaceWaterMacro","CoastalWaterMacro","OceanMacro")) {
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Water",mat,"Pre-consumer processes",] <- EF.mat["Water",mat,"Pre-consumer processes",] + EF[[comp]][[mat]][[dest]]
        }
      }
      
    }
  }
  
  
  ### END-OF-LIFE
  
  for(comp in comp.post){
    
    # remove the compartment of interest from the stop compartments
    comp.stop <- comp.post
    comp.stop <- comp.stop[-which(comp.stop == comp)]
    
    for(mat in Materials){
      
      EF[[comp]][[mat]] <- find.fc(comp = comp,
                                   mass = Mass.mat[mat,comp,],
                                   TC.Distr = TC.Norm[[mat]],
                                   verbose = F,
                                   stop.at = comp.stop)
      
      for(dest in c("UrbanSoilMicro","RuralSoilMicro","RoadSideMicro","AgriculturalSoilMicro","DeepUrbanSoilMicro","BeachSoilMicro")){
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Soil (MP)",mat,"Post-consumer processes",] <- EF.mat["Soil (MP)",mat,"Post-consumer processes",] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for(dest in c("UrbanSoilMacro","RuralSoilMacro","RoadSideMacro","AgriculturalSoilMacro","BeachSoilMacro")){
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Soil",mat,"Post-consumer processes",] <- EF.mat["Soil",mat,"Post-consumer processes",] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for (dest in c("SurfaceWaterMicro","CoastalWaterMicro","OceanMicro")) {
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Water (MP)",mat,"Post-consumer processes",] <- EF.mat["Water (MP)",mat,"Post-consumer processes",] + EF[[comp]][[mat]][[dest]]
        }
      }
      
      for (dest in c("SurfaceWaterMacro","CoastalWaterMacro","OceanMacro")) {
        if(!is.null(EF[[comp]][[mat]][[dest]])){
          EF.mat["Water",mat,"Post-consumer processes",] <- EF.mat["Water",mat,"Post-consumer processes",] + EF[[comp]][[mat]][[dest]]
        }
      }
      
    }
  }
  
  ### BAR PLOT ########################################################################
  
  library(ggplot2)
  library(patchwork)
  
  # color_vector <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
  #                   "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")
  
  ## Soil MP
  
  cp.order <- names(sort(apply(apply(EF.mat["Soil (MP)",,,],c(2,3),sum),1,mean)))
  lab <- sci.not(apply(EF.mat["Soil (MP)",,,],3,sum)*1000)
  labs <- cp.order <- cp.order[(length(cp.order)-9):length(cp.order)]
  labs[labs %in% PC] <- NiceNames[labs[labs %in% PC], "MediumLabel"]
  mean.cal <- apply(EF.mat["Soil (MP)",,cp.order,],c(1,2),mean)*1000
  sd.cal <- apply(EF.mat["Soil (MP)",,cp.order,],c(1,2),sd)*1000
  
  colnames(mean.cal) <- labs
  colnames(sd.cal) <- labs
  
  data <- data.frame(
    polymer = rep(rownames(mean.cal), ncol(mean.cal)),
    prod = rep(colnames(mean.cal), each = nrow(mean.cal)),
    mean = as.vector(mean.cal),
    sd = as.vector(sd.cal)
  )
  
  mean_sum <- aggregate(mean ~ prod, data = data, FUN = sum)
  sd_sum <- aggregate(sd ~ prod, data = data, FUN = sum)
  data_sd <- merge(mean_sum, sd_sum)
  
  # Reorder the dataframe
  
  data$prod <- with(data,reorder(prod, mean, FUN = sum))
  data$polymer <- factor(data$polymer, levels = c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET"))
  
  # Store results for output
  data.to.print.1 <- data
  data.to.print.1$comp <- rep('Soil MP',length(data$polymer))
  
  # Bar plot
  
  p1 <- ggplot(data, aes(x = prod, y = mean)) +
    geom_bar(aes(fill = polymer), stat = "identity", position = "stack", color = "black", width = 0.8) +  
    #scale_fill_manual(values = color_vector) + 
    scale_fill_brewer(palette="Set3")+
    theme_bw()+
    labs(x = "", y = "Mass (t)", fill = "Polymer") +
    ggtitle("Microplastic emissions to soil") +
    geom_pointrange(data = data_sd, aes(ymin = mean - sd, ymax = mean + sd), colour = "black", alpha = 0.9, size = 0.2) +
    ylim(0,NA) +
    coord_flip() +
    annotate ("text", x = -Inf, y = Inf, label = paste0("Σ = ",lab," t"), hjust = 1.05, vjust = -0.7)+
    theme(text = element_text(family = plot_family),
          axis.text = element_text(color = "black"))
  
  p1
  
  ####### Soil MaP
  
  cp.order <- names(sort(apply(apply(EF.mat["Soil",,,],c(2,3),sum),1,mean)))
  lab <- sci.not(apply(EF.mat["Soil",,,],3,sum)*1000)
  labs <- cp.order <- cp.order[(length(cp.order)-9):length(cp.order)]
  labs[labs %in% PC] <- NiceNames[labs[labs %in% PC], "MediumLabel"]
  mean.cal <- apply(EF.mat["Soil",,cp.order,],c(1,2),mean)*1000
  sd.cal <- apply(EF.mat["Soil",,cp.order,],c(1,2),sd)*1000
  
  colnames(mean.cal) <- labs
  colnames(sd.cal) <- labs
  
  data <- data.frame(
    polymer = rep(rownames(mean.cal), ncol(mean.cal)),
    prod = rep(colnames(mean.cal), each = nrow(mean.cal)),
    mean = as.vector(mean.cal),
    sd = as.vector(sd.cal)
  )
  
  mean_sum <- aggregate(mean ~ prod, data = data, FUN = sum)
  sd_sum <- aggregate(sd ~ prod, data = data, FUN = sum)
  data_sd <- merge(mean_sum, sd_sum)
  
  # Reorder the dataframe
  
  data$prod <- with(data,reorder(prod, mean, FUN = sum))
  data$polymer <- factor(data$polymer, levels = c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET"))
  
  # Store results for output
  data.to.print.2 <- data
  data.to.print.2$comp <- rep('Soil MaP',length(data$polymer))
  
  # Bar plot
  
  p2 <- ggplot(data, aes(x = prod, y = mean)) +
    geom_bar(aes(fill = polymer), stat = "identity", position = "stack", color = "black", width = 0.8) +  
    #scale_fill_manual(values = color_vector) + 
    scale_fill_brewer(palette="Set3")+
    theme_bw()+
    labs(x = "", y = "Mass (t)", fill = "Polymer") +
    ggtitle("Macroplastic emissions to soil") +
    geom_pointrange(data = data_sd, aes(ymin = mean - sd, ymax = mean + sd), colour = "black", alpha = 0.9, size = 0.2) +
    ylim(0,NA) +
    coord_flip() +
    annotate ("text", x = -Inf, y = Inf, label = paste0("Σ = ",lab," t"), hjust = 1.05, vjust = -0.7)+
    theme(text = element_text(family = plot_family),
          axis.text = element_text(color = "black"))
  
  p2
  
  ####### Water MP
  
  cp.order <- names(sort(apply(apply(EF.mat["Water (MP)",,,],c(2,3),sum),1,mean)))
  lab <- sci.not(apply(EF.mat["Water (MP)",,,],3,sum)*1000)
  labs <- cp.order <- cp.order[(length(cp.order)-9):length(cp.order)]
  labs[labs %in% PC] <- NiceNames[labs[labs %in% PC], "MediumLabel"]
  mean.cal <- apply(EF.mat["Water (MP)",,cp.order,],c(1,2),mean)*1000
  sd.cal <- apply(EF.mat["Water (MP)",,cp.order,],c(1,2),sd)*1000
  
  colnames(mean.cal) <- labs
  colnames(sd.cal) <- labs
  
  data <- data.frame(
    polymer = rep(rownames(mean.cal), ncol(mean.cal)),
    prod = rep(colnames(mean.cal), each = nrow(mean.cal)),
    mean = as.vector(mean.cal),
    sd = as.vector(sd.cal)
  )
  
  mean_sum <- aggregate(mean ~ prod, data = data, FUN = sum)
  sd_sum <- aggregate(sd ~ prod, data = data, FUN = sum)
  data_sd <- merge(mean_sum, sd_sum)
  
  # Avoid disappearing error bars
  data_sd_setlimit <- data_sd %>%
    mutate(sd = if_else(data_sd$mean>data_sd$sd,sd,mean))
  
  # Reorder the dataframe
  
  data$prod <- with(data,reorder(prod, mean, FUN = sum))
  data$polymer <- factor(data$polymer, levels = c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET"))
  
  # Store results for output
  data.to.print.3 <- data
  data.to.print.3$comp <- rep('Water MP',length(data$polymer))
  
  # Bar plot
  
  p3 <- ggplot(data, aes(x = prod, y = mean)) +
    geom_bar(aes(fill = polymer), stat = "identity", position = "stack", color = "black", width = 0.8) +  
    #scale_fill_manual(values = color_vector) + 
    scale_fill_brewer(palette="Set3")+
    theme_bw()+
    labs(x = "", y = "Mass (t)", fill = "Polymer") +
    ggtitle("Microplastic emissions to water") +
    geom_pointrange(data = data_sd_setlimit, aes(ymin = mean - sd, ymax = mean + sd), colour = "black", alpha = 0.9, size = 0.2) +
    ylim(0,NA) +
    coord_flip() +
    annotate ("text", x = -Inf, y = Inf, label = paste0("Σ = ",lab," t"), hjust = 1.05, vjust = -0.7)+
    theme(text = element_text(family = plot_family),
          axis.text = element_text(color = "black"))
  
  
  p3
  
  ####### Water MaP
  
  cp.order <- names(sort(apply(apply(EF.mat["Water",,,],c(2,3),sum),1,mean)))
  lab <- sci.not(apply(EF.mat["Water",,,],3,sum)*1000)
  labs <- cp.order <- cp.order[(length(cp.order)-9):length(cp.order)]
  labs[labs %in% PC] <- NiceNames[labs[labs %in% PC], "MediumLabel"]
  mean.cal <- apply(EF.mat["Water",,cp.order,],c(1,2),mean)*1000
  sd.cal <- apply(EF.mat["Water",,cp.order,],c(1,2),sd)*1000
  
  colnames(mean.cal) <- labs
  colnames(sd.cal) <- labs
  
  data <- data.frame(
    polymer = rep(rownames(mean.cal), ncol(mean.cal)),
    prod = rep(colnames(mean.cal), each = nrow(mean.cal)),
    mean = as.vector(mean.cal),
    sd = as.vector(sd.cal)
  )
  
  mean_sum <- aggregate(mean ~ prod, data = data, FUN = sum)
  sd_sum <- aggregate(sd ~ prod, data = data, FUN = sum)
  data_sd <- merge(mean_sum, sd_sum)
  
  # Reorder the dataframe
  
  data$prod <- with(data,reorder(prod, mean, FUN = sum))
  data$polymer <- factor(data$polymer, levels = c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET"))
  
  # Store results for output
  data.to.print.4 <- data
  data.to.print.4$comp <- rep('Water MaP',length(data$polymer))
  
  # Avoid disappearing error bars
  data_sd_setlimit <- data_sd %>%
    mutate(sd = if_else(data_sd$mean>data_sd$sd,sd,mean))
  
  # Bar plot
  
  p4 <- ggplot(data, aes(x = prod, y = mean)) +
    geom_bar(aes(fill = polymer), stat = "identity", position = "stack", color = "black", width = 0.8) +  
    #scale_fill_manual(values = color_vector) + 
    scale_fill_brewer(palette="Set3")+
    theme_bw()+
    labs(x = "", y = "Mass (t)", fill = "Polymer") +
    ggtitle("Macroplastic emissions to water") +
    geom_pointrange(data = data_sd_setlimit, aes(ymin = mean - sd, ymax = mean + sd), colour = "black", alpha = 0.9, size = 0.2) +
    coord_flip() +
    ylim(0,NA) +
    annotate ("text", x = -Inf, y = Inf, label = paste0("Σ = ",lab," t"), hjust = 1.05, vjust = -0.7)+
    theme(text = element_text(family = plot_family),
          axis.text = element_text(color = "black"))
  
  p4
  
  save <- p1 + p2 + p3 + p4 +
    plot_layout(guides = 'collect')
  
  #save
  
  ggsave(paste0("EmissionsByProd_",c,".png"),
         plot = save,
         path = "Results/Graphs/EmissionsByProd/",
         width = 9.5, height = 6,
         dpi = 500)
  
  data.to.print <- rbind(data.to.print.1,
                         data.to.print.2,
                         data.to.print.3,
                         data.to.print.4)
  data.to.print <- data.to.print[, c("prod", "polymer", "comp", "mean", "sd")]
  data.to.print <- data.to.print[order(data.to.print$prod), ]
  write.xlsx(data.to.print, paste0("Results/Tables/EmissionsByProd_",c,".xlsx"))
  
  message(paste0(format(Sys.time(), "%H:%M:%S"),
                 " Graph and results saved for Country: ", c))
}

rm(data,data_sd,data_sd_setlimit,data.to.print,data.to.print.1,data.to.print.2,
   data.to.print.3,data.to.print.4,EF,mean_sum,mean.cal,
   p1,p2,p3,p4,save,sd_sum,sd.cal,
   c,comp,comp.post,comp.pre,comp.stop,cp.order,dest,EF.mat,
   i,lab,labs,Mass.mat,mat,PC.stop,soil.comp,water.comp)
