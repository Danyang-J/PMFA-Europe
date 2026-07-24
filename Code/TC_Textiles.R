library("xlsx")

message(paste0(format(Sys.time(), "%H:%M:%S")," Calculating textile TC distributions..."))

##### Define important parameters for definition of TC from use of textiles #####

data <- read.xlsx(paste0("Input/",excel.file), sheetName = "Textiles")

# Organize by countries

#any_rows <- subset(Textiles, Country == "any")

#data <- rbind(subset(Flushing, Country == c), any_rows)
#data <- as.data.frame(data, stringsAsFactors = FALSE)

# Number of washing cycles performed in the lifetime of a product
N.washing.cl <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "N_washing_cl"), "Value"]), 
                      perc = as.numeric(data[which(data[,"ParameterName"] == "N_washing_cl"), "Spread"]),
                      N = SIM,
                      linf = 0)
N.washing.hh <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "N_washing_hh"), "Value"]), 
                      perc = as.numeric(data[which(data[,"ParameterName"] == "N_washing_hh"), "Spread"]),
                      N = SIM,
                      linf = 0)
# Fraction of textiles released during one cycle of washing
F.onecycle.washing  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "F_onecycle_washing"), "Value"]), 
                             perc = as.numeric(data[which(data[,"ParameterName"] == "F_onecycle_washing"), "Spread"]),
                             N = SIM,
                             linf = 0,
                             lsup = 1)

# Fraction of textiles released during one cycle of tumble-drying
F.onecycle.TD  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "F_onecycle_TD"), "Value"]), 
                        perc = as.numeric(data[which(data[,"ParameterName"] == "F_onecycle_TD"), "Spread"]),
                        N = SIM,
                        linf = 0)
# Fraction of textiles released during one cycle of cloth-line drying
F.onecycle.CLD.vs.washing  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "F_onecycle_CLD_vs_washing"), "Value"]), 
                                    perc = as.numeric(data[which(data[,"ParameterName"] == "F_onecycle_CLD_vs_washing"), "Spread"]),
                                    N = SIM,
                                    linf = 0,
                                    lsup = 1)

# Fraction of textiles released by wear during the lifetime of the product
F.wear.cl  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "F_wear_cl"), "Value"]), 
                    perc = as.numeric(data[which(data[,"ParameterName"] == "F_wear_cl"), "Spread"]),
                    N = SIM,
                    linf = 0,
                    lsup = 1)
# Fraction of textiles released by wear during the lifetime of the product
F.wear.other  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "F_wear_other"), "Value"]), 
                       perc = as.numeric(data[which(data[,"ParameterName"] == "F_wear_other"), "Spread"]),
                       N = SIM,
                       linf = 0,
                       lsup = 1)
# Fraction of textiles released by wear during the lifetime of the product
f.surface.worn  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "f_surface_worn"), "Value"]), 
                         perc = as.numeric(data[which(data[,"ParameterName"] == "f_surface_worn"), "Spread"]),
                         N = SIM,
                         linf = 0,
                         lsup = 1)
# Factor describing frequency of tumble-drying out of all washes
f.TD  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "f_TD"), "Value"]), 
               perc = as.numeric(data[which(data[,"ParameterName"] == "f_TD"), "Spread"]),
               N = SIM,
               linf = 0,
               lsup = 1)
# Factor describing frequency of indoor cloth-line drying out of all washes
f.ICLD  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "f_ICLD"), "Value"]), 
                 perc = as.numeric(data[which(data[,"ParameterName"] == "f_ICLD"), "Spread"]),
                 N = SIM,
                 linf = 0,
                 lsup = 1)
# Factor describing frequency of outdoor cloth-line drying out of all washes
f.OCLD  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "f_OCLD"), "Value"]), 
                 perc = as.numeric(data[which(data[,"ParameterName"] == "f_OCLD"), "Spread"]),
                 N = SIM,
                 linf = 0,
                 lsup = 1)
# time spent outdoors
t.outdoor  <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "t_outdoor"), "Value"]), 
                    perc = as.numeric(data[which(data[,"ParameterName"] == "t_outdoor"), "Spread"]),
                    N = SIM,
                    linf = 0,
                    lsup = 1)
# fraction of dust on TD filter that escapes to ww
f.to.WW <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "f_TD_filter_ww"), "Value"]), 
                 perc = as.numeric(data[which(data[,"ParameterName"] == "f_TD_filter_ww"), "Spread"]),
                 N = SIM,
                 linf = 0,
                 lsup = 1)
# fraction of dust on TD filter that escapes to indoor air
f.to.indoor.air <- rwrap(values = as.numeric(data[which(data[,"ParameterName"] == "f_TD_filter_indoorair"), "Value"]), 
                         perc = as.numeric(data[which(data[,"ParameterName"] == "f_TD_filter_indoorair"), "Spread"]),
                         N = SIM,
                         linf = 0,
                         lsup = 1)

##### EMISSIONS ###################################################################################

# clothing
Em.Clothing.WasteWater <- ( F.onecycle.washing * N.washing.cl + 
                              F.onecycle.TD * f.TD * N.washing.cl * f.to.WW )

Em.Clothing.IndoorAir <- ( F.onecycle.washing * F.onecycle.CLD.vs.washing * f.ICLD * N.washing.cl + 
                             F.wear.cl * (1 - t.outdoor) + 
                             F.onecycle.TD * f.TD * N.washing.cl * f.to.indoor.air )

Em.Clothing.OutdoorAir <- ( F.onecycle.washing * F.onecycle.CLD.vs.washing * f.OCLD * N.washing.cl + 
                              F.wear.cl * t.outdoor )

# household textiles
Em.HHText.WasteWater <- ( F.onecycle.washing * N.washing.hh + 
                            F.onecycle.TD * f.TD * N.washing.hh * f.to.WW )

Em.HHText.IndoorAir <- ( F.onecycle.washing * F.onecycle.CLD.vs.washing * f.ICLD * N.washing.hh + 
                           F.wear.other * f.surface.worn + 
                           F.onecycle.TD * f.TD * N.washing.hh * f.to.indoor.air )

Em.HHText.OutdoorAir <- ( F.onecycle.washing * F.onecycle.CLD.vs.washing * f.OCLD * N.washing.hh )



# ##### PLOTS #######################################################################################
# 
# ### LITERATURE DATA
# 
# pdf(file = "Results/TC/TC_Textiles.pdf",
#     height = 4,
#     width  = 5,
#     pointsize = 10)
# par(mfrow = c(1,1), mar = c(4,6,4,2), mgp = c(2.5,1,0), xpd = F)
# 
# barplot(as.numeric(data[which(data[,"ParameterName"] == "F_wear_cl"), "Value"])*100,
#         names.arg = c("Byrne", "Byrne", "Yoon", "ECHA"),
#         las = 2,
#         horiz = T,
#         axes = F,
#         col = "limegreen",
#         xlim = c(0,1),
#         xlab = "Fractional release (%)")
# title("Fractional release of textiles through wearing")
# axis(1)
# box()
# 
# vioplot(runif(10), runif(10), runif(10), runif(10),
#         names = c("", "", "", ""), col = NA, border = NA, pchMed = NA, drawRect = F, ylim = c(0,0.2))
# abline(h = seq(0,1,0.05), col = "gray85")
# abline(h = seq(0.025,1,0.025), col = "gray85", lty = 2)
# 
# vioplot(c(F.onecycle.washing * N.washing.cl + 
#             F.onecycle.TD * f.TD * N.washing.cl * f.to.WW +
#             F.onecycle.washing * F.onecycle.CLD.vs.washing * f.ICLD * N.washing.cl + 
#             F.wear.cl * (1 - t.outdoor) + 
#             F.onecycle.TD * f.TD * N.washing.cl * f.to.indoor.air +
#             F.onecycle.washing * F.onecycle.CLD.vs.washing * f.OCLD * N.washing.cl + 
#             F.wear.cl * t.outdoor),
#         c(F.onecycle.washing * N.washing.cl + 
#             F.onecycle.TD * f.TD * N.washing.cl * f.to.WW +
#             F.onecycle.washing * F.onecycle.CLD.vs.washing * f.ICLD * N.washing.cl + 
#             F.wear.cl * (1 - t.outdoor) + 
#             F.onecycle.TD * f.TD * N.washing.cl * f.to.indoor.air +
#             F.onecycle.washing * F.onecycle.CLD.vs.washing * f.OCLD * N.washing.cl + 
#             F.wear.cl * t.outdoor),
#         c(F.onecycle.washing * N.washing.hh + 
#             F.onecycle.TD * f.TD * N.washing.hh * f.to.WW +
#             F.onecycle.washing * F.onecycle.CLD.vs.washing * f.ICLD * N.washing.hh + 
#             F.wear.other + 
#             F.onecycle.TD * f.TD * N.washing.hh * f.to.indoor.air +
#             F.onecycle.washing * F.onecycle.CLD.vs.washing * f.OCLD * N.washing.hh),
#         c(F.onecycle.washing * N.washing.hh + 
#             F.onecycle.TD * f.TD * N.washing.hh * f.to.WW +
#             F.onecycle.washing * F.onecycle.CLD.vs.washing * f.ICLD * N.washing.hh + 
#             F.wear.other + 
#             F.onecycle.TD * f.TD * N.washing.hh * f.to.indoor.air +
#             F.onecycle.washing * F.onecycle.CLD.vs.washing * f.OCLD * N.washing.hh),
#         names = c("", "", "", ""),
#         col = c("aquamarine3", "aquamarine1", "mistyrose3", "mistyrose1"),
#         add = T)
# mtext("Clothing", 1, 1, at = 1)
# mtext("Technical\nClothing", 1, 2, at = 2)
# mtext("Household\nTextiles", 1, 2, at = 3)
# mtext("Technical\nHousehold\nTextiles", 1, 3, at = 4)
# 
# mtext("Transfer Coefficient", 2, 2.5)
# 
# title("Total fraction lost by washing, wearing, drying")
# 
# dev.off()
# 
# 
# ### breakdown of TC distributions
# 
# {
#   pdf(file = paste0("Results/TC/TC_Textiles_Origin_",".pdf"),
#       height = 6,
#       width  = 10,
#       pointsize = 10)
#   par(mfrow = c(2,3), mar = c(3.5,5,3,1), mgp = c(2.5,1,0), xpd = F)
#   
#   # emissions from clothing
#   
#   data <- cbind(Em.Clothing.WasteWater*100,
#                 c(F.onecycle.washing * N.washing.cl)*100,
#                 c(F.onecycle.TD * f.TD * N.washing.cl * f.to.WW)*100)
#   vioplot(data[,1], data[,2], data[,3],
#           names = c("Total", "Washing", "Tumble-drying"),
#           col = c("grey70", "turquoise", "violet", "springgreen"))
#   title("Emissions from clothing to wastewater")
#   mtext("Emission (%)",2,3.25,cex = 0.8)
#   text(par("usr")[1]-0.01*diff(par("usr")[1:2]),-0.17*diff(par("usr")[3:4]),"Mean", xpd = T, font = 3)
#   for(i in 1:ncol(data)){
#     text(i,-0.17*diff(par("usr")[3:4]),paste(signif(mean(data[,i]),1),"%"), xpd = T, font = 3)
#   }
#   grid(nx=NA, ny=NULL)
#   
#   data <- cbind(Em.Clothing.IndoorAir*100,
#                 c(F.onecycle.washing * F.onecycle.CLD.vs.washing * f.ICLD * N.washing.cl)*100,
#                 c(F.wear.cl * (1 - t.outdoor))*100,
#                 c(F.onecycle.TD * f.TD * N.washing.cl * f.to.indoor.air)*100)
#   vioplot(data[,1], data[,2], data[,3], data[,4],
#           names = c("Total", "Cloth-line drying", "Wear", "Tumble−drying"),
#           col = c("grey70", "turquoise", "violet", "springgreen"))
#   title("Emissions from clothing to indoor air")
#   mtext("Emission (%)",2,3.25,cex = 0.8)
#   text(par("usr")[1]-0.01*diff(par("usr")[1:2]),-0.17*diff(par("usr")[3:4]),"Mean", xpd = T, font = 3)
#   for(i in 1:ncol(data)){
#     text(i,-0.17*diff(par("usr")[3:4]),paste(signif(mean(data[,i]),1),"%"), xpd = T, font = 3)
#   }
#   grid(nx=NA, ny=NULL)
#   
#   data <- cbind(Em.Clothing.OutdoorAir*100,
#                 c(F.onecycle.washing * F.onecycle.CLD.vs.washing * f.OCLD * N.washing.cl)*100,
#                 c(F.wear.cl * t.outdoor)*100)
#   vioplot(data[,1], data[,2], data[,3],
#           names = c("Total", "Cloth-line drying", "Wear"),
#           col = c("grey70", "turquoise", "violet", "springgreen"))
#   title("Emissions from clothing to outdoor air")
#   mtext("Emission (%)",2,3.25,cex = 0.8)
#   text(par("usr")[1]-0.01*diff(par("usr")[1:2]),-0.17*diff(par("usr")[3:4]),"Mean", xpd = T, font = 3)
#   for(i in 1:ncol(data)){
#     text(i,-0.17*diff(par("usr")[3:4]),paste(signif(mean(data[,i]),1),"%"), xpd = T, font = 3)
#   }
#   grid(nx=NA, ny=NULL)
#   
#   
#   # emissions from household textiles
#   
#   data <- cbind(Em.HHText.WasteWater*100,
#                 c(F.onecycle.washing * N.washing.hh)*100,
#                 c(F.onecycle.TD * f.TD * N.washing.hh * f.to.WW)*100)
#   vioplot(data[,1], data[,2], data[,3],
#           names = c("Total", "Washing", "Tumble−drying"),
#           col = c("grey70", "turquoise", "violet", "springgreen"))
#   title("Emissions from household text. to wastewater")
#   mtext("Emission (%)",2,3.25,cex = 0.8)
#   text(par("usr")[1]-0.01*diff(par("usr")[1:2]),-0.17*diff(par("usr")[3:4]),"Mean", xpd = T, font = 3)
#   for(i in 1:ncol(data)){
#     text(i,-0.17*diff(par("usr")[3:4]),paste(signif(mean(data[,i]),1),"%"), xpd = T, font = 3)
#   }
#   grid(nx=NA, ny=NULL)
#   
#   data <- cbind(Em.HHText.IndoorAir*100,
#                 c(F.onecycle.washing * F.onecycle.CLD.vs.washing * f.ICLD * N.washing.hh)*100,
#                 c(F.wear.other * f.surface.worn)*100,
#                 c(F.onecycle.TD * f.TD * N.washing.hh * f.to.indoor.air)*100)
#   vioplot(data[,1], data[,2], data[,3], data[,4],
#           names = c("Total", "Cloth-line drying", "Wear", "Tumble-drying"),
#           col = c("grey70", "turquoise", "violet", "springgreen"))
#   title("Emissions from household text. to indoor air")
#   mtext("Emission (%)",2,3.25,cex = 0.8)
#   text(par("usr")[1]-0.01*diff(par("usr")[1:2]),-0.17*diff(par("usr")[3:4]),"Mean", xpd = T, font = 3)
#   for(i in 1:ncol(data)){
#     text(i,-0.17*diff(par("usr")[3:4]),paste(signif(mean(data[,i]),1),"%"), xpd = T, font = 3)
#   }
#   grid(nx=NA, ny=NULL)
#   
#   data <- cbind(Em.HHText.OutdoorAir*100,
#                 c(F.onecycle.washing * F.onecycle.CLD.vs.washing * f.OCLD * N.washing.hh)*100)
#   vioplot(data[,1], data[,2],
#           names = c("Total", "Cloth-line drying"),
#           col = c("grey70", "turquoise", "violet", "springgreen"))
#   title("Emissions from household text. to outdoor air")
#   mtext("Emission (%)",2,3.25,cex = 0.8)
#   text(par("usr")[1]-0.01*diff(par("usr")[1:2]),-0.17*diff(par("usr")[3:4]),"Mean", xpd = T, font = 3)
#   for(i in 1:ncol(data)){
#     text(i,-0.17*diff(par("usr")[3:4]),paste(signif(mean(data[,i]),1),"%"), xpd = T, font = 3)
#   }
#   grid(nx=NA, ny=NULL)
#   
#   dev.off()
# }
