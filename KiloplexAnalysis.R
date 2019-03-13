#Nathan Thai
#Install all necessary packages -----------------------------------------
pkg <-
  c("stringr","rvest", "reshape2","ComplexHeatmap","pvclust","circlize","RColorBrewer","heatmap3","clusterSim","janitor","tibble","colorRamps","ggrepel","readxl","dplyr","ggplot2",  "magrittr", "limma","manhattanly","plotrix","ggpubr","plotly","knitr","gdata","lattice","latticeExtra","ROCR")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}
library(stringr)
library(rvest)
library(pvclust)
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(readxl)
library(heatmap3)
library(reshape2)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(limma)
library(manhattanly)
library(plotly)
library(knitr)
library(janitor)
library(ggpubr)
library(clusterSim)
library(gdata)
library(grid)
library(plotrix)
library(lattice)
library(latticeExtra)
library(gtools)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(plyr)
library(ROCR)
library(tibble)
library(calibrate)
library(colorRamps)
library(devtools)
rm(pkg,new.pkg)
library(easyGgplot2)
# Loading up data---------------------------------------------------------
data_file <- file.choose()
setwd(dirname(data_file))
data_sheet1 <- read_excel(data_file, sheet = 1, .name_repair = "universal")
data_sheet2 <- read_excel(data_file, sheet = 2, .name_repair = "universal")
data_sheet3 <- read_excel(data_file, sheet = 3, .name_repair = "universal")
data_sheet4 <- read_excel(data_file, sheet = 4, .name_repair = "universal")
data_sheet5 <- read_excel(data_file, sheet = 5, .name_repair = "universal")
data_sheet6 <- read_excel(data_file, sheet = 6, .name_repair = "universal")
data_sheet17 <- read_excel(data_file, sheet = 7, .name_repair = "universal")
data_sheet8 <- read_excel(data_file, sheet = 8, .name_repair = "universal")
data_sheet9 <- read_excel(data_file, sheet = 9, .name_repair = "universal")
data_sheet10 <- read_excel(data_file, sheet = 10, .name_repair = "universal")
data_sheet11 <- read_excel(data_file, sheet = 11, .name_repair = "universal")
data_sheet12 <- read_excel(data_file, sheet = 12, .name_repair = "universal")
data_sheet13 <- read_excel(data_file, sheet = 13, .name_repair = "universal")
data_sheet14 <- read_excel(data_file, sheet = 14, .name_repair = "universal")
data_sheet15 <- read_excel(data_file, sheet = 15, .name_repair = "universal")
data_sheet16 <- read_excel(data_file, sheet = 16, .name_repair = "universal")
data_sheet17 <- read_excel(data_file, sheet = 17, .name_repair = "universal")
data_sheet18 <- read_excel(data_file, sheet = 18, .name_repair = "universal")
data_sheet19 <- read_excel(data_file, sheet = 19, .name_repair = "universal")
# 2.1 Correlation Plots ---------------------------------------------------
a <- data_sheet1[1:25,]
b <- "AI.value"
filename = "AI Week 0 or 12"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (4:(4+14))){
  plot[[i-3]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "spearman",
                           xlab = "Activity Index" , ylab = paste(colnames(a)[i],"(pg/ml)/(mg/dl)",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet2[1:25,]
b <- "CI.value"
filename = "CI Week 0 or 12"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (4:(4+14))){
  plot[[i-3]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "spearman",
                           xlab = "CI" , ylab = paste(colnames(a)[i],"(pg/ml)/(mg/dl)",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet3[1:28,]
b <- "rSLEDAI.value"
filename = "rSLEDAI Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (3:(3+14))){
  plot[[i-2]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                           add = "reg.line", conf.int = TRUE, point = T,
                           cor.coef = TRUE, cor.method = "spearman",
                           xlab = "rSLEDAI" , ylab = paste(colnames(a)[i],"(pg/ml)/(mg/dl)",sep=" "))
  
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()


a <- data_sheet13[1:24,]
b <- "PGA.value"
filename = "PGA Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (3:(3+14))){
  plot[[i-2]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "spearman",
                           xlab = "PGA" , ylab = paste(colnames(a)[i],"(pg/ml)/(mg/dl)",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()


a <- data_sheet5[1:28,]
b <- "Serum.Cr"
filename = "Serum Cr Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (3:(3+14))){
  plot[[i-2]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "spearman",
                           xlab = "Serum Cr" , ylab = paste(colnames(a)[i],"(pg/ml)/(mg/dl)",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()


a <- data_sheet14[2:102,]
b <- "PGA.value"
filename = "PGA All Times"
png(paste(filename,".png",sep=""),
    units="in",
    width = 15,
    height = 8,
    pointsize= 12,
    res=100)
plot <- list()
for (i in (18:(18+14))){
  plot[[i-17]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "spearman",
                            xlab = "PGA" , ylab = paste(colnames(a)[i],"(pg/ml)/(mg/dl)",sep=" "))
}

do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet13[2:102,]
b <- "rSLEDAI.value"
filename = "rSLEDAI All Times"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (18:(18+14))){
  plot[[i-17]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "spearman",
                            xlab = "rSLEDAI" , ylab = paste(colnames(a)[i],"(pg/ml)/(mg/dl)",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()


a <- data_sheet15[2:102,]
b <- "Serum.Cr"
filename = "Serum Cr All Times"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (11:(11+14))){
  plot[[i-10]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "spearman",
                            xlab = "Serum Cr" , ylab = paste(colnames(a)[i],"(pg/ml)/(mg/dl)",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

# MW Correlation Plots ----------------------------------------------------
b <- as.numeric(unlist(data_sheet2[1:554,2]))
c <- as.numeric(unlist(data_sheet2[1:554,3]))
a <- cbind(b,c)
filename = "MW of protein vs. fold-change of SLE and HC "
MW_max = max(a$MW..Da.)
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 20, 
    height = 10, 
    pointsize= 12,
    res=100)
ggscatter(a,x = "MW..Da.", y = "FC.SLE.CTRL",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 8,
          xlab = "MW of proteins [Da]" , ylab = "FC of SLE and HC")+ ggtitle(filename) +geom_point()+ 
  theme(plot.title = element_text(size = 28, face = "bold"),axis.title = element_text(size = 20)) + scale_x_continuous(breaks = seq(0,MW_max,300000))
dev.off()
# MW Correlation Plots #6 ----------------------------------------------------
a <- data_sheet6[1:995,][-417,]
filename = "MW of protein vs. fold-change in SLE>0 vs. SLE=0 "
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 20, 
    height = 10, 
    pointsize= 12,
    res=100)
ggscatter(a,x = "MW..Dalton.", y = "FC..rSLEDAI.0...rSLEDAI.0..",
          add = ("reg.line"), conf.int = TRUE, 
          cor.coef = TRUE, 
          cor.method = "spearman",cor.coef.size = 8,
          xlab = "MW of proteins [Da]" , ylab = "FC in SLE>0 vs. SLE=0")+ggtitle(filename) + geom_point() + 
  theme(plot.title = element_text(size = 28, face = "bold"),axis.title = element_text(size = 20)) + scale_x_continuous(breaks = seq(0,max(data_sheet4$MW..Dalton.),30000))
dev.off()
# 2.1 Dichotomize Start programming -------------------------------------------------------
AI_greater_equal_6 <- data_sheet3[4:105,-c(1:10,960:1010)]
AI_less_6 <- data_sheet3[106:112,-c(1:10,960:1010)]
# AI_HC <- data_sheet1[26:32,-(1:3)][,-(991:1000)]
AI_pvalues_66 <- list()
# AI_pvalues_g6h <- list()
# AI_pvalues_l6h <- list()
AI_FC <- list()
for (i in (1:length(AI_greater_equal_6))) {
  AI_pvalues_66[i] <- wilcox.test((AI_less_6[,i]),(AI_greater_equal_6[,i]),exact = TRUE, correct = TRUE)$p.value
  # AI_pvalues_g6h[i] <- wilcox.test(t(AI_HC[i]),t(AI_greater_equal_6[i]))$p.value
  # AI_pvalues_l6h[i] <- wilcox.test(t(AI_less_6[i]),t(AI_HC[i]))$p.value
  AI_FC[i] <- abs(log2(mean(t(AI_greater_equal_6[i]),na.rm=TRUE)/mean(t(AI_less_6[i]),na.rm=TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(AI_FC)))), file = "AI_FC.csv",row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(AI_pvalues_66)))), file = "AI_pvalues_66.csv", row.names = FALSE)

# 2.1 Dichotomize Start programming -------------------------------------------------------
AI_greater_equal_6 <- data_sheet1[data_sheet1$AI.value >= 6,][1:25,-(1:3)]
AI_less_6 <- data_sheet1[data_sheet1$AI.value < 6,][1:25,-(1:3)]
# AI_HC <- data_sheet1[26:32,-(1:3)][,-(991:1000)]
AI_pvalues_66 <- list()
# AI_pvalues_g6h <- list()
# AI_pvalues_l6h <- list()
AI_FC <- list()
for (i in (1:length(AI_greater_equal_6))) {
  AI_pvalues_66[i] <- wilcox.test((AI_less_6[,i]),(AI_greater_equal_6[,i]),exact = TRUE, correct = TRUE)$p.value
  # AI_pvalues_g6h[i] <- wilcox.test(t(AI_HC[i]),t(AI_greater_equal_6[i]))$p.value
  # AI_pvalues_l6h[i] <- wilcox.test(t(AI_less_6[i]),t(AI_HC[i]))$p.value
  AI_FC[i] <- abs(log2(mean(t(AI_greater_equal_6[i]),na.rm=TRUE)/mean(t(AI_less_6[i]),na.rm=TRUE)))
}

# new_sheet1_AI <- data_sheet1[-(33:35),c(-(1:3),-(991:1000))]
# new_sheet1_AI[nrow(new_sheet1_AI)+1,] <- t(AI_pvalues_66)
# new_sheet1_AI[nrow(new_sheet1_AI)+1,] <- t(AI_pvalues_g6h)
# new_sheet1_AI[nrow(new_sheet1_AI)+1,] <- t(AI_pvalues_l6h)
# new_sheet1_AI[nrow(new_sheet1_AI)+1,] <- t(AI_FC)
# new_sheet1_AI_sorted_66 <- new_sheet1_AI[,order(new_sheet1_AI[33,])]
# new_sheet1_AI_sorted_66 <- new_sheet1_AI_sorted_66[,order(new_sheet1_AI_sorted_66[36,], decreasing = TRUE)]
# new_sheet1_AI_sorted_g6h <- new_sheet1_AI[,order(new_sheet1_AI[34,])]
# new_sheet1_AI_sorted_g6h <- new_sheet1_AI_sorted_g6h[,order(new_sheet1_AI_sorted_g6h[36,], decreasing = TRUE)]
# new_sheet1_AI_sorted_l6h <- new_sheet1_AI[,order(new_sheet1_AI[35,])]
# new_sheet1_AI_sorted_g6h <- new_sheet1_AI_sorted_l6h[,order(new_sheet1_AI_sorted_l6h[36,], decreasing = TRUE)]
write.csv(unname(as.numeric(paste(unlist(AI_FC)))), file = "AI_FC.csv",row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(AI_pvalues_66)))), file = "AI_pvalues_66.csv", row.names = FALSE)

# Dichotomize for CI ------------------------------------------------------

CI_pvalues_33 <- list()
# CI_pvalues_g3h <- list()
# CI_pvalues_l3h <- list()
CI_greater_equal_3 <- data_sheet2[data_sheet2$CI.value >= 3,][1:25,-(1:4)]
CI_less_3 <- data_sheet2[data_sheet2$CI.value < 3,][1:25,-(1:4)]
CI_HC <- data_sheet2[26:32,-(1:4)]
CI_FC <- list()
for (i in (1:length(CI_greater_equal_3))) {
  # CI_pvalues_33[i] <- wilcox.test(t(CI_less_3[i]),t(CI_greater_equal_3[i]), exact = TRUE, correct = TRUE)$p.value
  CI_pvalues_33[i] <- wilcox.test(t(CI_HC[i]),t(CI_greater_equal_3[i]), exact = TRUE, correct = TRUE)$p.value
  # CI_pvalues_g3h[i] <- wilcox.test(t(CI_HC[i]),t(CI_greater_equal_3[i]))$p.value
  # CI_pvalues_l3h[i] <- wilcox.test(t(CI_HC[i]),t(CI_less_3[i]))$p.value
  CI_FC[i] <- abs(log2(abs(mean(t(CI_greater_equal_3[i]),na.rm=TRUE)/mean(t(CI_less_3[i]),na.rm=TRUE))))
}
# new_sheet2_CI <- data_sheet2[-(33:35),c(-(1:3),-(991:1000))]
# new_sheet2_CI[nrow(new_sheet2_CI)+1,] <- t(CI_pvalues_33)
# new_sheet2_CI[nrow(new_sheet2_CI)+1,] <- t(CI_pvalues_g3h)
# new_sheet2_CI[nrow(new_sheet2_CI)+1,] <- t(CI_pvalues_l3h)
# new_sheet2_CI[nrow(new_sheet2_CI)+1,] <- t(CI_FC)
# new_sheet2_CI <- new_sheet2_CI[,order(df[33,])]

# write.csv(unname(as.numeric(paste(unlist(AI_FC)))), file = "AI_FC.txt", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(CI_FC)))), file = "CI_FC.csv", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(AI_pvalues)))), file = "AI_pvalue.txt", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(CI_pvalues_33)))), file = "CI_pvalue.csv", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(AI_pvalues)))), file = "AI_pvalue.txt", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(CI_pvalues)))), file = "CI_pvalue.txt", row.names = FALSE)

# 2.1 Dichotomize Start programming -------------------------------------------------------
LNclass5_yes <- data_sheet7[data_sheet7$LN.V..Yes.or.No.=="Yes" ,][1:25,-(1:5)]
LNclass5_no <- data_sheet7[data_sheet7$LN.V..Yes.or.No.=="No",][1:25,-(1:5)]
LN5_pvalues <- list()
LN5_FC <- list()
for (i in (1:length(LNclass5_yes))) {
  LN5_pvalues[i] <- wilcox.test(t(LNclass5_yes[i]),t(LNclass5_no[i]))$p.value
  LN5_FC[i] <- abs(log2(mean(t(LNclass5_no[i]),na.rm=TRUE)/mean(t(LNclass5_yes[i]),na.rm=TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(LN5_pvalues)))), file = "LN5_pvalues.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(LN5_FC)))), file = "LN5_FC.csv", row.names = FALSE)

# Mixed 5 -------------------------------------------------------------------
LNclassM5_yes <- data_sheet8[data_sheet8$LN.V..Yes.or.No.=="Mixed" | data_sheet8$LN.V..Yes.or.No.=="mixed",][1:17,-(1:3)]
LNclassM5_no <- data_sheet8[data_sheet8$LN.V..Yes.or.No.=="Yes",][1:17,-(1:3)]
LN5_pvalues <- list()
LN5_FC <- list()
for (i in (1:length(LNclassM5_yes))) {
  LN5_pvalues[i] <- wilcox.test((LNclassM5_yes[,i]),(LNclassM5_no[,i]),exact = TRUE, correct = TRUE)$p.value
  LN5_FC[i] <- abs(log2(mean(t(LNclassM5_yes[i]),na.rm=TRUE)/mean(t(LNclassM5_no[i]),na.rm=TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(LN5_pvalues)))), file = "LN5_pvalues.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(LN5_FC)))), file = "LN5_FC.csv", row.names = FALSE)

# 4 vs 3 ------------------------------------------------------------------
LNclass3_yes <- data_sheet6[data_sheet6$LN.Class.AMP.Bx=="[IV]",][1:25,-(1:4)]
LNclass4 <- data_sheet6[data_sheet6$LN.Class.AMP.Bx=="[III]",][1:25,-(1:4)]
LN3_pvalues <- list()
LN3_FC <- list()
for (i in (1:length(LNclass3_yes))) {
  LN3_pvalues[i] <- wilcox.test(t(LNclass3_yes[i]),t(LNclass4[i]),exact = TRUE, correct = TRUE)$p.value
  LN3_FC[i] <- abs(log2(mean(t(LNclass4[i]),na.rm=TRUE)/mean(t(LNclass3_yes[i]),na.rm=TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(LN3_pvalues)))), file = "LN3_pvalues.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(LN3_FC)))), file = "LN3_FC.csv", row.names = FALSE)

# Part 3 code: ------------------------------------------------------------
Renal_greater_equal_6 <- data_sheet3[data_sheet3$rSLEDAI.value >= 6,][1:28,-(1:2)]
Renal_less_6 <- data_sheet3[data_sheet3$rSLEDAI.value < 6,][1:28,-(1:2)]
Renal_pvalues <- list()
Renal_FC <- list()
for (i in (1:length(Renal_greater_equal_6))) {
  Renal_pvalues[i] <- wilcox.test((Renal_greater_equal_6[,i]),(Renal_less_6[,i]),exact = TRUE, correct = TRUE)$p.value
  Renal_FC[i] <- abs(log2(mean(t(Renal_greater_equal_6[i]),na.rm=TRUE)/mean(t(Renal_less_6[i]),na.rm=TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(Renal_pvalues)))), file = "Renal_pvalue.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(Renal_FC)))), file = "Renal_FC.csv", row.names = FALSE)

# PGA W0 ------------------------------------------------------------------

PGA_greater_equal_2 <- data_sheet4[data_sheet4$PGA.value >= 2,][1:24,-(1:2)]
PGA_less_2 <- data_sheet4[data_sheet4$PGA.value < 2,][1:24,-(1:2)]
PGA_pvalues <- list()
PGA_FC <- list()
for (i in (1:length(PGA_greater_equal_2))) {
  PGA_pvalues[i] <- wilcox.test(t(PGA_greater_equal_2[i]),t(PGA_less_2[i]), exact = TRUE, correct = TRUE)$p.value
  PGA_FC[i] <- abs(log2(mean(t(PGA_greater_equal_2[i]),na.rm=TRUE)/mean(t(PGA_less_2[i]),na.rm=TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(PGA_pvalues)))), file = "PGA_pvalue.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(PGA_FC)))), file = "PGA_FC.csv", row.names = FALSE)

# Part 3: PGA All: ------------------------------------------------------------
PGA_greater_equal_2 <- data_sheet14[data_sheet14$PGA.value >= 2,][1:102,-(1:17)]
PGA_less_2 <- data_sheet14[data_sheet14$PGA.value < 2,][1:102,-(1:17)]
PGA_pvalues <- list()
PGA_FC <- list()
for (i in (1:length(PGA_greater_equal_2))) {
  PGA_pvalues[i] <- wilcox.test(t(PGA_greater_equal_2[i]),t(PGA_less_2[i]), exact = TRUE, correct = TRUE)$p.value
  PGA_FC[i] <- abs(log2(mean(t(PGA_greater_equal_2[i]),na.rm=TRUE)/mean(t(PGA_less_2[i]),na.rm=TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(PGA_pvalues)))), file = "PGA_pvalue.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(PGA_FC)))), file = "PGA_FC.csv", row.names = FALSE)

# Dichromtoized All time rSLEDAI: ------------------------------------------------------------
Renal_greater_equal_6_all <- data_sheet13[data_sheet13$rSLEDAI.value >= 6,][1:102,-(1:18)]
Renal_less_6_all <- data_sheet13[data_sheet13$rSLEDAI.value < 6,][1:102,-(1:18)]
Renal_hc_all <- data_sheet13[data_sheet13$S == "HC",][1:102,-(1:18)]
Renal_pvalues_all <- list()
Renal_hc_p_value <- list()
Renal_FC_all <- list()
for (i in (1:length(Renal_greater_equal_6_all))) {
  # Renal_pvalues_all[i] <- wilcox.test(t(Renal_greater_equal_6_all[i]),t(Renal_less_6_all[i]))$p.value
  Renal_hc_p_value[i] <- wilcox.test(t(Renal_greater_equal_6_all[i]),t(Renal_hc_all[i]))$p.value
  Renal_FC_all[i] <- abs(log2(mean(t(Renal_greater_equal_6_all[i]),na.rm = TRUE)/mean(t(Renal_less_6_all[i]),na.rm = TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(Renal_hc_p_value)))), file = "Renal_hc_pvalue.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(Renal_pvalues_all)))), file = "Renal_pvalue.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(Renal_FC_all)))), file = "Renal_FC.csv", row.names = FALSE)

# ALl time PGA ------------------------------------------------------------
PGA_greater_equal_2_all <- data_sheet14[data_sheet14$PGA.value >= 2,][1:102,-(1:18)]
PGA_less_2_all <- data_sheet14[data_sheet14$PGA.value < 2,][1:102,-(1:18)]
PGA_hc_all <- data_sheet14[data_sheet14$S == "HC",][1:102,-(1:18)]
PGA_pvalues_all <- list()
PGA_FC_all <- list()
for (i in (1:length(PGA_greater_equal_2_all))) {
  # PGA_pvalues_all[i] <- wilcox.test(t(PGA_greater_equal_2_all[i]),t(PGA_less_2_all[i]))$p.value
  PGA_pvalues_all[i] <- wilcox.test(t(PGA_greater_equal_2_all[i]),t(PGA_hc_all[i]))$p.value
  PGA_FC_all[i] <- abs(log2(mean(t(PGA_greater_equal_2_all[i]),na.rm = TRUE)/mean(t(PGA_less_2_all[i]),na.rm = TRUE)))
}
write.csv(unname(as.numeric(paste(unlist(PGA_pvalues_all)))), file = "PGA_pvalue.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(PGA_FC_all)))), file = "PGA_FC.csv", row.names = FALSE)

# PVAL and FC Mem vs Pro ----------------------------------------------------------------
Proliferative <- data_sheet11[data_sheet11$LN.class== "[III]" | data_sheet11$LN.class== "[IV]" | data_sheet11$LN.class== "[III][V]" | data_sheet11$LN.class== "[IV][V]",][,-(1:6)]
# LN4 <- data_sheet11[data_sheet11$LN.class== "[IV]",][,-(1:7)]
Membranous <- data_sheet11[data_sheet11$LN.class== "[V]",][,-(1:6)]
# LN_3_5 <- data_sheet11[data_sheet11$LN.class== "[III][V]",][,-(1:7)]
# LN_4_5 <- data_sheet11[data_sheet11$LN.class== "[IV][V]",][,-(1:7)]
HC <- data_sheet11[data_sheet11$LN.class== "HC",][,-(1:6)]
LN_pvalues_pro_all <- list()
LN_FC_pro_all <- list()
LN_pvalues_mem_all <- list()
LN_FC_mem_all <- list()
for (i in (1:length(Proliferative))) {
  LN_pvalues_pro_all[i] <- wilcox.test(t(Proliferative[i]),t(Membranous[i]))$p.value
  LN_FC_pro_all[i] <- (mean(t(Proliferative[i]),na.rm = TRUE)/mean(t(Membranous[i]),na.rm = TRUE))
  LN_pvalues_mem_all[i] <- wilcox.test(t(Membranous[i]),t(Proliferative[i]))$p.value
  LN_FC_mem_all[i] <- (mean(t(Membranous[i]),na.rm = TRUE)/mean(t(Proliferative[i]),na.rm = TRUE))
  #LN_FC_mem_all[i] <- (mean(t(Membranous[i]))/mean(t(Proliferative[i])))
}
write.csv(unname(as.numeric(paste(unlist(LN_pvalues_pro_all)))), file = "LN_pvalues_pro_all.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(LN_FC_pro_all)))), file = "LN_FC_pro_all.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(LN_pvalues_mem_all)))), file = "LN_pvalues_mem_all.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(LN_FC_mem_all)))), file = "LN_FC_mem_all.csv", row.names = FALSE)
# FC & p-Values for sheet 9  ------------------------------------------------------------
PU_NR <- data_sheet9[data_sheet9$PU.Responder..R.or.NR. =="NR",][1:34,-(1:2)]
PU_R <- data_sheet9[data_sheet9$PU.Responder..R.or.NR. =="HC",][1:34,-(1:2)]
PU_HC <- data_sheet9[(28:34),-(1:2)]
PU_pvalues_1 <- list()
# PU_pvalues_2 <- list()
# PU_pvalues_3 <- list()
PU_FC_1 <- list()
# PU_FC_2 <- list()
# PU_FC_3 <- list()
for (i in (1:length(PU_R))) {
  PU_pvalues_1[i] <- wilcox.test(t(PU_R[i]),t(PU_NR[i]), exact = TRUE, correct = TRUE)$p.value
  PU_FC_1[i] <- abs(log2(mean(t(PU_NR[i]),na.rm = TRUE)/mean(t(PU_R[i]),na.rm = TRUE)))
  # PU_pvalues_2[i] <- wilcox.test(t(PU_R[i]),t(PU_HC[i]))$p.value
  # PU_FC_2[i] <- (mean(t(PU_NR[i]),na.rm = TRUE)/mean(t(PU_HC[i]),na.rm = TRUE))
  # PU_pvalues_3[i] <- wilcox.test(t(PU_NR[i]),t(PU_HC[i]))$p.value
  # PU_FC_3[i] <- (mean(t(PU_NR[i]),na.rm = TRUE)/mean(t(PU_HC[i]),na.rm = TRUE))
}

write.csv(unname(as.numeric(paste(unlist(PU_pvalues_1)))), file = "PU_pvalues_1.csv", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(PU_pvalues_2)))), file = "PU_pvalues_2.txt", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(PU_pvalues_3)))), file = "PU_pvalues_3.txt", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(PU_FC_1)))), file = "PU_FC_1.csv", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(PU_FC_2)))), file = "PU_FC_2.txt", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(PU_FC_3)))), file = "PU_FC_3.txt", row.names = FALSE)

# FC & p-Values for sheet 11  ------------------------------------------------------------

SLEDAI_NR <- data_sheet10[data_sheet10$rSLEDAI.responder..R.or.NR =="HC",][(1:34),-(1:2)]
SLEDAI_R <- data_sheet10[data_sheet10$rSLEDAI.responder..R.or.NR =="R",][(1:34),-(1:2)]
# SLEDAI_HC <- data_sheet10[(28:34),-(1:2)]
SLEDAI_pvalues_1 <- list()
# SLEDAI_pvalues_2 <- list()
# SLEDAI_pvalues_3 <- list()
SLEDAI_FC_1 <- list()
# SLEDAI_FC_2 <- list()
# SLEDAI_FC_3 <- list()
for (i in (1:length(SLEDAI_NR))) {
  SLEDAI_pvalues_1[i] <- wilcox.test(t(SLEDAI_R[i]),t(SLEDAI_NR[i]))$p.value
  SLEDAI_FC_1[i] <- abs(log2(mean(t(SLEDAI_NR[i]),na.rm = TRUE)/mean(t(SLEDAI_R[i]),na.rm = TRUE)))
  # SLEDAI_pvalues_2[i] <- wilcox.test(t(SLEDAI_R[i]),t(SLEDAI_HC[i]))$p.value
  # SLEDAI_FC_2[i] <- (mean(t(SLEDAI_R[i]),na.rm = TRUE)/mean(t(SLEDAI_HC[i]),na.rm = TRUE))
  # SLEDAI_pvalues_3[i] <- wilcox.test(t(SLEDAI_R[i]),t(SLEDAI_NR[i]))$p.value
  # SLEDAI_FC_3[i] <- (mean(t(SLEDAI_NR[i]),na.rm = TRUE)/mean(t(SLEDAI_HC[i]),na.rm = TRUE))
}
# SLEDAI_FC_1[SLEDAI_FC_1 == Inf] <- 0 
# SLEDAI_FC_2[SLEDAI_FC_2 == Inf] <- 0 
# SLEDAI_FC_3[SLEDAI_FC_3 == Inf] <- 0 
write.csv(unname(as.numeric(paste(unlist(SLEDAI_pvalues_1)))), file = "SLEDAI_pvalues_1.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(SLEDAI_FC_1)))), file = "SLEDAI_FC_1.csv", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(SLEDAI_pvalues_2)))), file = "SLEDAI_pvalues_2.txt", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(SLEDAI_FC_2)))), file = "SLEDAI_FC_2.txt", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(SLEDAI_pvalues_3)))), file = "SLEDAI_pvalues_3.txt", row.names = FALSE)
# write.csv(unname(as.numeric(paste(unlist(SLEDAI_FC_3)))), file = "SLEDAI_FC_3.txt", row.names = FALSE)

# Correlation Test --------------------------------------------------------
p_value <- list()
corr_ <- list()
for (i in (11:length(data_sheet15))) {
  p_value[i-10] <- cor.test(x = data_sheet15[1:102,8], y=data_sheet15[1:102,i], method = 'spearman',na.action = "na.exclude")$p.value
  corr_[i-10] <- cor.test(x = data_sheet15[1:102,8], y=data_sheet15[1:102,i], method = 'spearman',na.action = "na.exclude")$estimate
}
write.csv(unname(as.numeric(paste(unlist(p_value)))), file = "P_pvalue.txt", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(corr_)))), file = "corr.txt", row.names = FALSE)
# Serial Plots PGA (still not working but trying to graph all)-----------------------------------------------------------------
rawByWeeks <- data_sheet2
biomarker <- "EGF.R"
con <- "PGA.value"
label <- "PGA"
filename = "Top 10 proteins increased in patients with worse AI"
png(paste0(label,".png"), 
    units="in", 
    width = 20, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
u <- 0
par(mar = c(5, 4, 4, 4) + 0.3)
for (i in c(94,100,101,102,104,108,109,111,113,114,115,1214,1217,1220,1221)){
  W0_protein <- rawByWeeks[rawByWeeks$Weeks == "W0" & rawByWeeks$ID == i,][[biomarker]]
  W12_protein <- rawByWeeks[rawByWeeks$Weeks == "W12" & rawByWeeks$ID == i,][[biomarker]]
  W24_protein <- rawByWeeks[rawByWeeks$Weeks == "W24" & rawByWeeks$ID == i,][[biomarker]]
  W52_protein <- rawByWeeks[rawByWeeks$Weeks == "W52" & rawByWeeks$ID == i,][[biomarker]]
  
  W0_sCr <- rawByWeeks[rawByWeeks$Weeks == "W0" & rawByWeeks$ID == i,][[con]]
  W12_sCr <- rawByWeeks[rawByWeeks$Weeks == "W12" & rawByWeeks$ID == i,][[con]]
  W24_sCr <- rawByWeeks[rawByWeeks$Weeks == "W24" & rawByWeeks$ID == i,][[con]]
  W52_sCr <- rawByWeeks[rawByWeeks$Weeks == "W52" & rawByWeeks$ID == i,][[con]]
  
  x_axis <- c("W0","W12","W24","W52")
  y_axis1 <- c(W0_protein,W12_protein,W24_protein,W52_protein)
  y_axis2 <- c(W0_sCr,W12_sCr,W24_sCr,W52_sCr)
  y_axis <- append(y_axis1,y_axis2)
  
  u <- u+1

  plot[u]<- plot(y_axis1, type = "b", axes = FALSE, bty = "n", xlab = "", ylab = "", cex = 3, lwd = 3)
  mtext(paste0(biomarker," (pg/ml)/(mg/dl)"), side=2, line=3, col = "black",font = 4,cex=2)
  axis(side = 1, at = 1:4, labels = x_axis, cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
  axis(side = 2, at = pretty(range(y_axis1)), cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
  par(new = TRUE)
  plot(y_axis2, type = "b", axes = FALSE, col = "blue", bty = "n", xlab = "", ylab = "", lwd = 3)
  axis(side=4, at = pretty(y_axis2), cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3, col= "blue", col.axis ="blue")
  mtext(label, side=4, line=2.5, col = "blue", font = 4, cex = 2)
  mtext(paste("Patient",i,":", label , "vs.",paste0(biomarker)), font = 4, cex=3)
}
do.call(ggarrange,c(plot,nrow=4,ncol=4,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

# Serial Plots (individuals graphs) -----------------------------------------------------------------

rawByWeeks <- data_sheet1
biomarker <- "LAP.TGFb1."
con <- "rSLEDAI.value"
label <- "rSLEDAI"
plot <- list()
#
for (i in c(94,100,101,102,104,108,109,111,113,114,115,1214,1217,1220,1221)){
  png(paste0("Patient(",i,") ",con," vs. ",biomarker,".png"),
      units="in",
      width = 15,
      height = 8,
      pointsize= 12,
      res=100)
  
  W0_protein <- rawByWeeks[rawByWeeks$Weeks == "W0" & rawByWeeks$ID == i,][[biomarker]]
  W12_protein <- rawByWeeks[rawByWeeks$Weeks == "W12" & rawByWeeks$ID == i,][[biomarker]]
  W24_protein <- rawByWeeks[rawByWeeks$Weeks == "W24" & rawByWeeks$ID == i,][[biomarker]]
  W52_protein <- rawByWeeks[rawByWeeks$Weeks == "W52" & rawByWeeks$ID == i,][[biomarker]]
  
  W0_sCr <- rawByWeeks[rawByWeeks$Weeks == "W0" & rawByWeeks$ID == i,][[con]]
  W12_sCr <- rawByWeeks[rawByWeeks$Weeks == "W12" & rawByWeeks$ID == i,][[con]]
  W24_sCr <- rawByWeeks[rawByWeeks$Weeks == "W24" & rawByWeeks$ID == i,][[con]]
  W52_sCr <- rawByWeeks[rawByWeeks$Weeks == "W52" & rawByWeeks$ID == i,][[con]]
  
  x_axis <- c("W0","W12","W24","W52")
  y_axis1 <- c(W0_protein,W12_protein,W24_protein,W52_protein)
  y_axis2 <- c(W0_sCr,W12_sCr,W24_sCr,W52_sCr)
  y_axis <- append(y_axis1,y_axis2)
  
  set.seed(101)
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  plot[i] <- plot(y_axis1, type = "b", axes = FALSE, bty = "n", xlab = "", ylab = "", cex = 3, lwd = 3)
  mtext(paste0(biomarker," log2(pg/ml)/(mg/dl)"), side=2, line=3, col = "black",font = 4,cex=2)# first plot
  axis(side = 1, at = 1:4, labels = x_axis, cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
  axis(side = 2, at = pretty(range(y_axis1)), cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
  par(new = TRUE)
  plot[i] <- plot(y_axis2, type = "b", axes = FALSE, col = "blue", bty = "n", xlab = "", ylab = "", lwd = 3)
  axis(side=4, at = pretty(y_axis2), cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3, col= "blue", col.axis ="blue")
  mtext(label, side=4, line=2.5, col = "blue", font = 4, cex = 2)
  mtext(paste("Patient",i,":", label , "vs.",paste0(biomarker)), font = 4, cex=3)
  dev.off()
}
# average Serial Plots --------------------------------------------------------------
rawByWeeks <- data_sheet1
con <- "rSLEDAI.value"
label <- "rSLEDAI"
for (biomarker in c("ALCAM","CD6")){
  png(paste0(con," vs. ",biomarker,".png"),
      units="in",
      width = 15,
      height = 8,
      pointsize= 12,
      res=100)
  plot <- list()
  for (i in 1:length(con)){
    W0_protein <- mean(rawByWeeks[rawByWeeks$Weeks == "W0",][[biomarker]],na.rm = T)
    W12_protein <- mean(rawByWeeks[rawByWeeks$Weeks == "W12",][[biomarker]],na.rm = T)
    W24_protein <- mean(rawByWeeks[rawByWeeks$Weeks == "W24",][[biomarker]],na.rm = T)
    W52_protein <- mean(rawByWeeks[rawByWeeks$Weeks == "W52",][[biomarker]],na.rm = T)
    
    W0_sCr <- mean(rawByWeeks[rawByWeeks$Weeks == "W0",][[con]],na.rm = T)
    W12_sCr <- mean(rawByWeeks[rawByWeeks$Weeks == "W12",][[con]],na.rm = T)
    W24_sCr <- mean(rawByWeeks[rawByWeeks$Weeks == "W24",][[con]],na.rm = T)
    W52_sCr <- mean(rawByWeeks[rawByWeeks$Weeks == "W52",][[con]],na.rm = T)
    
    x_axis <- c("W0","W12","W24","W52")
    y_axis1 <- c(W0_protein,W12_protein,W24_protein,W52_protein)
    y_axis2 <- c(W0_sCr,W12_sCr,W24_sCr,W52_sCr)
    y_axis <- append(y_axis1,y_axis2)
    
    set.seed(101)
    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
    plot[i] <- plot(y_axis1, type = "b", axes = FALSE, bty = "n", xlab = "", ylab = "", cex = 3, lwd = 3)
    mtext(paste0(biomarker," log2(pg/ml)/(mg/dl)"), side=2, line=3, col = "black",font = 4,cex=2)# first plot
    axis(side = 1, at = 1:4, labels = x_axis, cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
    axis(side = 2, at = pretty(range(y_axis1)), cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
    par(new = TRUE)
    plot(y_axis2, type = "b", axes = FALSE, col = "blue", bty = "n", xlab = "", ylab = "", lwd = 3)
    axis(side=4, at = pretty(range(y_axis2)), cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
    mtext(label, side=4, line=2.5, col = "blue", font = 4, cex = 2)
    mtext(paste("Avg",label, "vs.",paste0(biomarker)), font = 4, cex=3)
  }
  dev.off()
}
# do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
# Average Serial Plots for Special Analysis ALCAM CD6 --------------------------------------------------------------
rawByWeeks <- data_sheet2
con <- "PGA.value"
label <- "PGA"
for (biomarker in c("A")){
  png(paste0(con," vs. ",biomarker,".png"),
      units="in",
      width = 15,
      height = 8,
      pointsize= 12,
      res=100)
  plot <- list()
  for (i in 1:length(con)){
    #protein
    W0_protein <- mean(rawByWeeks[rawByWeeks$Weeks == "W0",][[biomarker]],na.rm = T)
    W12_protein <- mean(rawByWeeks[rawByWeeks$Weeks == "W12",][[biomarker]],na.rm = T)
    initial_p <- (W0_protein+W12_protein)/2
    W24_protein <- mean(rawByWeeks[rawByWeeks$Weeks == "W24",][[biomarker]],na.rm = T)
    W52_protein <- mean(rawByWeeks[rawByWeeks$Weeks == "W52",][[biomarker]],na.rm = T)
    final_p <- (W24_protein+W52_protein)/2
    FC_p <- final_p/initial_p
    #indicator
    W0_sCr <- mean(rawByWeeks[rawByWeeks$Weeks == "W0",][[con]],na.rm = T)
    W12_sCr <- mean(rawByWeeks[rawByWeeks$Weeks == "W12",][[con]],na.rm = T)
    initial_i <- (W0_sCr+W12_sCr)/2
    W24_sCr <- mean(rawByWeeks[rawByWeeks$Weeks == "W24",][[con]],na.rm = T)
    W52_sCr <- mean(rawByWeeks[rawByWeeks$Weeks == "W52",][[con]],na.rm = T)
    final_i <- (W24_sCr+W52_sCr)/2
    FC_i <- final_i/initial_i
    
    x_axis <- c("Initial","Final")
    y_axis1 <- c(initial_p,final_p)
    y_axis2 <- c(initial_i,final_i)
    y_axis <- append(y_axis1,y_axis2)
    max_ax <- max(y_axis1,y_axis2,initial_i,final_i)+1
    
    set.seed(101)
    par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
    plot[i] <- plot(y_axis1, type = "b", axes = FALSE, bty = "n", xlab = "", ylab = "", ylim=c(0,max_ax), cex = 3, lwd = 3)
    mtext(paste0(biomarker," (pg/ml)/(mg/dl)"), side=2, line=3, col = "black",font = 4,cex=2)# first plot
    axis(side = 1, at = 1:2, labels = x_axis, cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
    axis(side = 2, cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
    par(new = TRUE)
    plot[i] <- plot(y_axis2, type = "b", axes = FALSE, col = "blue", ylim=c(0,max_ax), bty = "n", xlab = "", ylab = "", lwd = 3)
    axis(side=4,cex.axis = 2, font =4, lwd = 3, lwd.ticks = 3)
    mtext(label, side=4, line=2.5, col = "blue", font = 4, cex = 2)
    mtext(paste("Avg",label, "vs.",paste0(biomarker)), font = 4, cex=3)
    legend(95,95,legend=c(paste0("%","FC ",FC_p),paste0("%","FC ",FC_i)),col=c("black", "blue"), lty=1:2, cex=0.8)
  }
  dev.off()
}
# do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
# volcanos ----------------------------------------------------------------
vcsheet1 <- data_sheet1
vcsheet1 <- data_sheet2
vcsheet3 <- data_sheet3
# vc 1----------------------------------------------------------------------
cut_p = 2
with(vcsheet1, plot(log2FoldChange, -log10(pvalue), pch=20, font = 2, lwd =2, main="Proliferative LN", xlim=c(-8,8)))

# Add colored points: red if padj<0.05, orange of log2FC>1, green if both)
with(subset(vcsheet1, pvalue<.05 ), points(log2FoldChange, -log10(pvalue), pch=20, col="red"))
with(subset(vcsheet1, abs(log2FoldChange)>cut_p), points(log2FoldChange, -log10(pvalue), pch=20, col="orange"))
with(subset(vcsheet1, pvalue<.05 & abs(log2FoldChange)>cut_p), points(log2FoldChange, -log10(pvalue), pch=20, col="green"))
with(subset(vcsheet1, pvalue<.05 & abs(log2FoldChange)>cut_p), textxy(log2FoldChange, -log10(pvalue), labs=Gene, cex=1.2))

# Vc 2 --------------------------------------------------------------------

with(vcsheet2, plot(log2FoldChange, -log10(pvalue), pch=20,font = 2,lwd =2, main="PU: R VS NR W0", xlim=c(-8,8)))

# Add colored points: red if padj<0.05, orange of log2FC>1, green if both)
with(subset(vcsheet2, pvalue<.05 ), points(log2FoldChange, -log10(pvalue), pch=20, col="red"))
with(subset(vcsheet2, abs(log2FoldChange)>cut_p), points(log2FoldChange, -log10(pvalue), pch=20, col="orange"))
with(subset(vcsheet2, pvalue<.05 & abs(log2FoldChange)>cut_p), points(log2FoldChange, -log10(pvalue), pch=20, col="green"))
with(subset(vcsheet2, pvalue<.05 & abs(log2FoldChange)>cut_p), textxy(log2FoldChange, -log10(pvalue), labs=Gene, cex=.6))

# Vc 3 --------------------------------------------------------------------

with(vcsheet3, plot(log2FoldChange, -log10(pvalue), pch=20,font = 2,lwd =2, main="rSLEDAI: R VS NR W0", xlim=c(-10,10)))

# Add colored points: red if padj<0.05, orange of log2FC>1, green if both)
with(subset(vcsheet3, pvalue<.05 ), points(log2FoldChange, -log10(pvalue), pch=20, col="red"))
with(subset(vcsheet3, abs(log2FoldChange)>cut_p), points(log2FoldChange, -log10(pvalue), pch=20, col="orange"))
with(subset(vcsheet3, pvalue<.05 & abs(log2FoldChange)>cut_p), points(log2FoldChange, -log10(pvalue), pch=20, col="green"))
with(subset(vcsheet3, pvalue<.05 & abs(log2FoldChange)>cut_p), textxy(log2FoldChange, -log10(pvalue), labs=Gene, cex=.8))

# heatmaps loading dock ----------------------------------------------------------------
row.names(data_sheet) <- hm1[,1]
hm1 <- hm1[-1,-1]
# hm1[hm1 == NA] <- 0
hm1 <- t(hm1)
hm1_matrix <- data.matrix(hm1)
heatmap(hm1_matrix, Rowv = NA, col = green2red(256),scale="column",cex.lab = 2, font.lab=2, cex.main = 2, font=2, na.rm=T, margins=c(5,5))
# mtext("Patients", side =4 , pos=-2 , cex=1.5)
mtext("sCr2 All of Time Points", font =2, side =3 , line= -1 , cex=1.5)

# New Heatmaps ------------------------------------------------------------
# aa <- (data_sheet18[18:1016,2:109])
# b <- data.Normalization(as.matrix(data_sheet18), type="n3a",normalization="row")

data_sheet17.2 <- data_sheet17[-1,-1]
data_sheet_name <- data_sheet17[-1,]
rownames(data_sheet17.2) <- data_sheet_name$Patient.ID
# data_sheet17.2 <- data_sheet17.2[-c(1,2,3,4,5,6),]
# data_sheet17 <- data_sheet17[1:5,]
# rownames(data_sheet17.2) <-data_sheet17$Patient.ID[-1,1]
col_name=vector(mode = "character",102)
col_name[1:7] = "HC"
col_name[8:102] = "SLE"
colnames(data_sheet17.2) = col_name
data_heat <- data.matrix(data_sheet17.2)
distance = dist((data_heat), method = "manhattan")
# distance = dist(1 - cor(t(data_heat), use = "pa"))
cluster = as.dendrogram(hclust(distance), method = "ward.D2")
# distance = dist(data_heat)
# cluster = as.dendrogram(hclust(distance, method = "average"))
my_palette <- colorRampPalette(c("green", "black", "red"))(n = 10000)

png("heatmaps.png",    # create PNG for the heat map        
    width = 12*300,        # 5 x 300 pixels
    height = 10*300,
    res = 300,            # 300 pixels per inch
    pointsize = 8) 
heatmap3(
  data_heat,
  cellnote = data_heat,
  # same data set for cell labels
  # heat map title
  notecol = "black",
  keysize = 1.5,
  x.center = 0,
  cex.key.xlab = 5,
  # change font color of cell labels to black
  density.info = "none",
  # turns off density plot inside color legend
  trace = "none",
  ColIndividualColors = (12:25),
  srtCol = 45,
  key= TRUE,
  cexRow = 1 ,
  # turns off trace lines inside the heat map
  margins = c(5,0),
  # widens margins around plot
  col = greenred(5),
  # use on color palette defined earlier
  dendrogram = "row",
  # breaks = col_breaks,
  # only draw a row dendrogram
  Rowv = cluster,
  Colv = NA,
  symbreaks = T,
  scale = "row"
)
dev.off()
# Dot plot for AI ---------------------------------------------------------------
data_sheet1f <- data_sheet1[1:32,]
par(mar = c(5, 4, 4, 4) + 0.3)
filename = "Top 10 proteins increased in patients with worse AI"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 19, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (5:(5+9))){
  my_comparisons <-
    list(c("<6", ">=6"), c("<6", "HC"), c(">=6", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet1f,
    xName = 'S',
    yName = toString(colnames(data_sheet1f)[i]),
    size = 4,
    ytitle = paste(toString(colnames(data_sheet1f)[i]), "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'S',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(labels = c(
    "<6" = paste("AI<6", paste0("N=", length(data_sheet1f$S[data_sheet1f$S == "<6"])), sep = "\n"),
    ">=6" = paste("AI>=6", paste0("N=", length(data_sheet1f$S[data_sheet1f$S == ">=6"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet1f$S[data_sheet1f$S == "HC"])), sep = "\n")
  )
  )
  plot[[i-4]] <- a
}
# do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =.1, heights = .1))
# annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =.1, heights = .1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()
# dot plot for CI ---------------------------------------------------------
data_sheet2f <- data_sheet2[1:32,]
par(mar = c(5, 4, 4, 4) + 0.3)
filename = "Top 15 proteins increased in patients with worse CI"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 19, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (5:(5+14))){
  my_comparisons <-
    list(c("<3", ">=3"), c("<3", "HC"), c(">=3", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet2f,
    xName = 'S',
    yName = colnames(data_sheet2f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet2f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'S',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  ) + scale_x_discrete(labels = c(
    "<3" = paste("CI<3", paste0("N=", length(data_sheet2f$S[data_sheet2f$S == "<3"])), sep = "\n"),
    ">=3" = paste("CI>=3", paste0("N=", length(data_sheet2f$S[data_sheet2f$S == ">=3"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet2f$S[data_sheet2f$S == "HC"])), sep = "\n")
  ))
  plot[[i-4]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()
# Dot plot for SLEDAI W0 --------------------------------------------------
data_sheet3f <- data_sheet3[1:35,]
par(mar = c(5, 4, 4, 4) + 0.3)
filename = "Top 15 proteins increased in patients with worse rSLEDAI W0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (4:(4+14))){
  my_comparisons <-
    list(c("<6", ">=6"), c("<6", "HC"), c(">=6", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet3f,
    xName = 'S',
    yName = colnames(data_sheet3f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet3f)[i], "(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'S',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(labels = c(
    "<6" = paste("rSLEDAI<6", paste0("N=", length(data_sheet3f$S[data_sheet3f$S == "<6"])), sep = "\n"),
    ">=6" = paste("rSLEDAI>=6", paste0("N=", length(data_sheet3f$S[data_sheet3f$S == ">=6"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet3f$S[data_sheet3f$S == "HC"])), sep = "\n")
  )
  )
  plot[[i-3]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", size =15, rot = 360
  )
)
dev.off()
# Dot plot for PGA W0 -----------------------------------------------------
data_sheet4f <- data_sheet4[1:31,]
par(mar = c(5, 4, 4, 4) + 0.3)
filename = "Top 15 proteins increased in patients with worse PGA Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (4:(4+14))){
  my_comparisons <-
    list(c("<2", ">=2"), c("<2", "HC"), c(">=2", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet4f,
    xName = 'S',
    yName = colnames(data_sheet4f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet4f)[i], "(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'S',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(labels = c(
    "<2" = paste("PGA<2", paste0("N=", length(data_sheet4f$S[data_sheet4f$S == "<2"])), sep = "\n"),
    ">=2" = paste("PGA>=2", paste0("N=", length(data_sheet4f$S[data_sheet4f$S == ">=2"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet4f$S[data_sheet4f$S == "HC"])), sep = "\n")
  )
  )
  plot[[i-3]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(
    paste(
      "****: p<=0.0001",
      "***: p<=0.001",
      "**: p<=0.01",
      "*: p<=0.05",
      "ns: p>0.05",
      sep = "\n"
    ),
    color = "blue",
    size = 15,
    rot = 360
  )
)
dev.off()
# Dot plot rSLEDAI all Time -----------------------------------------------
data_sheet13f <- data_sheet10[1:92,]
par(mar = c(5, 4, 4, 4) + 0.3)
filename = "rSLEDAI: ALCAM and CD6"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (4:(4+1))){
  my_comparisons <-
    list(c("<6", ">=6"), c("<6", "HC"), c(">=6", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet13f,
    xName = 'S',
    yName = colnames(data_sheet13f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet13f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'S',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(labels = c(
    "<6" = paste("rSLEDAI<6", paste0("N=", length(data_sheet13f$S[data_sheet13f$S == "<6"])), sep = "\n"),
    ">=6" = paste("rSLEDAI>=6", paste0("N=", length(data_sheet13f$S[data_sheet13f$S == ">=6"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet13f$S[data_sheet13f$S == "HC"])), sep = "\n")
  )
  )
  plot[[i-3]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()
# Dot plot PGA all time ---------------------------------------------------
data_sheet14f <- data_sheet4[1:83,]
par(mar = c(5, 4, 4, 4) + 0.3)
filename = "Top 15 proteins increased in patients with worse PGA All Times"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (13:(13+1))){
  my_comparisons <-
    list(c("<2", ">=2"), c("<2", "HC"), c(">=2", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet14f,
    xName = 'S',
    yName = colnames(data_sheet14f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet14f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'S',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(labels = c(
    "<2" = paste("PGA<2", paste0("N=", length(data_sheet14f$S[data_sheet14f$S == "<2"])), sep = "\n"),
    ">=2" = paste("PGA>=2", paste0("N=", length(data_sheet14f$S[data_sheet14f$S == ">=2"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet14f$S[data_sheet14f$S == "HC"])), sep = "\n")
  )
  )
  plot[[i-12]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()

# Dot plot data_sheet6 for LN 3 vs 4 --------------------------------------------------
data_sheet6f <- data_sheet6[1:32,][-(17:25),]
# par(mar = c(5, 4, 4, 4) + 0.3)
filename = "LN 3 vs 4 Dot Plots For Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (5:(5+14))){
  my_comparisons <-
    list(c("[III]", "[IV]"), c("[III]", "HC"), c("[IV]", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet6f,
    xName = 'LN.Class.AMP.Bx',
    yName = colnames(data_sheet6f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet6f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'LN.Class.AMP.Bx',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(labels = c(
    "[III]" = paste("LN 3", paste0("N=", length(data_sheet6f$LN.Class.AMP.Bx[data_sheet6f$LN.Class.AMP.Bx == "[III]"])), sep = "\n"),
    "[IV]" = paste("LN 4", paste0("N=", length(data_sheet6f$LN.Class.AMP.Bx[data_sheet6f$LN.Class.AMP.Bx == "[IV]"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet6f$LN.Class.AMP.Bx[data_sheet6f$LN.Class.AMP.Bx == "HC"])), sep = "\n")
  )
  )
  plot[[i-4]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()

# Pro vs Mem --------------------------------------------------------------
data_sheet6f <- data_sheet16[1:33,]
# par(mar = c(5, 4, 4, 4) + 0.3)
filename = "6 proteins elevated in Proliferative LN"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (7:(7+5))){
  my_comparisons <-
    list(c("Proliferative", "Membranous"))
  a <- ggplot2.dotplot(
    data = data_sheet6f,
    xName = 'LN.class',
    yName = colnames(data_sheet6f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet6f)[i], "(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'LN.class',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(labels = c(
    "Proliferative" = paste("Proliferative", paste0("N=", length(data_sheet6f$LN.class[data_sheet6f$LN.class == "Proliferative"])), sep = "\n"),
    "Membranous" = paste("Membranous", paste0("N=", length(data_sheet6f$LN.class[data_sheet6f$LN.class == "Membranous"])), sep = "\n")
  )
  )
  plot[[i-6]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()
# dot plots for data_sheet7 ---------------------------------------------------
data_sheet7f <- data_sheet7[1:32,]
filename = "LN 5 vs Not 5 Dot Plots For Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (6:(6+14))){
  my_comparisons <-
    list(c("No", "[V]"),c("No", "HC"),c("[V]", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet7f,
    xName = 'LN.V..Yes.or.No.',
    yName = colnames(data_sheet7f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet7f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'LN.V..Yes.or.No.',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(limits = c("No","[V]","HC"),labels = c(
    "No" = paste("LN Not 5", paste0("N=", length(data_sheet7f$LN.V..Yes.or.No.[data_sheet7f$LN.V..Yes.or.No. == "No"])), sep = "\n"),
    "[V]" = paste("LN 5", paste0("N=", length(data_sheet7f$LN.V..Yes.or.No.[data_sheet7f$LN.V..Yes.or.No. == "[V]"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet7f$LN.V..Yes.or.No.[data_sheet7f$LN.V..Yes.or.No. == "HC"])), sep = "\n")
  )
  )
  plot[[i-5]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()
# Dot plot for sheet9 -----------------------------------------------------
data_sheet9f <- data_sheet9[1:34,]
filename = "PU Response vs No Response Dot Plot For Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (3:(3+14))){
  my_comparisons <-
    list(c("NR", "R"), c("NR", "HC"), c("R", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet9f,
    xName = 'PU.Responder..R.or.NR.',
    yName = colnames(data_sheet9f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet9f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'PU.Responder..R.or.NR.',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(limits = (c("NR","R","HC")),labels = c(
    "NR" = paste("PU NR", paste0("N=", length(data_sheet9f$PU.Responder..R.or.NR.[data_sheet9f$PU.Responder..R.or.NR. == "NR"])), sep = "\n"),
    "R" = paste("PU R", paste0("N=", length(data_sheet9f$PU.Responder..R.or.NR.[data_sheet9f$PU.Responder..R.or.NR. == "R"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet9f$PU.Responder..R.or.NR.[data_sheet9f$PU.Responder..R.or.NR. == "HC"])), sep = "\n")
  )
  )
  plot[[i-2]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()

# Pro vs Mem --------------------------------------------------------------
data_sheet6f <- data_sheet17[1:33,]
# par(mar = c(5, 4, 4, 4) + 0.3)
filename = "Top 10 proteins elevated in Membranous LN"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (7:(7+9))){
  my_comparisons <-
    list(c("Proliferative", "Membranous"))
  a <- ggplot2.dotplot(
    data = data_sheet6f,
    xName = 'LN.class',
    yName = colnames(data_sheet6f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet6f)[i], "(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'LN.class',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(labels = c(
    "Proliferative" = paste("Proliferative", paste0("N=", length(data_sheet6f$LN.class[data_sheet6f$LN.class == "Proliferative"])), sep = "\n"),
    "Membranous" = paste("Membranous", paste0("N=", length(data_sheet6f$LN.class[data_sheet6f$LN.class == "Membranous"])), sep = "\n")
  )
  )
  plot[[i-6]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()

# Dot plot for sheet10 ----------------------------------------------------

data_sheet10f <- data_sheet10[1:34,]
filename = "rSLEDAI Response vs No Response Dot Plot For Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (3:(3+14))){
  my_comparisons <-
    list(c("NR", "R"), c("NR", "HC"), c("R", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet10f,
    xName = 'rSLEDAI.responder..R.or.NR',
    yName = colnames(data_sheet10f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet10f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'rSLEDAI.responder..R.or.NR',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(limits = c("NR","R","HC"),labels = c(
    "NR" = paste("rSLEDAI NR", paste0("N=", length(data_sheet10f$rSLEDAI.responder..R.or.NR[data_sheet10f$rSLEDAI.responder..R.or.NR == "NR"])), sep = "\n"),
    "R" = paste("rSLEDAI R", paste0("N=", length(data_sheet10f$rSLEDAI.responder..R.or.NR[data_sheet10f$rSLEDAI.responder..R.or.NR == "R"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet10f$rSLEDAI.responder..R.or.NR[data_sheet10f$rSLEDAI.responder..R.or.NR == "HC"])), sep = "\n")
  )
  )
  plot[[i-2]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()
# Dot plot for By Race PU -----------------------------------------------------
data_sheet9f <- data_sheet5
filename = "(White) PU Response vs No Response Dot Plot For Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (1002:(1005))){
  my_comparisons <-
    list(c("NR", "R"), c("NR", "HC"), c("R", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet9f,
    xName = 'PU.Responder..R.or.NR.',
    yName = colnames(data_sheet9f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet9f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'PU.Responder..R.or.NR.',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(limits = (c("NR","R","HC")),labels = c(
    "NR" = paste("PU NR", paste0("N=", length(data_sheet9f$PU.Responder..R.or.NR.[data_sheet9f$PU.Responder..R.or.NR. == "NR"])), sep = "\n"),
    "R" = paste("PU R", paste0("N=", length(data_sheet9f$PU.Responder..R.or.NR.[data_sheet9f$PU.Responder..R.or.NR. == "R"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet9f$PU.Responder..R.or.NR.[data_sheet9f$PU.Responder..R.or.NR. == "HC"])), sep = "\n")
  )
  )
  plot[[i-1001]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()

# Dot plot for sheet10 ----------------------------------------------------

data_sheet10f <- data_sheet4
filename = "(White) rSLEDAI Response vs No Response Dot Plot For Week 0"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 21, 
    height = 12, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (1001:1002)){
  my_comparisons <-
    list(c("NR", "R"), c("NR", "HC"), c("R", "HC"))
  a <- ggplot2.dotplot(
    data = data_sheet10f,
    xName = 'rSLEDAI.responder..R.or.NR',
    yName = colnames(data_sheet10f)[i],
    size = 4,
    ytitle = paste(colnames(data_sheet10f)[i], "log2(pg/ml)/(mg/dl)"),
    ytitleFont = c(12, "bold", "#993333"),
    ytickLabelRotation = 45,
    yTickLabelFont = c(10, "bold", "black"),
    axisLine = c(1, "solid", "black"),
    removePanelGrid = TRUE,
    removePanelBorder = TRUE,
    xShowTitle = FALSE,
    addBoxplot = TRUE,
    boxplotLineWeight = .2,
    notch = TRUE,
    groupName = 'rSLEDAI.responder..R.or.NR',
    groupColors = c("#00AFBB", "#E7B800", "#FC4E07"),
    showLegend = FALSE
  ) + stat_compare_means(
    comparisons = my_comparisons,
    method = "wilcox.test",
    tip.length = .01,
    paired = FALSE,
    label = "p.signif",
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
      symbols = c("****", "***", "**", "*", "ns")
    )
  )+ scale_x_discrete(limits = c("NR","R","HC"),labels = c(
    "NR" = paste("rSLEDAI NR", paste0("N=", length(data_sheet10f$rSLEDAI.responder..R.or.NR[data_sheet10f$rSLEDAI.responder..R.or.NR == "NR"])), sep = "\n"),
    "R" = paste("rSLEDAI R", paste0("N=", length(data_sheet10f$rSLEDAI.responder..R.or.NR[data_sheet10f$rSLEDAI.responder..R.or.NR == "R"])), sep = "\n"),
    "HC" = paste("HC", paste0("N=", length(data_sheet10f$rSLEDAI.responder..R.or.NR[data_sheet10f$rSLEDAI.responder..R.or.NR == "HC"])), sep = "\n")
  )
  )
  plot[[i-1000]] <- a
}
figure <- ggarrange(plotlist = plot,
                    nrow = 3,
                    ncol = 5, widths = .1, heights = .1)
annotate_figure(
  figure,
  top = text_grob(
    filename,
    color = "blue",
    face = "bold",
    size = 20
  ),
  right = text_grob(paste(
    "****: p<=0.0001",
    "***: p<=0.001",
    "**: p<=0.01",
    "*: p<=0.05",
    "ns: p>0.05",
    sep = "\n"), color = "blue", rot = 360
  )
)
dev.off()

# VK code Testing ---------------------------------------------------------
data <- data_sheet3[,-1] #remove the first sheet
rownames(data) <- data_sheet3$Protein #rename the row into proteins
HC <- data[,1:3] #spliting the data for Healthy
Patients <- data[4:6] #spliting the data for Patients
p_value <- list() #empty list
fc_value <- list() #empty list
for (i in 1:1001){
  p_value[i] <- wilcox.test(t(HC[i,]),t(Patients[i,]),exact = TRUE, correct = TRUE)$p.value
  fc_value[i] <- abs(log2(mean(t(HC[i,]),na.rm = TRUE)/mean(t(Patients[i,]),na.rm = TRUE))) # I noticed you did not take the log 2 when you calculated FC, FC should always be log2 if you are looking for upregulated and downregulated.
}
write.csv(unname(as.numeric(paste(unlist(p_value)))), file = "p_value.csv", row.names = FALSE)
write.csv(unname(as.numeric(paste(unlist(fc_value)))), file = "fc_value.csv", row.names = FALSE)
