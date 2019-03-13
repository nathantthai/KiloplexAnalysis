# 2.1 AI Correlation Plots ---------------------------------------------------
a <- data_sheet17
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
                            xlab = "PGA" , ylab = paste("[",colnames(a)[i],"]",sep=" "))
}

do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet4
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
                           xlab = "PGA" , ylab = paste("[",colnames(a)[i],"]",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet16
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
                            xlab = "rSLEDAI" , ylab = paste("[",colnames(a)[i],"]",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet3
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
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "spearman",
                           xlab = "rSLEDAI" , ylab = paste("[",colnames(a)[i],"]",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet1
b <- "AI.value"
filename = "AI Week 0 or 12"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (6:(6+8))){
  plot[[i-5]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "spearman",
                           xlab = "Activity Index" , ylab = paste("[",colnames(a)[i],"]",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet5
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
                           xlab = "Serum Cr" , ylab = paste("[",colnames(a)[i],"]",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet2
b <- "CI.value"
filename = "CI Week 0 or 12"
png(paste(filename,".png",sep=""), 
    units="in", 
    width = 15, 
    height = 8, 
    pointsize= 12,
    res=100)
plot <- list()
for (i in (6:(6+14))){
  plot[[i-5]] <- ggscatter(a, x = b, y = colnames(a)[i], 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "spearman",
                           xlab = "CI" , ylab = paste("[",colnames(a)[i],"]",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()

a <- data_sheet18
b <- "Serum.Cr"
filename = "Serum Cr All Times"
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
                            xlab = "Serum Cr" , ylab = paste("[",colnames(a)[i],"]",sep=" "))
}
do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1))
annotate_figure(do.call(ggarrange,c(plot,nrow=3,ncol=5,widths =1, heights = 1)),top = text_grob( filename, color = "blue", face = "bold", size = 20))
dev.off()