# Impacts of heat stress on soft corals, an overlooked and highly vulnerable component of coral reef ecosystems, at a central equatorial Pacific atoll

# Authors: Dominique G. Maucieri[1], and Julia K. Baum[1]
# Institution: [1] Department of Biology, University of Victoria, Victoria, British Columbia, V8P 5C2, Canada
# Corresponding Author: Julia K. Baum, Email: baum@uvic.ca

# Script to create the figures from the manuscript

##############################

## load packages 
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(scales)
library(glmmTMB)

## Set your working directory
# Make sure that this contains all the file folders
setwd("C:/Users/...") # If on a PC
setwd("/Users/...") # If on a Mac

load("data/raw_soft_coral.Rdata")

publication_site_names <- rev(c("VL13", "VL8", "VL2", "VL12", "VL1", "VL11", "VL5", "VL9", "VL10", "VL6", "L5", "M10", "M11", "L6", "L3", "L2", "L1", "M5", "H2", "VH3", "VH1", "VH4", "VH2", "H1", "M12", "L4", "M3", "M2", "M1", "M4", "M6", "M7", "M8", "M9", "L7", "VL7", "VL3", "VL4", "VL14", "VL15"))

region_colors <- c("#CC503E", "#E17C05", "#EDAD08", "#0F8554", "#1D6996", "#5F4690", "#000000")
region_shapes <- c(5,15,16,17,18,8,1)
Disturbance_Colors <- c("#2A0BD9", "#40A1FF", "#ABF8FF", "#FFAD73", "#A60021")
WindExposure_colors <- c("yellow", "white")

theme_DGM <- function () { 
  theme_classic(base_size=12, base_family="Times New Roman") + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.title=element_text(size=14, face="bold"), axis.text = element_text(size=8, face="plain"), legend.text=element_text(size=14, face="plain"), legend.title = element_text(size=16, face="bold"))}

##############################

## Figure 2c
cover_before <- raw_soft_coral %>% filter(TimeBlock == "before") %>% group_by(UniqueID) %>% 
  mutate(SoftCoral = sum(Sarcophyton, Cladiella, Sinularia, Lobophytum)) %>% ungroup() %>% 
  group_by(Site, HD_Cat, HD_Cont, Region, NPP, WE, WaveEnergy) %>% summarise(meanSoftCoral = mean(SoftCoral) / 100, meanLobo = mean(Lobophytum) / 100, meanSinu = mean(Sinularia) / 100, meanSarco = mean(Sarcophyton) / 100, meanClad = mean(Cladiella) / 100)

bubble_data <- cover_before %>% pivot_longer(names_to = "Soft_Coral", values_to = "prop_cover", cols = c(meanSoftCoral, meanSinu, meanSarco, meanClad, meanLobo)) %>% group_by(Site, Soft_Coral) %>% mutate(percent_cover = prop_cover*100, radius = sqrt(percent_cover/ pi ))

bubble_data$Region <- factor(bubble_data$Region, levels = c("Bay of Wrecks","North Shore","North Lagoon","Mid Lagoon","South Lagoon", "Vaskess Bay","Korean Wrecks"))

bubble_data$Site <- factor(bubble_data$Site, levels = rev(c("site17", "site18", "site19", "site39", "site15", "site16", "site20", "site11", "site21", "site10", "site23", "site1", "site22", "site2", "site24", "site38", "site3", "site25", "site26", "site30", "site27", "site31", "site32", "site40", "site33", "site9", "site34", "site35", "site8", "site14", "site6", "site7", "site13", "site12", "site4", "site36", "site5", "site37", "site28", "site29")))

bubble_data$Soft_Coral <- factor(bubble_data$Soft_Coral, levels = c("meanClad", "meanSarco", "meanSinu", "meanLobo", "meanSoftCoral"))

bubble_plot <- ggplot(bubble_data, aes(x = Site, y = Soft_Coral, fill = Region))+ geom_point(aes(size=percent_cover),shape=21)+ scale_size_identity(guide="legend") + theme_DGM() + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0)) + scale_fill_manual(values = region_colors) + scale_y_discrete(labels = c(expression(italic("Cladiella")), expression(italic("Sarcophyton")), expression(italic("Sinularia")), expression(italic("Lobophytum")), "All")) + scale_x_discrete(labels = publication_site_names) + labs(x = "Site Names", y = "Genera", size = "Mean percent cover") + guides(fill = guide_legend(override.aes = list(size = 5)))+ theme(axis.text = element_text(size=12, face="plain")) 

jpeg(filename = "tables_figures/Figure_2c.jpeg", width = 13, height = 6.5, units = "in", pointsize = 15, quality = 300, res = 300)
bubble_plot
dev.off()

## Figure 3
## panel a
cover_before$NPP_std <- rescale(cover_before$NPP)
cover_before$HD_Cont_std <- rescale(cover_before$HD_Cont)

model4 <- glmmTMB(meanSoftCoral ~ HD_Cont_std + WE + NPP_std , data = cover_before, ziformula = ~1, family = beta_family(link = "logit"))

coefs <- data.frame(confint(model4, full = TRUE))
colnames(coefs) <- c("LowerCI", "UpperCI", "Estimate")
main.coef <- coefs[-c(1,5,6), ]
rownames(main.coef) <- c("HD", "WE","NPP")
main.coef$Variable <- c("HD", "WE","NPP")
main.coef$Fill <- c("white","black","black")

main.coef.plot <- ggplot(main.coef, aes(x = factor(Variable, levels = c("NPP","WE","HD")), y = Estimate)) + geom_hline(yintercept = 0, color = gray(1/2), lty = 2) + geom_pointrange(aes(x = factor(Variable, levels = c("NPP","WE","HD")), y = Estimate, ymin = LowerCI, ymax = UpperCI), position = position_dodge(width = 1/2), shape = 21, fatten = 4, size = 1, fill = main.coef$Fill) + coord_flip() + theme_DGM() + xlab("")

## panel b
cover_before$HD_Cat <- factor(cover_before$HD_Cat, levels = c("Very Low", "Low", "Medium", "High", "Very High"))
cover_before$Region <- factor(cover_before$Region, levels = c("Bay of Wrecks", "North Shore","North Lagoon","Mid Lagoon","South Lagoon", "Vaskess Bay","Korean Wrecks"))

disturbance.plot <- ggplot(cover_before, aes(y=meanSoftCoral, x=sqrt(HD_Cont))) + geom_smooth(formula = y ~ x, color="black", method=glm, se=TRUE, method.args = list(family = "quasibinomial"), fill = "grey") + geom_point(size=3, aes(color = Region, shape = Region))  + labs(x="HD", y="Mean proportion of soft coral cover", title="", color = "Region") + theme_DGM() + scale_colour_manual(values = region_colors)+ theme(legend.position = "none", axis.title=element_text(size=12, face="bold")) + scale_shape_manual(values = region_shapes)

## panel c
WindExposure.Plot <- ggplot(cover_before, aes(y=meanSoftCoral, x=WE, fill = WE)) + geom_boxplot() + labs(x="WE", y="Mean proportion of soft coral cover", title="") + theme_DGM() + scale_fill_manual(values = WindExposure_colors)+ theme(legend.position = "none", axis.title=element_text(size=11, face="bold")) + scale_x_discrete(breaks = c("windward", "sheltered"), labels = c("Windward", "Sheltered"))

## panel d
NPP.plot <- ggplot(cover_before, aes(y=meanSoftCoral, x=NPP)) + geom_smooth(formula = y ~ x, color="black", method=glm, se=TRUE, method.args = list(family = "quasibinomial"), fill = "grey") + geom_point(size=3, aes(color = Region, shape = Region)) + labs(x=expression(bold(paste("NPP (mg C ", m^{-2}, " ", day^{-1}, ")", sep = ""))), y="Mean proportion of soft coral cover", title="", color = "Region", shape = "Region") + theme_DGM()+ scale_colour_manual(values = region_colors)  + theme(legend.position = "bottom") + theme(legend.text=element_text(size=12, face="plain"), legend.title = element_text(size=12, face="bold"), axis.title=element_text(size=11, face="bold"))+ scale_shape_manual(values = region_shapes)


Figure_3_top <- plot_grid(main.coef.plot, disturbance.plot, rel_widths = c(1, 2.5), labels = c("a", "b"), nrow = 1, label_fontfamily = "Times")
Figure_3_left <- plot_grid(WindExposure.Plot, NULL, rel_heights = c(1, 0.28), ncol = 1)
Figure_3_bottom <- plot_grid(Figure_3_left, NPP.plot, rel_widths = c(1, 2.5), labels = c("c", "d"), nrow = 1, label_fontfamily = "Times")

Figure_3 <- plot_grid(Figure_3_top, Figure_3_bottom, rel_heights = c(1, 1.25), ncol = 1, label_fontfamily = "Times")

jpeg(filename = "tables_figures/Figure_3.jpeg", width = 12, height = 7, units = "in", pointsize = 15, quality = 300, res = 300)
Figure_3
dev.off()

## Figure S2b
Wave_Energy <- cover_before %>% filter(WaveEnergy > 0)

Wave_Energy$Region <- factor(Wave_Energy$Region, levels = c("Bay of Wrecks","North Shore","North Lagoon","Mid Lagoon","South Lagoon", "Vaskess Bay","Korean Wrecks"))

region_colors_sub <- c("#CC503E", "#E17C05", "#EDAD08", "#1D6996", "#5F4690")
region_shapes_sub <- c(5,15,16,18,8)

wave_energy_plot <- ggplot(Wave_Energy, aes(x= WaveEnergy, y=meanSoftCoral)) + scale_shape_manual(values = region_shapes_sub)+ geom_smooth(formula = y ~ x, color="black", fill = "grey", method=glm, method.args = list(family = "quasibinomial")) + geom_point(size=3, aes(color = Region, shape = Region)) + labs(x=expression(bold(paste("Wave Energy (kW ", m^{-1}, ")", sep = ""))), y="Mean proportion of soft coral cover", title="", color = "Region", shape = "Region") + theme_DGM() + scale_colour_manual(values = region_colors_sub)  + theme(legend.position = "none") + theme(legend.text=element_text(size=12, face="plain"), legend.title = element_text(size=12, face="bold"), axis.title=element_text(size=11, face="bold")) 

jpeg(filename = "tables_figures/Figure_S2b.jpeg", width = 14, height = 14, units = "cm", pointsize = 15, quality = 300, res = 300)
wave_energy_plot
dev.off()


## Figure S3
NPP.plot.lobo <- ggplot(cover_before, aes(y=meanLobo, x=NPP)) + geom_smooth(formula = y ~ x, color="black", method=glm, se=TRUE, method.args = list(family = "quasibinomial"), fill = "grey") + geom_point(size=3) + labs(x="", y=expression(bold(paste("Mean ", italic("Lobophytum "), "percent cover", sep = ""))), title="") + theme_DGM() + theme(axis.title=element_text(size=11, face="bold")) + ylim(0, 0.15)
NPP.plot.sinu <- ggplot(cover_before, aes(y=meanSinu, x=NPP)) + geom_smooth(formula = y ~ x, color="black", method=glm, se=TRUE, method.args = list(family = "quasibinomial"), fill = "grey") + geom_point(size=3) + labs(x=expression(bold(paste("NPP (mg C ", m^{-2}, " ", day^{-1}, ")", sep = ""))), y=expression(bold(paste("Mean ", italic("Sinularia "), "percent cover", sep = ""))), title="") + theme_DGM() + theme(axis.title=element_text(size=11, face="bold"))+ ylim(0, 0.15)

WindExposure.Plot.lobo <- ggplot(cover_before, aes(y=meanLobo, x=WE, fill = WE)) + geom_boxplot() + labs(x= "", y=expression(bold(paste("Mean ", italic("Lobophytum "), "percent cover", sep = ""))), title="") + theme_DGM() + scale_fill_manual(values = WindExposure_colors)+ theme(legend.position = "none", axis.title=element_text(size=11, face="bold")) + scale_x_discrete(breaks = c("windward", "sheltered"), labels = c("Windward", "Sheltered"))
WindExposure.Plot.sinu <- ggplot(cover_before, aes(y=meanSinu, x=WE, fill = WE)) + geom_boxplot() + labs(x= "WE", y=expression(bold(paste("Mean ", italic("Sinularia "), "percent cover", sep = ""))), title="") + theme_DGM() + scale_fill_manual(values = WindExposure_colors)+ theme(legend.position = "none", axis.title=element_text(size=11, face="bold")) + scale_x_discrete(breaks = c("windward", "sheltered"), labels = c("Windward", "Sheltered"))


Figure_S3_right <- plot_grid(WindExposure.Plot.lobo, WindExposure.Plot.sinu, rel_heights = c(1, 1), labels = c("a", "b"), label_fontfamily = "Times", ncol=1)
Figure_S3_left <- plot_grid(NPP.plot.lobo, NPP.plot.sinu, rel_heights = c(1, 1), labels = c("c", "d"), label_fontfamily = "Times", ncol=1)

Figure_S3 <- plot_grid(Figure_S3_right, NULL, Figure_S3_left, nrow = 1, rel_widths = c(1, 0.1, 1.4), label_fontfamily = "Times")

jpeg(filename = "tables_figures/Figure_S3.jpeg", width = 12, height = 7, units = "in", pointsize = 15, quality = 300, res = 300)
Figure_S3
dev.off()


