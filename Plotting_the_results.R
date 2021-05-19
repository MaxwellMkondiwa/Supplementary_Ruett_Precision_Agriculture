library(dplyr)
library(extrafont)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(ggstance)
library(tidyverse)
library(moments)

# Check fonts to use Times New Roman
extrafont::loadfonts(device="win")
extrafont::fonts()
font_import()
fonts()

# Plot NPV Baseline ####
data_G <- read.csv("MC_Results_CBA/mcSimulationResults.csv")
data_G <- dplyr::select(data_G, starts_with("NPV_G")) %>%
  stack(drop=FALSE)
data_G$values <- as.numeric(data_G$values) 
distribution_G <- ggplot(data_G, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("Baseline"), values = ("red4"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.2), width = 0.1, fill = "red4" ) +
  geom_vline(aes(xintercept = 0)) +
  ylim(breaks = c(0,1)) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""),
                     limits = c(-750000,1200000))+
  annotate("text", x = 500000, y = 0.8, label = 'atop(bold("Baseline"))', 
           size = 10, parse = TRUE,family="Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title=element_blank(),
        axis.text.x=element_text(colour = "white"),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

ggsave("distribution_G.png", device = "png",  width = 20, 
       height = 20, units = "cm")

# Plot NPV Improved ####
data_I <- read.csv("MC_Results_CBA/mcSimulationResults.csv")
data_I <- dplyr::select(data_I, starts_with("NPV_I")) %>%
  stack(drop=FALSE)
data_I$values <- as.numeric(data_I$values) 
distribution_I <- ggplot(data_I, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("Improved"), values = ("blue4"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.2), width = 0.1, fill = "blue4" ) +
  geom_vline(aes(xintercept = 0)) +
  ylim(breaks = c(0,1)) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""),
                     limits = c(-750000,1200000))+
  annotate("text", x = 500000, y = 0.8, label = 'atop(bold("Improved"))', 
           size = 10, parse = TRUE,family="Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title=element_blank(),
        axis.text.x=element_text(colour = "white"),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

ggsave("distribution_I.png", device = "png",  width = 20, 
       height = 20, units = "cm")

# Plot NPV Sensor ####
data_S <- read.csv("MC_Results_CBA/mcSimulationResults.csv")
data_S <- dplyr::select(data_S, starts_with("NPV_S")) %>%
  stack(drop=FALSE)
data_S$values <- as.numeric(data_S$values) 
distribution_S <- ggplot(data_S, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("Sensor"), values = ("yellow3"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.2), width = 0.1, fill = "yellow3" ) +
  geom_vline(aes(xintercept = 0)) +
  ylim(breaks = c(0,1)) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""),
                     limits = c(-750000,1200000))+
  annotate("text", x = 500000, y = 0.8, label = 'atop(bold("Sensor"))', 
           size = 10, parse = TRUE,family="Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title=element_blank(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

ggsave("distribution_S.png", device = "png",  width = 20, 
       height = 20, units = "cm")

# Plot NPV Improved-Baseline ####
data_IG <- read.csv("MC_Results_CBA/mcSimulationResults.csv")
data_IG <- dplyr::select(data_IG, starts_with("comp_NPV_IG")) %>%
  stack(drop=FALSE)
data_IG$values <- as.numeric(data_IG$values) 
distribution_IG <- ggplot(data_IG, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("DoMoreVisual"), values = ("darkorchid4"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.2), width = 0.1, fill = "darkorchid4" ) +
  geom_vline(aes(xintercept = 0)) +
  ylim(breaks = c(0,1)) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""),
                     limits = c(-750000,1200000))+
  annotate("text", x = 600000, y = 0.8, label = 'atop(bold("DoMoreVisual"))', 
           size = 10, parse = TRUE,family="Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title=element_blank(),
        axis.text.x=element_text(colour = "white"),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

ggsave("distribution_IG.png", device = "png",  width = 20, 
       height = 20, units = "cm")

# Plot NPV Sensor-Baseline ####
data_SG <- read.csv("MC_Results_CBA/mcSimulationResults.csv")
data_SG <- dplyr::select(data_SG, starts_with("comp_NPV_SG")) %>%
  stack(drop=FALSE)
data_SG$values <- as.numeric(data_SG$values) 
distribution_SG <- ggplot(data_SG, aes(x = values, y = ind, fill = ind)) +
  geom_density(aes(y=..scaled..), alpha = 0.5) +
  scale_fill_manual(labels = ("UseSensor"), values = ("darkorange3"),guide="legend") +
  geom_boxploth(aes(x = values, y = 0.2), width = 0.1, fill = "darkorange3" ) +
  geom_vline(aes(xintercept = 0)) +
  ylim(breaks = c(0,1)) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "", prefix = ""),
                     limits = c(-750000,1200000))+
  annotate("text", x = 600000, y = 0.8, label = 'atop(bold("UseSensor"))', 
           size = 10, parse = TRUE,family="Times New Roman") +
  theme(text=element_text(family="Times New Roman"),
        axis.title=element_blank(),
        axis.text.x=element_text(),
        axis.ticks.x=element_line(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=20),
        axis.text = element_text(size=20),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank())

ggsave("distribution_SG.png", device = "png",  width = 20, 
       height = 20, units = "cm")

# Merge NPV tables ####
three_NPV <- ggarrange(distribution_G, distribution_I, distribution_S, ncol = 1, nrow = 3)

three_NPV <- annotate_figure(three_NPV,
                             left = text_grob("Scaled Density", 
                                              color = "black", rot = 90,
                                              face = "bold", size =28, 
                                              family = "Times New Roman"),
                             bottom = text_grob("Net Present Value (Euros)", color = "black", 
                                                rot = 0,
                                                face = "bold", size =28, 
                                                family = "Times New Roman"))

ggsave("./three_NPV.png", three_NPV, 
       dpi = 800, device = "png", width = 8, height = 10)

two_NPV <- ggarrange(distribution_IG, distribution_SG, ncol = 1, nrow = 2)

two_NPV <- annotate_figure(two_NPV,
                           left = text_grob("Scaled Density", 
                                            color = "black", rot = 90,
                                            face = "bold", size =28, 
                                            family = "Times New Roman"),
                           bottom = text_grob("Net Present Value (Euros)", color = "black", 
                                              rot = 0,
                                              face = "bold", size =28, 
                                              family = "Times New Roman"))

ggsave("./two_NPV.png", two_NPV, 
       dpi = 800, device = "png", width = 8, height = 10)

# EVPI and VIP of Decisions ####
EVPI_data_Test_IG<-read.csv("EVPI_Results_CBA/EVPI_table_comp_NPV_IG.csv", header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$EVPI_do),] %>%
  subset(.,EVPI_do>0)
colnames(EVPI_data_Test_IG)[colnames(EVPI_data_Test_IG)=="X"] <- "Variable"
colnames(EVPI_data_Test_IG)[colnames(EVPI_data_Test_IG)=="EVPI_do"] <- "EVPI"
EVPI_data_Test_IG$Variable <- as.character(EVPI_data_Test_IG$Variable) 
EVPI_data_Test_IG$Variable[EVPI_data_Test_IG$Variable == "Number_of_saved_high_quality_plants_I"] <-"Improved: Saved high quality plants"
EVPI_data_Test_IG$Variable[EVPI_data_Test_IG$Variable == "labor_costs_I"] <-"Improved: Labor costs"
EVPI_data_Test_IG$Variable[EVPI_data_Test_IG$Variable == "Number_of_saved_high_quality_plants_G"] <-"Baseline: Saved high quality plants"
EVPI_data_Test_IG <- EVPI_data_Test_IG[order(EVPI_data_Test_IG$EVPI), ] 
EVPI_data_Test_IG$Variable <- factor(EVPI_data_Test_IG$Variable, levels = EVPI_data_Test_IG$Variable)
EVPI_data_Test_IG$EVPI <- round(EVPI_data_Test_IG$EVPI, digits= 2) %>%
  sprintf('%.2f', .) %>%
  as.numeric(.)


VIP_data_Tests_IG <- read.csv("./MC_Results_CBA/comp_NPV_IG_pls_results.csv",
                              header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  subset(.,VIP>0.8)

colnames(VIP_data_Tests_IG)[colnames(VIP_data_Tests_IG)=="X"] <- "Variable"
VIP_data_Tests_IG$Variable <- as.character(VIP_data_Tests_IG$Variable) 
VIP_data_Tests_IG$Variable[VIP_data_Tests_IG$Variable == "labor_costs_G"] <-"Baseline: Labor costs"
VIP_data_Tests_IG$Variable[VIP_data_Tests_IG$Variable == "Number_of_saved_high_quality_plants_G"] <-"Baseline: Saved high quality plants"
VIP_data_Tests_IG$Variable[VIP_data_Tests_IG$Variable == "labor_costs_I"] <-"Improved: Labor costs"
VIP_data_Tests_IG$Variable[VIP_data_Tests_IG$Variable == "Number_of_saved_high_quality_plants_I"] <-"Improved: Saved high quality plants"

VIP_data_Tests_IG <- VIP_data_Tests_IG[order(VIP_data_Tests_IG$VIP), ] 
VIP_data_Tests_IG$Variable <- factor(VIP_data_Tests_IG$Variable, levels = VIP_data_Tests_IG$Variable)
VIP_data_Tests_IG$VIP <- round(VIP_data_Tests_IG$VIP, digits= 2)%>%
  sprintf('%.2f', .) %>%
  as.numeric(.)
VIP_data_Tests_IG$color <- ifelse(VIP_data_Tests_IG$Coefficient>0, "steelblue1", "orange")
VIP_data_Tests_IG$color <- as.character(VIP_data_Tests_IG$color)

EVPI_data_Test_SG<-read.csv("EVPI_Results_CBA/EVPI_table_comp_NPV_SG.csv", header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$EVPI_dont),] %>%
  subset(.,EVPI_dont>0)
colnames(EVPI_data_Test_SG)[colnames(EVPI_data_Test_SG)=="X"] <- "Variable"
colnames(EVPI_data_Test_SG)[colnames(EVPI_data_Test_SG)=="EVPI_dont"] <- "EVPI"
EVPI_data_Test_SG$Variable <- as.character(EVPI_data_Test_SG$Variable) 
EVPI_data_Test_SG$Variable[EVPI_data_Test_SG$Variable == "labor_costs_S"] <-"Sensor: Labor costs"
EVPI_data_Test_SG$Variable[EVPI_data_Test_SG$Variable == "Number_of_saved_high_quality_plants_S"] <-"Sensor: Saved high quality plants"
EVPI_data_Test_SG <- EVPI_data_Test_SG[order(EVPI_data_Test_SG$EVPI), ] 
EVPI_data_Test_SG$Variable <- factor(EVPI_data_Test_SG$Variable, levels = EVPI_data_Test_SG$Variable)
EVPI_data_Test_SG$EVPI <- round(EVPI_data_Test_SG$EVPI, digits= 2) %>%
  sprintf('%.2f', .) %>%
  as.numeric(.)

VIP_data_Tests_SG <- read.csv("./MC_Results_CBA/comp_NPV_SG_pls_results.csv",
                              header = TRUE) %>%
  data.frame(.) %>%
  .[order(-.$VIP),] %>%
  subset(.,VIP>0.8)

colnames(VIP_data_Tests_SG)[colnames(VIP_data_Tests_SG)=="X"] <- "Variable"
VIP_data_Tests_SG$Variable <- as.character(VIP_data_Tests_SG$Variable) 
VIP_data_Tests_SG$Variable <- as.character(VIP_data_Tests_SG$Variable)
VIP_data_Tests_SG$Variable[VIP_data_Tests_SG$Variable == "Initial_investment_S"] <-"Sensor: Initial investment"
VIP_data_Tests_SG$Variable[VIP_data_Tests_SG$Variable == "labor_costs_G"] <-"Baseline: Labor costs"
VIP_data_Tests_SG$Variable[VIP_data_Tests_SG$Variable == "Number_of_saved_high_quality_plants_G"] <-"Baseline: Saved high quality plants"
VIP_data_Tests_SG$Variable[VIP_data_Tests_SG$Variable == "Number_of_saved_high_quality_plants_S"] <-"Sensor: Saved high quality plants"
VIP_data_Tests_SG$Variable[VIP_data_Tests_SG$Variable == "labor_costs_S"] <-"Sensor: Labor costs"

VIP_data_Tests_SG <- VIP_data_Tests_SG[order(VIP_data_Tests_SG$VIP), ] 
VIP_data_Tests_SG$Variable <- factor(VIP_data_Tests_SG$Variable, levels = VIP_data_Tests_SG$Variable)
VIP_data_Tests_SG$VIP <- round(VIP_data_Tests_SG$VIP, digits= 2)%>%
  sprintf('%.2f', .) %>%
  as.numeric(.)
VIP_data_Tests_SG$color <- ifelse(VIP_data_Tests_SG$Coefficient>0, "steelblue1", "orange")
VIP_data_Tests_SG$color <- as.character(VIP_data_Tests_SG$color)

# IG ####
IG <- merge(VIP_data_Tests_IG, EVPI_data_Test_IG, by = "Variable", all.x = TRUE)
IG$EVPI[is.na(IG$EVPI)] <- 0

q <-  ggplot(IG, aes(x = Variable, y = EVPI, fill = EVPI))+
  geom_bar(width = 1, stat = "identity", color = "black", fill = "purple") +
  ggtitle("Nothing", subtitle ="Information Value") +
  theme_bw()+theme(panel.grid = element_line())+ 
  ylab("EVPI")+ 
  xlab(NULL)+
  scale_y_continuous(labels = scales::dollar_format(suffix = "", prefix = ""),
                     limits = c(0, 16000), breaks = c(0,7500,15000)) +
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15), 
        axis.text.y = element_blank(),
        axis.line.y.left = element_blank(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        legend.position = "none") +
  coord_flip()

IG$X <- NULL
IG$expected_gain <- NULL
p <- ggplot(IG,aes(x=Variable,y=VIP))+
  geom_bar(width = 1,aes(fill=color),stat ="identity", color = "black")+ 
  ggtitle("Nothing", subtitle ="Variable Importance") +
  ylab("VIP")+
  xlab(NULL)+
  theme_bw()+theme(panel.grid=element_line())+
  scale_fill_manual(values = c("orange","steelblue1") )+
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        legend.position = "none",
        axis.ticks =  element_line(),
        axis.text.y = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.y.right = element_line(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        panel.background = element_blank()) +
  scale_y_reverse(lim = c(6, 0)) +
  coord_flip()

G.mid <- ggplot(IG,aes(x=1,y=Variable))+geom_text(aes(label=Variable))+
  geom_segment(aes(x=0,xend=0,yend=Variable))+
  geom_segment(aes(x=0,xend=0,yend=Variable))+
  ggtitle("DoMoreVisual", subtitle = "Result probably positive") +
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(1.0,1.0))+
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(hjust = 0.5,color = "darkgreen", size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5, color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y.left = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"),
        axis.line.y = element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=15, color=NA),
        axis.ticks.x=element_line(size=15, color=NA),
        plot.margin = unit(c(1,0,1,0), "mm"))
gg1 <- ggplot_gtable(ggplot_build(p))
gg2 <- ggplot_gtable(ggplot_build(q))
gg.mid <- ggplot_gtable(ggplot_build(G.mid))
IG_plot <- cowplot::plot_grid(gg1,gg.mid,gg2, ncol = 3, align = "h")

# SG ####
SG <- merge(VIP_data_Tests_SG, EVPI_data_Test_SG, by = "Variable", all.x = TRUE)
SG$EVPI[is.na(SG$EVPI)] <- 0

q <-  ggplot(SG, aes(x = Variable, y = EVPI, fill = EVPI))+
  geom_bar(width = 1, stat = "identity", color = "black", fill = "purple") +
  ggtitle("Nothing", subtitle ="Information Value") +
  theme_bw()+theme(panel.grid=element_line())+ 
  ylab("EVPI")+ 
  xlab(NULL)+
  scale_y_continuous(labels = scales::dollar_format(suffix = "", prefix = ""),
                     limits = c(0, 16000), breaks = c(0,7500,15000)) +
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15), 
        axis.text.y = element_blank(),
        axis.line.y.left = element_blank(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        legend.position = "none") +
  coord_flip()

SG$X <- NULL
SG$expected_gain <- NULL
p <- ggplot(SG,aes(x=Variable,y=VIP))+
  geom_bar(width = 1,aes(fill=color),stat ="identity", color = "black")+ 
  ggtitle("Nothing", subtitle ="Variable Importance") +
  ylab("VIP")+
  xlab(NULL)+
  theme_bw()+theme(panel.grid=element_line())+
  scale_fill_manual(values = c("orange","steelblue1") )+
  theme(text=element_text(family="Times New Roman"),
        plot.title = element_text(hjust = 0.5,color = "white", size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,color = "black", size = 15, face = "bold"),
        legend.position = "none",
        axis.ticks =  element_line(),
        axis.text.y = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.ticks.y.right = element_line(),
        plot.margin = unit(c(1,1,1,1), "mm"),
        axis.title.x=element_text(),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15,face="bold"),
        panel.background = element_blank()) +
  scale_y_reverse(lim = c(6, 0)) +
  coord_flip()

G.mid <- ggplot(SG,aes(x=1,y=Variable))+geom_text(aes(label=Variable))+
  geom_segment(aes(x=0,xend=0,yend=Variable))+
  geom_segment(aes(x=0,xend=0,yend=Variable))+
  ggtitle("UseSensor", subtitle = "Result probably negative") +
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(1.0,1.0))+
  theme(text=element_text(family="Times New Roman"),
        plot.subtitle = element_text(hjust = 0.5,color = "red", size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5, color = "black", size = 15, face = "bold"),
        axis.title.y = element_text(color="black", size=15),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y.left = element_line(color = "black"),
        axis.ticks.y.right = element_line(color = "black"),
        axis.line.y = element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=15, color=NA),
        axis.ticks.x=element_line(size=15, color=NA),
        plot.margin = unit(c(1,0,1,0), "mm"))
gg1 <- ggplot_gtable(ggplot_build(p))
gg2 <- ggplot_gtable(ggplot_build(q))
gg.mid <- ggplot_gtable(ggplot_build(G.mid))
SG_plot <- cowplot::plot_grid(gg1,gg.mid,gg2, ncol = 3, align = "h")

All_cowplot <- cowplot::plot_grid(IG_plot, SG_plot, ncol = 1, nrow = 2)
ggsave("./All_cowplot.png", All_cowplot, device = "png", width = 19, dpi = 200, height = 16, units =  "cm")
