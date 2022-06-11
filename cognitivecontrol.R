###############################################################################
################## analysis code for cognitive control data ###################
##################### last updated by victoria 11 june 22 #####################
###############################################################################

# loading packages -----
library(tidyverse)
library(ggplot2) 
library(readxl) 
library(cowplot) 
library(stringr)
library(papaja)
library(dplyr)
library(reshape2)
library(extrafont)
library(rstatix)
library(TOSTER)
loadfonts(device="pdf")
par(family = "LM Roman 10")

# loading data ----- 
ccdata <- read_excel("/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/cogcontrol/cogcontrol_20210716T075918.xlsx")#; View(ccdata)
behavedata <- data.frame(read_excel('/Users/victoriamousley/Documents/MATLAB/BehaviouralData.xlsx'))

# cleaning data -----
ccdata$PropCorrectAntiSac_Learn[is.na(ccdata$PropCorrectAntiSac_Learn)] <- 0
ccdata$PropCorrectAntiSac_Rev[is.na(ccdata$PropCorrectAntiSac_Rev)] <- 0

# Exclusion: those with no valid learning phase data
ccdata <- ccdata %>% filter(PropCorrectAntiSac_Learn != 0)

# average across blocks within participants
ccdata <- aggregate(ccdata, by = list(ccdata$ID),
                          mean, na.rm = FALSE)

ccdata <- ccdata[,-30]

names(ccdata)[names(ccdata)=="Group.1"] <- "ID" # change annoying name to nice one

idSplitter = function(x) unlist(str_split(x,"_"))[1]
ccdata$ID = sapply(ccdata$ID, idSplitter)

ccdata$ID[!ccdata$ID %in% behavedata$ID] 
ccdata[ccdata == 'P11'] <- 'P011'
ccdata[ccdata == 'A012'] <- 'P012' 
ccdata[ccdata == 'B01'] <- 'B001'

finalDF <- merge(ccdata, behavedata, by = 'ID')

names(finalDF)[names(finalDF)=="ID"] <- "id"
names(finalDF)[names(finalDF)=="Age"] <- "age"
names(finalDF)[names(finalDF)=="Group"] <- "group"
names(finalDF)[names(finalDF)=="cum.voc.comp"] <- "receptive"
names(finalDF)[names(finalDF)=="cum.voc.prod"] <- "productive"
names(finalDF)[names(finalDF)=="X24.comp"] <- "receptive_24"
names(finalDF)[names(finalDF)=="X24.prod"] <- "productive_24"
finalDF$group <- as.factor(as.character(finalDF$group))
finalDF$receptive <- as.numeric(as.character(finalDF$receptive))
finalDF$productive <- as.numeric(as.character(finalDF$productive))
finalDF$receptive_24 <- as.numeric(as.character(finalDF$receptive_24))
finalDF$productive_24 <- as.numeric(as.character(finalDF$productive_24))

finalDF$agem <- finalDF$age*(12/365)

ccdataD <- filter(finalDF, grepl('D', id))
ccdataH <- filter(finalDF, !grepl('D', id))

# descriptive stats -----
## checks for group diffs in background vars
bi_cc <- ccdataH %>% filter(group == 'B')
mono_cc <- ccdataH %>% filter(group == 'M')

t.test(bi_cc$age, mono_cc$age)

# hyp #1: prop correct antisaccades in reverse phase -----
cc_Prop <- ccdataH %>% select(id, age, group, PropCorrectAntiSac_Learn, PropCorrectAntiSac_Rev) %>%
  pivot_longer(cols = c('PropCorrectAntiSac_Learn','PropCorrectAntiSac_Rev'), names_to = 'phase', values_to = 'PropCorrectAntiSac') %>%
  droplevels()

ccProp.aov <- anova_test(data = cc_Prop, dv = PropCorrectAntiSac, wid = id, within = phase, between = group, effect.size = "pes"); ccProp.aov

cc_Prop_plot <- cc_Prop %>% ggplot(aes(x = phase, y = PropCorrectAntiSac, fill = group))+
  geom_boxplot() +
  labs(y='Proportion Correct', fill = 'Group', title = "") +
  scale_x_discrete(labels=c('Learn', 'Reverse')) +
  scale_fill_manual(values=c('white', 'grey30'), labels=c('Bilingual', 'Monolingual')) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = 14, margin = margin(t = 10, r = 0, b = 10, l = 0)),
    axis.text.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(r = 10)),
    axis.text.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(r = 10)),
    legend.title = element_text(family = 'LM Roman 10', face = 'bold', size = 10),
    legend.text = element_text(family = 'LM Roman 10', size = 10),
    legend.position = 'right'); cc_Prop_plot

# EQUIVALENCE TEST
# TOSTtwo(m1, m2, sd1, sd2, n1, n2, low_eqbound_d, high_eqbound_d, alpha, var.equal, plot = TRUE)
TOSTtwo(0.676, 0.703, 0.225, 0.243, 42, 41, -0.5, 0.5, alpha = 0.05, var.equal=FALSE, plot = TRUE)

# hyp #2: RT correct antisaccades in reverse phase -----
### reaction time analysis 
cc_RT <- ccdataH %>% select(id, age, group, RTCorrectAntiSacMean_Learn, RTCorrectAntiSacMean_Rev) %>%
  pivot_longer(cols = c('RTCorrectAntiSacMean_Learn','RTCorrectAntiSacMean_Rev'), names_to = 'phase', values_to = 'RTCorrectAntiSac') %>%
  droplevels()

ccRT.aov <- anova_test(data = cc_RT, dv = RTCorrectAntiSac, wid = id, within = phase, between = group, effect.size = "pes"); ccRT.aov

cc_RT_plot <- cc_RT %>% ggplot(aes(x = phase, y = RTCorrectAntiSac, fill = group))+
  geom_boxplot() + 
  labs(y='RT Correct (sec)', fill = 'Group', title = "") +
  scale_x_discrete(labels=c('Learn', 'Reverse')) +
  scale_fill_manual(values=c('white', 'grey30'), labels=c('Bilingual', 'Monolingual')) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = 14, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(r = 10)),
        axis.text.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(r = 10)),
        legend.title = element_text(family = 'LM Roman 10', face = 'bold', size = 10),
        legend.text = element_text(family = 'LM Roman 10', size = 10),
        legend.position = 'right'); cc_RT_plot

# TOSTtwo(m1, m2, sd1, sd2, n1, n2, low_eqbound_d, high_eqbound_d, alpha, var.equal, plot = TRUE)
TOSTtwo(0.783, 0.803, 0.300, 0.380, 42, 41, -0.5, 0.5, alpha = 0.05, var.equal=FALSE, plot = TRUE)

ccPlot <- cowplot::plot_grid(cc_Prop_plot, cc_RT_plot, labels = "AUTO",
                             label_fontfamily = 'LM Roman 10', label_fontface = "bold"); ccPlot

ggsave('CognitiveControlResults.jpeg', 
       path = "/Users/victoriamousley/BASIS/github version/BASIS-ET-analysis/", width = 12, height = 8, device='tiff', dpi=300)
