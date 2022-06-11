###############################################################################
##################### analysis code for gap overlap data ######################
##################### last updated by victoria 11 june 22 #####################
###############################################################################

# loading packages -----
require('tidyverse')
require('ggplot2')
require('readxl')
require("reshape2")
require('papaja')
require('effsize')
require('rstatix')
require('cowplot')
require('TOSTER')
library(extrafont)
loadfonts(device="pdf")
par(family = "LM Roman 10")

# loading data ----- 
gapdata <- read_excel("/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/gap/gap_results_20210716_07_55_17.xlsx")#; View(gapdata)
gaptrials <- read_excel("/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/gap/gap_trials_20210716_07_55_17.xlsx")#; View(gaptrials)
behavedata <- data.frame(read_excel('/Users/victoriamousley/Documents/MATLAB/BehaviouralData.xlsx'))

# cleaning data -----
idSplitter = function(x) unlist(str_split(x,"_"))[1]
gapdata$ID = sapply(gapdata$ID, idSplitter)

gapdata$ID[!gapdata$ID %in% behavedata$ID]
gapdata[gapdata == 'P11'] <- 'P011'
gapdata[gapdata == 'P078'] <- 'B078' 
gapdata[gapdata == 'B01'] <- 'B001'

finalDF <- merge(gapdata, behavedata, by = 'ID')

names(finalDF)[names(finalDF)=="ID"] <- "id"
names(finalDF)[names(finalDF)=="ID"] <- "id"
names(finalDF)[names(finalDF)=="Age"] <- "age" 
names(finalDF)[names(finalDF)=="Group"] <- "group"
names(finalDF)[names(finalDF)=="cum.voc.comp"] <- "receptive" 
names(finalDF)[names(finalDF)=="cum.voc.prod"] <- "productive"

finalDF$receptive <- as.numeric(finalDF$receptive, na.rm = TRUE)
finalDF$productive <- as.numeric(finalDF$productive, na.rm = TRUE)
finalDF$composite <- as.numeric(finalDF$composite, na.rm = TRUE)
finalDF$X24.comp <- as.numeric(finalDF$X24.comp, na.rm = TRUE)
finalDF$X24.prod <- as.numeric(finalDF$X24.prod, na.rm = TRUE)
finalDF$group <- as.factor(as.character(finalDF$group, na.rm = TRUE))
finalDF$VR_T <- as.numeric(finalDF$VR_T, na.rm = TRUE)
finalDF$GM_T <- as.numeric(finalDF$GM_T, na.rm = TRUE)
finalDF$FM_T <- as.numeric(finalDF$FM_T, na.rm = TRUE)
finalDF$EL_T <- as.numeric(finalDF$EL_T, na.rm = TRUE)
finalDF$RL_T <- as.numeric(finalDF$RL_T, na.rm = TRUE)
finalDF$P1.ed <- as.numeric(as.character(finalDF$P1.ed))
finalDF$SES <- as.numeric(as.character(finalDF$SES))

# Exclusion: those with fewer than 6 valid trials in any trial type
finalDF <- finalDF %>% filter(BASELINE_NumValid > 6) %>%
  filter(GAP_NumValid > 6) %>%
  filter(OVERLAP_NumValid > 6)

bi_gap <- finalDF %>% filter(group == 'B')
mono_gap <- finalDF %>% filter(group == 'M')

# descriptive stats -----
## checks for group diffs in background vars
t.test(bi_gap$age, mono_gap$age)

## manipulation check
srt <- finalDF %>% select(id, age, group, BASELINE_SRT_mean, GAP_SRT_mean, OVERLAP_SRT_mean) %>%
  pivot_longer(cols = c('BASELINE_SRT_mean','GAP_SRT_mean','OVERLAP_SRT_mean'), names_to = 'condition', values_to = 'srt') %>%
  droplevels()

man.aov <- anova_test(data = srt, dv = srt, wid = id, within = condition, effect.size = "pes"); man.aov

pwc_man <- srt %>%
  pairwise_t_test(srt ~ condition, paired = TRUE,
    p.adjust.method = 'bonferroni'); pwc_man

pwc_man <- pwc_man %>% add_xy_position(x = 'condition')
pwc_man$stars<- c('***', '***', '***')

manipulation_aov_plot <- srt %>% ggplot(aes(x = condition, y = srt))+
  geom_boxplot(aes(fill = condition)) +
  stat_pvalue_manual(pwc_man, label = 'stars', stacked = TRUE, hide.ns = FALSE) +
  labs(y='Mean SRTs (ms)', x = '',
       title = "Manipulation Check",
       subtitle = '') +
  scale_x_discrete(labels=c('Baseline', 'Gap', 'Overlap')) +
  scale_fill_manual(values=c('white', 'white', 'white')) +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = (14), margin = margin(t = 10)),
        # plot.caption = element_text(family = 'LM Roman 10', size = 10, margin = margin(t = 10, b = 10)),
        # axis.title.x = element_text(hjust = 0.5, family = 'LM Roman 10', size = 12, margin = margin(t = 10, b = 10)),
        axis.text.x = element_text(hjust = 0.5, family = 'LM Roman 10', size = 12, margin = margin(t = 10, b = 10)),
        axis.title.y = element_text(hjust = 0.5, family = 'LM Roman 10', size = 12, margin = margin(t = 10, r = 20)),
        axis.text.y = element_text(hjust = 0.5, family = "LM Roman 10", size = 12, margin = margin(t = 10, r = 20)),
        legend.position = 'none'); manipulation_aov_plot

# hyp 1: group difference on disengagement -----
t.test(bi_gap$DIS_SRT, mono_gap$DIS_SRT)
TOSTtwo(100.309, 75.064, 63.33, 62.02, 33, 39, -0.799, 0.799, alpha = 0.05, var.equal=FALSE, plot = TRUE)

dis_plot <- finalDF %>%
  ggplot(aes(x = group, y = DIS_SRT)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,350) + 
  scale_x_discrete(labels=c('Bilingual', 'Monolingual')) +
  scale_fill_manual(values=c('white', 'grey30')) +
  labs(y='Disengagement SRT (ms)', x = '',
       title = "Disengagement Saccadic Reaction Time by Group",subtitle = '') +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = 14, margin = margin(t = 10)),
        # plot.subtitle = element_text(hjust = 0.5, family = 'LM Roman 10', size = 11, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        # axis.title.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10, r = 10, b = 10, l = 0)),
        axis.text.y = element_text(family = "LM Roman 10", size = 12, margin = margin(t = 10, r = 10, b = 10, l = 0)),
        legend.position = 'none'); dis_plot

gapoverlapPlots <- cowplot::plot_grid(manipulation_aov_plot, dis_plot, labels = "AUTO",
                             label_fontfamily = 'LM Roman 10', label_fontface = "bold"); gapoverlapPlots

ggsave('GapOverlapResult.jpeg', 
       path = "/Users/victoriamousley/BASIS/github version/BASIS-ET-analysis/", width = 13, height = 8, device='tiff', dpi=300)
