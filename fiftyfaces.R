###############################################################################
###################### analysis code for 50 faces data ########################
##################### last updated by victoria 11 june 22 #####################
###############################################################################

# loading packages -----
require("praise")
require("ggplot2")
require("readxl")
require("reshape2")
require("tidyverse")
require("dplyr")
require("lme4")
require("cowplot")
require("papaja")
require("cowplot")
require("extrafont")
require("showtext")
require('rstatix')
require('TOSTER')
library(extrafont)
loadfonts(device="pdf")
par(family = "LM Roman 10")

# loading data ----- 
df1 <- data.frame(read_excel("/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/50faces_wide_faces 1.m4v_20210716T075618.xlsx"))
df2 <- data.frame(read_excel("/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/50faces_wide_faces 2.m4v_20210716T075654.xlsx"))
df3 <- data.frame(read_excel("/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/50faces_wide_faces 3.m4v_20210716T075733.xlsx"))
fifty <- rbind(df1, df2, df3)

behavedata <- data.frame(read_excel("/Users/victoriamousley/Documents/MATLAB/BehaviouralData.xlsx"))

# Exclusion:
## 1) Valid trials must have more than 2 seconds of total looking
## 2) Participants must have 2 or more valid trials to be included in analyses

# 1) Trials > 2 sec looking
fifty$time_total <- NA 
for (row in 1:nrow(fifty)){ 
  fifty$time_total[row] = sum(fifty$body_timeInAOI[row], fifty$outer_face_timeInAOI[row], 
                               fifty$face_timeInAOI[row], fifty$bg_people_timeInAOI[row])}

fifty <- subset(fifty, fifty$time_total >= 2) 

# 2) Participants > 2 valid trials
idFreq <- data.frame(table(fifty$id)) 
oneTrialList <- vector() 
onePlusTrialList <- vector() 
for (row in 1:nrow(idFreq)) { 
  id <- toString(idFreq[row, "Var1"]) 
  freq  <- idFreq[row, "Freq"]
  if(freq <= 1 ) { 
    oneTrialList <- c(oneTrialList, id) 
    print(paste(id, "is less than 1")) 
  } else { 
    onePlusTrialList <- c(onePlusTrialList, id)
    next 
  }
}


idSplitter = function(x) unlist(str_split(x,"#"))[1]
fifty$id = sapply(fifty$id, idSplitter)

fifty$id[!fifty$id %in% behavedata$ID]
fifty[fifty == 'A012'] <- 'B012'
fifty[fifty == 'B01'] <- 'B001'
fifty[fifty == 'P11'] <- 'P011'

fifty <- aggregate(fifty, by = list(fifty$id),
                   mean, na.rm = TRUE)

names(fifty)[names(fifty)=="Group.1"] <- "id"
fifty <- fifty[,-2]

fifty <- fifty[-c(67:82),] # including only pre-COVID participants for analysis

fifty$EMI <- fifty$eyes_timeInAOI / (fifty$eyes_timeInAOI + fifty$mouth_timeInAOI)
fifty$PDT <- fifty$mouth_timeInAOI / fifty$time_total
names(fifty)[names(fifty)=="mouth_meanLook"] <- "ADL"

fifty <- merge(fifty, behavedata, by.x = 'id', by.y = 'ID')

names(fifty)[names(fifty)=="Group"] <- "group" 

fifty$cum.voc.comp <- as.numeric(fifty$cum.voc.comp)
fifty$cum.voc.prod <- as.numeric(fifty$cum.voc.prod)
fifty$Degree.of.bi <- as.numeric(fifty$Degree.of.bi)
fifty$lang.mixing.scale <- as.numeric(fifty$lang.mixing.scale)
fifty$X24.comp <- as.numeric(fifty$X24.comp)
fifty$X24.prod <- as.numeric(fifty$X24.prod)
fifty$composite <- as.numeric(fifty$composite)
fifty$Group <- as.factor(fifty$Group)
fifty$mat_ed <- as.numeric(fifty$mat_ed)
fifty$pat_ed <- as.numeric(fifty$pat_ed)
fifty$SES <- as.numeric(fifty$SES)
fifty$composite <- as.numeric(fifty$composite)
fifty$GM_T <- as.numeric(fifty$GM_T)
fifty$VR_T <- as.numeric(fifty$VR_T)
fifty$RL_T <- as.numeric(fifty$RL_T)
fifty$FM_T <- as.numeric(fifty$FM_T)
fifty$EL_T <- as.numeric(fifty$EL_T)

fifty <- fifty %>% filter(group != 'D')
fiftyBi <- fifty %>% filter(group == "B")
fiftyMono <- fifty %>% filter(group == "M")

# hyp 1: EMI -----
t.test(fiftyBi$EMI, fiftyMono$EMI)

# perform equivalence testing for an independent t-test
TOSTtwo(m1 = 0.466, m2 = 0.537, sd1 = 0.261, sd2 = 0.292, n1 = 29, n2 = 36, low_eqbound_d = -0.5,
high_eqbound_d = 0.5, alpha = 0.05, var.equal = FALSE, plot = TRUE, verbose = TRUE)

# hyp 2: PDT -----
t.test(fiftyBi$PDT, fiftyMono$PDT)

#TOSTtwo(m1, m2, sd1, sd2, n1, n2, low_eqbound_d, high_eqbound_d, alpha, var.equal, plot = TRUE)
TOSTtwo(m1 = 0.24, m2 = 0.19, sd1 = 0.12, sd2 = 0.14, n1 = 29, n2 = 36, low_eqbound_d = -0.5, high_eqbound_d = 0.5, alpha = 0.05, var.equal=FALSE, plot = TRUE, verbose = TRUE)

# hyp 3: ADL -----
t.test(fiftyBi$ADL, fiftyMono$ADL)

# exploratory: prop of fixations on mouth over time -----
trials <- read.csv('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/50faces/eventAnalysis/propLookToMouth.csv')
trials$X <- NULL

trials$talkStatus <- as.numeric(as.factor(as.character(trials$talkStatus)))

trialsAgg <- trials %>% filter(talkStatus == 2) %>%
  pivot_longer(cols = c('propMono','propBi'), names_to = 'group', values_to = 'propFix') %>%
  droplevels()

trials1 <- trials %>% filter(trial == 1)
trials1$trial <- NULL
trials2 <- trials %>% filter(trial == 2)
trials2$trial <- NULL
trials3 <- trials %>% filter(trial == 3)
trials3$trial <- NULL

trials1 <- melt(trials1,id=c('time', 'talkStatus'), na.rm=TRUE)
names(trials1)[names(trials1)=="variable"] <- "group" 
names(trials1)[names(trials1)=="value"] <- "propFix"
trials1$talkStatus <- as.factor(trials1$talkStatus)

trials1_plot <- ggplot(trials1, aes(time, propFix, color = group)) +
  geom_smooth() +
  annotate("rect", fill = 'yellow', xmin= 4.641098, xmax= 7.324076, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 8.123940, xmax= 9.607117, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 10.523616, xmax= 12.481795, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 12.723404, xmax= 12.965029, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 13.164991, xmax= 13.2026659, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  scale_fill_manual(values = alpha(c("blue", "red"), 0.2)) +
  labs(title = 'Trial 1', x = "Time (sec)", y = 'Proportion of Fixations', color = 'Group') +
  scale_color_manual(values = c('black', 'grey60'), labels=c('Monolingual', 'Bilingual')) +
  scale_x_continuous(breaks = 0:13) +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'LM Roman 10', size = 12),
        axis.title.x = element_text(family = "LM Roman 10", size = 12),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12),
        axis.text.y = element_text(family = 'LM Roman 10', size = 12),
        legend.title = element_text(family = 'LM Roman 10', face = 'bold', size = 12),
        legend.text = element_text(family = 'LM Roman 10', size = 12),
        legend.position = 'none'); trials1_plot

trials2 <- melt(trials2,id=c('time', 'talkStatus'), na.rm=TRUE)
names(trials2)[names(trials2)=="variable"] <- "group"
names(trials2)[names(trials2)=="value"] <- "propFix"
trials2$talkStatus <- as.factor(trials2$talkStatus)

trials2_plot <- ggplot(trials2, aes(time, propFix, color = group)) +
  geom_smooth() +
  annotate("rect", fill = 'yellow', xmin= 2.524723, xmax= 3.366262, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 3.724528, xmax= 4.482778, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 5.882594, xmax= 9.082172, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 9.165511, xmax= 11.040268, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  scale_fill_manual(values = alpha(c("blue", "red"), 0.2)) +
  labs(title = ' Trial 2', x = "Time (sec)", y = 'Proportion of Fixations', color = 'Group') +
  scale_color_manual(values = c('black', 'grey60'), labels=c('Monolingual', 'Bilingual')) +
  scale_x_continuous(breaks = 0:11) +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'LM Roman 10', size = 12),
        axis.title.x = element_text(family = "LM Roman 10", size = 12),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12),
        axis.text.y = element_text(family = 'LM Roman 10', size = 12),
        legend.title = element_text(family = 'LM Roman 10', face = 'bold', size = 12),
        legend.text = element_text(family = 'LM Roman 10', size = 12),
        legend.position = 'none'); trials2_plot

trials3 <- melt(trials3,id=c('time', 'talkStatus'), na.rm=TRUE) # format data for aov
names(trials3)[names(trials3)=="variable"] <- "group" # change annoying name to nice one
names(trials3)[names(trials3)=="value"] <- "propFix" # change annoying name to nice one
trials3$talkStatus <- as.factor(trials3$talkStatus)

trials3_plot <- ggplot(trials3, aes(time, propFix, color = group)) +
  geom_smooth() +
  annotate("rect", fill = 'yellow', xmin= 0.441535, xmax= 1.808085, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 1.966411, xmax= 5.765950, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 7.840640, xmax= 9.082164, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  annotate("rect", fill = 'yellow', xmin= 9.882076, xmax= 11.406873, ymin=-Inf, ymax=Inf, alpha = 0.2) +
  scale_fill_manual(values = alpha(c("blue", "red"), 0.2)) +
  labs(title = 'Trial 3', x = "Time (sec)", y = 'Proportion of Fixation', color = 'Group') +
  scale_color_manual(values = c('black', 'grey60'), labels=c('Monolingual', 'Bilingual')) +
  scale_x_continuous(breaks = 0:12) +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = (14)),
        plot.subtitle = element_text(hjust = 0.5, family = 'LM Roman 10', size = 12),
        axis.title.x = element_text(family = "LM Roman 10", size = 12),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12),
        axis.text.y = element_text(family = 'LM Roman 10', size = 12),
        legend.title = element_text(family = 'LM Roman 10', face = 'bold', size = 12),
        legend.text = element_text(family = 'LM Roman 10', size = 12),
        legend.position = 'none'); trials3_plot

trials <- cowplot::plot_grid(trials1_plot, trials2_plot, trials3_plot, labels = "AUTO",
                             label_fontfamily = 'LM Roman 10', label_fontface = "bold"); trials

ggsave('50FacesFixationsByTrial.jpeg', 
       path = "/Users/victoriamousley/BASIS/github version/BASIS-ET-analysis/", width = 10, height = 8, device='tiff', dpi=300)
