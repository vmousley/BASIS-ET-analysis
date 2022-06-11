###############################################################################
##################### analysis code for face pop-out data #####################
##################### last updated by victoria 11 june 22 #####################
###############################################################################

# loading packages -----
require("lme4")
require("ggplot2")
require("readxl")
require("reshape2")
require("tidyverse")
require("cowplot")
require("extrafont")
require('rstatix')
require('effsize') 
require('TOSTER')
require('papaja')
require('ggpubr')
library(extrafont)
loadfonts(device="pdf")
par(family = "LM Roman 10")

# loading data ----- 
df <- read_excel('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/popout/popout_wide_20210716T075310.xlsx') # read in tall excel file from lm_analysis folder 
df2 <- read_excel('/Users/victoriamousley/Documents/MATLAB/lm_preproc/lm_analysis/popout/popout_tall_20210716T075310.xlsx')
behavedata <- data.frame(read_excel('/Users/victoriamousley/Documents/MATLAB/BehaviouralData.xlsx'))
  
# cleaning data -----
df[df == 65535] <- NA 
totalPop <- length(unique(df$id))

# Exclusion:
## 1) trials must have more than 1 second of looking
## 2) participants must have fewer than 5 trials

# 1) VALID TRIALS MUST BE > 1 SECOND
df$time_total <- NA 
for (row in 1:nrow(df)){ 
  df$time_total[row] = sum(df$face_timeInAOI[row], df$car_timeInAOI[row], 
                           df$phone_timeInAOI[row], df$noise_timeInAOI[row],
                           df$bird_timeInAOI[row])
}
df1 <- subset(df, df$time_total > 1) 
df2 <- subset(df, df$time_total < 1)

# 2) 5 TRIALS PER PARTICIPANT
a <- data.frame(table(df1$id)) 
mylist <- vector() 
for (row in 1:nrow(a)) {
  id <- toString(a[row, "Var1"])
  freq  <- a[row, "Freq"] 
  if(freq <= 5) { 
    mylist <- c(mylist, id) 
    print(paste(id, "is less than 5")) 
  } else {
    next
  }
}

mydf <- df1[!df1$id %in% mylist, ] 
mydf <- mydf %>% arrange(id) 

# calculate variables of interest
flatlist <- vector() 
clatlist <- vector() 
platlist <- vector() 
nlatlist <- vector() 
blatlist <- vector() 
nonflatlist <- vector() 
nonclatlist <- vector()
nonplatlist <- vector() 
nonnlatlist <- vector() 
nonblatlist <- vector()

fcountlist <- vector() 
ccountlist <- vector() 
pcountlist <- vector() 
ncountlist <- vector()
bcountlist <- vector() 
nonfcountlist <- vector()

fdurlist <- vector()
cdurlist <- vector()
pdurlist <- vector()
ndurlist <- vector()
bdurlist <- vector()
nonfdurlist <- vector()

ids <- unique(mydf$id)
idList <- vector()

for (x in ids){ # for every id in object ids
  dfLoop <- mydf[which(mydf$id %in% c(x)),] # make a dataframe of trials for each id in mydf
  idList <- c(idList,x) # add id values to list to make column of ids
  
  flat <- mean(dfLoop$face_firstTimeS, na.rm = TRUE) # latency face measure, ignore empty
  flatlist <- c(flatlist, flat) # put mean value in an ordered list
  
  clat <- mean(dfLoop$car_firstTimeS, na.rm = TRUE) # latency car measure, ignore empty
  clatlist <- c(clatlist, clat) # put mean value in an ordered list
  
  plat <- mean(dfLoop$phone_firstTimeS, na.rm = TRUE) # latency phone measure, ignore empty
  platlist <- c(platlist, plat) # put mean value in an ordered list
  
  nlat <- mean(dfLoop$noise_firstTimeS, na.rm = TRUE) # latency noise measure, ignore empty
  nlatlist <- c(nlatlist, nlat) # put mean value in an ordered list
  
  blat <- mean(dfLoop$bird_firstTimeS, na.rm = TRUE) # latency bird measure, ignore empty
  blatlist <- c(blatlist, blat) # put mean value in an ordered list
  
  nonflat <- mean(c(clat, plat, nlat, blat), na.rm = TRUE) # global non-face latency measure, ignore empty
  nonflatlist <- c(nonflatlist, nonflat) # put mean value in an ordered list
  
  nonclat <- mean(c(flat, plat, nlat, blat), na.rm = TRUE) # global non-face latency measure, ignore empty
  nonclatlist <- c(nonclatlist, nonclat) # put mean value in an ordered list
  
  nonplat <- mean(c(flat, clat, nlat, blat), na.rm = TRUE) # global non-face latency measure, ignore empty
  nonplatlist <- c(nonplatlist, nonplat) # put mean value in an ordered list
  
  nonnlat <- mean(c(flat, clat, plat, blat), na.rm = TRUE) # global non-face latency measure, ignore empty
  nonnlatlist <- c(nonnlatlist, nonnlat) # put mean value in an ordered list
  
  nonblat <- mean(c(flat, clat, plat, nlat), na.rm = TRUE) # global non-face latency measure, ignore empty
  nonblatlist <- c(nonblatlist, nonblat) # put mean value in an ordered list
  
  fcount <- mean(dfLoop$face_numLooks, na.rm = TRUE) # fixation count face measure, ignore empty
  fcountlist <- c(fcountlist, fcount) # put mean value in an ordered list
  
  ccount <- mean(dfLoop$car_numLooks, na.rm = TRUE) # fixation count car measure, ignore empty
  ccountlist <- c(ccountlist, ccount) # put mean value in an ordered list
  
  pcount <- mean(dfLoop$phone_numLooks, na.rm = TRUE) # fixation count phone measure, ignore empty
  pcountlist <- c(pcountlist, pcount) # put mean value in an ordered list
  
  ncount <- mean(dfLoop$noise_numLooks, na.rm = TRUE) # fixation count noise measure, ignore empty
  ncountlist <- c(ncountlist, ncount) # put mean value in an ordered list
  
  bcount <- mean(dfLoop$bird_numLooks, na.rm = TRUE) # fixation count bird measure, ignore empty
  bcountlist <- c(bcountlist, bcount) # put mean value in an ordered list
  
  nonfcount <- mean(c(ccount, pcount, ncount, bcount), na.rm = TRUE) # fixation count non-face measure, ignore empty
  nonfcountlist <- c(nonfcountlist, nonfcount) # # put mean value in an ordered list

  fdur <- mean(dfLoop$face_timeInAOI, na.rm = TRUE)
  fdurlist <- c(fdurlist, fdur)
  
  cdur <- mean(dfLoop$car_timeInAOI, na.rm = TRUE)
  cdurlist <- c(cdurlist, cdur)
    
  pdur <- mean(dfLoop$phone_timeInAOI, na.rm = TRUE)
  pdurlist <- c(pdurlist, pdur)
    
  ndur <- mean(dfLoop$noise_timeInAOI, na.rm = TRUE)
  ndurlist <- c(ndurlist, ndur)
    
  bdur <- mean(dfLoop$bird_timeInAOI, na.rm = TRUE)
  bdurlist <- c(bdurlist, bdur)

  nonfdur <- mean(c(cdur, pdur, ndur, bdur), na.rm = TRUE)
  nonfdurlist <- c(nonfdurlist, nonfdur)
}

s <- data.frame(ids, flatlist, clatlist, platlist, nlatlist, blatlist, nonflatlist, nonclatlist, nonblatlist, nonplatlist, nonnlatlist,
                fcountlist, ccountlist, pcountlist, ncountlist, bcountlist, nonfcountlist,
                fdurlist, cdurlist, pdurlist, ndurlist, bdurlist, nonfdurlist)

idSplitter = function(x) unlist(str_split(x,"_"))[1]
s$ids = sapply(s$ids, idSplitter)

names(behavedata)[names(behavedata)=="ID"] <- "ids" 

s$ids[!s$ids %in% behavedata$ids] 
s[s == 'P11'] <- 'P011' 
s[s == 'B01'] <- 'B001' 
s[s == 'A012'] <- 'P012' 

fpop_only <- merge(s, behavedata, by = 'ids')

names(fpop_only)[names(fpop_only)=="Age"] <- "age" 
names(fpop_only)[names(fpop_only)=="Group"] <- "group" 
names(fpop_only)[names(fpop_only)=="X24.comp"] <- "receptive_24" 
names(fpop_only)[names(fpop_only)=="X24.prod"] <- "productive_24" 

fpop_only$group <- as.factor(as.character(fpop_only$group))
fpop_only$P1.ed <- as.numeric(fpop_only$P1.ed)
fpop_only$P2.ed <- as.numeric(fpop_only$P2.ed)
fpop_only$SES <- as.numeric(fpop_only$SES)
fpop_only$p1.race <- as.numeric(fpop_only$p1.race)
fpop_only$p2.race <- as.numeric(fpop_only$p2.race)
fpop_only$cum.voc.comp <- as.numeric(fpop_only$cum.voc.comp)
fpop_only$cum.voc.prod <- as.numeric(fpop_only$cum.voc.prod)
fpop_only$mat_ed <- as.numeric(fpop_only$mat_ed)
fpop_only$pat_ed <- as.numeric(fpop_only$pat_ed)
fpop_only$Nursery.hrs <- as.numeric(fpop_only$Nursery.hrs)
fpop_only$Degree.of.bi <- as.numeric(fpop_only$Degree.of.bi)
fpop_only$lang.mixing.scale <- as.numeric(fpop_only$lang.mixing.scale)
fpop_only$agem <- fpop_only$age * (12/365)

fpop_onlyWholeGroup <- fpop_only # saves data frame for all pre- and post-pandemic participants
finalDF <- fpop_only[c(1:56,74,75),] # manually separate 58 pre pandemic for analysis 

fpopMono <- finalDF %>% filter(group == 'M')
fpopMono$group <- droplevels(fpopMono$group)

fpopBi <- finalDF %>% filter(group == 'B')
fpopBi$group <- droplevels(fpopBi$group)

fpopD <- filter(fpop_onlyWholeGroup, grepl('D', ids))
fpopD$group <- droplevels(fpopD$group)

# descriptive stats -----
## normality tests & transforms on relevant vars
shapiro.test(finalDF$flatlist)
finalDF$FlatLog <- log(finalDF$flatlist)
shapiro.test(finalDF$FlatLog)

shapiro.test(finalDF$nonflatlist)
finalDF$NonFlatLog <- log(finalDF$nonflatlist)
shapiro.test(finalDF$NonFlatLog)

shapiro.test(finalDF$fcountlist)

# checks for group diffs in background vars
t.test(fpopMono$age, fpopBi$age)
t.test(fpopMono$SES, fpopBi$SES)
t.test(fpopMono$mat_ed, fpopBi$mat_ed)
t.test(fpopMono$pat_ed, fpopBi$pat_ed)
t.test(fpopMono$Nursery.hrs, fpopBi$Nursery.hrs)
t.test(fpopMono$cum.voc.comp, fpopBi$cum.voc.comp)
t.test(fpopMono$cum.voc.prod, fpopBi$cum.voc.prod)

# pre-reg analyses -----
## assumptions of ANOVA
### 1: random sampling
### 2: equal variance
### 3: independence of errors
### 4: normal distribution of errors
### 5: additivity of treatment effects

## hypothesis #1: latency -----
lat2 <- finalDF %>% select(ids, group, age, flatlist, nonflatlist) %>%
  pivot_longer(cols = c('flatlist','nonflatlist'), names_to = 'aoi', values_to = 'lat') %>%
  droplevels()

lat2aov <- anova_test(data = lat2, dv = lat, wid = ids, within = aoi, between = group, effect.size = 'pes')

facelat2 <- lat2 %>% filter(aoi == 'flatlist') %>%
  t_test(lat ~ group, p.adjust.method = 'bonferroni',
         detailed = TRUE)

nonfacelat2 <- lat2 %>% filter(aoi == 'nonflatlist') %>%
  t_test(lat ~ group, p.adjust.method = 'bonferroni',
         detailed = TRUE)

cohenFace <- lat2 %>% filter(aoi == 'flatlist')
cohen.d(cohenFace$lat, cohenFace$group, var = FALSE)

cohenNonFace <- lat2 %>% filter(aoi == 'nonflatlist')
cohen.d(cohenNonFace$lat, cohenNonFace$group, var = FALSE)

nonfacelat2$stars <- c('**')
nonfacelat2 <- nonfacelat2 %>% add_xy_position(x = 'aoi')

latencyplot <- lat2 %>% ggplot(aes(x = aoi, y = lat))+
  geom_boxplot(aes(fill = group)) +
  labs(y='Fixation Latency (sec)', x = 'Areas of Interest (AOI)', 
       title = 'Attention Capture', 
       subtitle = '', fill = 'Group') +
  scale_x_discrete(labels=c('Face', 'Non Face')) +
  stat_pvalue_manual(nonfacelat2, label = 'stars', y.position = 5) +
  scale_fill_manual(values=c('white', 'grey30'), label=c('Bilingual', 'Monolingual')) +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = 14, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10, b = 10)),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10, b = 10)),
        axis.text.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(l = 10, r = 10)),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(l = 10, r = 10)),
        legend.title = element_text(family = 'LM Roman 10', face = 'bold', size = 12),
        legend.text = element_text(family = 'LM Roman 10', size = 12),
        legend.position = 'right'); latencyplot 

## check result is the same with transformed data 
lat3 <- finalDF %>% select(ids, group, age, FlatLog, NonFlatLog) %>%
  pivot_longer(cols = c('FlatLog','NonFlatLog'), names_to = 'aoi', values_to = 'lat') %>%
  droplevels()

secondlat2aov <- anova_test(data = lat3, dv = lat, wid = ids, within = aoi, between = group, effect.size = "pes")

## follow-up with all non-face AOIs per pre-reg plan
lat5 <- finalDF %>% select(ids, group, age, flatlist, clatlist, blatlist, nlatlist, platlist) %>%
  pivot_longer(cols = c('flatlist','clatlist', 'blatlist', 'nlatlist', 'platlist'), names_to = 'aoi', values_to = 'lat') %>%
  droplevels()

lat5ttests <- lat5 %>%
  t_test(lat ~ group, p.adjust.method = 'bonferroni',
         detailed = TRUE)

lat5ttests$stars <- c('***')
lat5ttestsLab <- lat5ttests %>% add_xy_position(x = 'aoi')

my_comparisons <- list(c("flatlist","clatlist"),c("flatlist","platlist"),c("flatlist","nlatlist"),c("flatlist","blatlist"))

lat5plot <- lat5 %>% ggplot(aes(x = aoi, y = lat))+
  geom_boxplot() +
  labs(y='Fixation Latency (sec)', x = 'Areas of Interest (AOI)', title = 'Attention Capture',
       subtitle = '', fill = 'Group') +
  scale_x_discrete(labels=c('Face', 'Car', 'Phone', 'Scrambled Face', 'Bird')) +
  scale_fill_manual(values=c('white', 'grey30')) +
  ylim(0,11)+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y=c(7.5,8.5,9.5,10.5)) +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = 14, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.title.x = element_text(family = 'LM Roman 10', size = 10, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.text.x = element_text(family = 'LM Roman 10', size = 10, margin = margin(t = 10, r = 0, b = 10, l = 0)),
        axis.text.y = element_text(family = 'LM Roman 10', size = 10, margin = margin(t = 0, r = 10, b = 0, l = 10)),
        axis.title.y = element_text(family = 'LM Roman 10', size = 10, margin = margin(t = 0, r = 10, b = 0, l = 10)),
        legend.position = 'none'); lat5plot

## hypotheses #2: maintenance -----
count2 <- finalDF %>% select(ids, group, age, fcountlist, nonfcountlist) %>%
  pivot_longer(cols = c('fcountlist','nonfcountlist'), names_to = 'aoi', values_to = 'count') %>%
  droplevels()

count2aov2 <- anova_test(data = count2, wid = ids, dv = count, within = aoi, between = group, effect.size = 'pes')

fixationGroupEffectPlot <- count2 %>% ggplot(aes(x = aoi, y = count)) +
  geom_boxplot(aes(fill = group)) + 
  labs(y='Number of Fixations', x = 'Areas of Interest (AOI)', 
       title = 'Attention Maintenance', 
       fill = 'Group') +
  scale_x_discrete(labels=c('Face', 'Non Face')) +
  scale_y_continuous(breaks = 0:6) +
  scale_fill_manual(values=c('white', 'grey30'), labels=c('Bilingual', 'Monolingual')) +
  theme(plot.title = element_text(hjust = 0.5, family = 'LM Roman 10', face = 'bold', size = 14, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10, b = 10)),
        axis.text.x = element_text(family = 'LM Roman 10', size = 12, margin = margin(t = 10, b = 10)),
        axis.text.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(l = 10, r = 10)),
        axis.title.y = element_text(family = 'LM Roman 10', size = 12, margin = margin(l = 10, r = 10)),
        legend.title = element_text(family = 'LM Roman 10', face = 'bold', size = 12),
        legend.text = element_text(family = 'LM Roman 10', size = 12),
        legend.position = 'right'); fixationGroupEffectPlot

facepopoutresults <- cowplot::plot_grid(latencyplot, lat5plot, fixationGroupEffectPlot, labels = "AUTO",
                                        label_fontfamily = 'LM Roman 10', label_fontface = "bold"); facepopoutresults

ggsave('FacePopOutResults.jpeg', 
       path = "/Users/victoriamousley/BASIS/github version/BASIS-ET-analysis/", width = 12, height = 10, device='tiff', dpi=300)

# TOSTtwo(m1, m2, sd1, sd2, n1, n2, low_eqbound_d, high_eqbound_d, alpha, var.equal, plot = TRUE)
TOSTtwo(2.99, 3.11, 0.84, 0.86, 32, 26, -0.5, 0.5, alpha = 0.05, var.equal = FALSE, plot = TRUE)
