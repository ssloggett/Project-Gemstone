rm(list=ls())
setwd('~/documents/project-gemstone/acceptability judgments')
library(dplyr)
library(ggplot2)
library(gtable)

###############################################################################################
###################################### Column Definitions #####################################
###############################################################################################
cols = c("SID","MD5","trialType","number","element","cond","item","sentence","response","correct", 'rt')

###############################################################################################
############################## Zscore function for trial rejection ############################
###############################################################################################
zscoreRT = function(data){
  require(dplyr)
  data$zRT = 0
  subj.rt = ddply(data, .(subj), summarize, mean = mean(rt))
  for(i in unique(data$subj)){
    subj.sd = sd(subset(data, subj == i)$rt)
    data$zRT[data$subj == i] = abs((data$rt[data$subj == i] - subj.rt$mean[subj.rt$subj == i])/subj.sd)
  }
  return(data)
}

###############################################################################################
################################### Experiment 1 (Garnet) ####################################
###############################################################################################
#######################
#### Factor Coding ####
#######################
garnet.factors = data.frame(cond = 1:8, 
                            lure = rep(c('+', '-'), 4), 
                            target = rep(c('+','-'), each=2, times=2), 
                            verb = rep(c('speech','perception'),each=4))
garnet.factors$lure = factor(garnet.factors$lure, levels = c('+', '-'))
garnet.factors$target = factor(garnet.factors$target, levels = c('+', '-'))
garnet.factors$verb = factor(garnet.factors$verb, levels = c('speech', 'perception'))

######################
#### Read in data ####
######################
garnet.alldata = read.csv('garnet_acceptabilityData.txt', header = 0, sep = ',', comment.char = '#', col.names = cols)

#########################################
#### Extract demographic information ####
#########################################
garnet.bio = droplevels(subset(garnet.alldata,cond%in%c("consent", "intro", "debrief", "exit"), select = c("MD5","sentence","response")))
#subset(garnet.bio, SID == 1441384256)
garnet.bio = cast(garnet.bio, MD5~sentence, value = 'response')
garnet.shortBio = subset(garnet.bio, select = c("MD5","age","state","natlang","otherlang","worker_id"))
garnet.shortBio$subj = c(1:nrow(garnet.shortBio))
garnet.shortBio$natlang = ifelse(garnet.shortBio$natlang == 'English' | 
                                   garnet.shortBio$natlang == 'english' |
                                   garnet.shortBio$natlang == 'American English', "English", 'Other')

###########################
#### Subject Rejection ####
###### Reject subjects who:
###########################
garnet.exclude = garnet.shortBio$subj[duplicated(garnet.shortBio$worker_id) | # participated more than once
                                        as.numeric(as.character(garnet.shortBio$age)) > 55 | # are older than 55
                                        garnet.shortBio$natlang != "English" | # are non-native speakers
                                        garnet.shortBio$otherlang %in%c('Chinese', 'Korean')] # were exposed to logophors

########################
#### Label subjects ####
########################
garnet.subjs = data.frame('subj' = garnet.shortBio$subj, 'MD5'=garnet.shortBio$MD5)
garnet.alldata = merge(garnet.alldata, garnet.subjs, 'MD5')

#############################
#### Extract rating data ####
#############################
garnet.data = droplevels(subset(garnet.alldata, trialType == 'AcceptabilityJudgment' & 
                                  !is.na(rt) & !cond == 'practice' & 
                                  rt > 0 & response != 'NULL' & 
                                  substr(cond,0,6) == 'garnet', 
                                select = c('cond','item','response','subj','rt')))

#############################
#### Re-format variables ####
#############################
garnet.data$response = as.numeric(as.character(garnet.data$response))
garnet.data$cond = substr(garnet.data$cond, 8,8)
garnet.data$item = as.numeric(as.character(garnet.data$item))

######################################################
#### Exclude subjects and Check counter-balancing ####
######################################################
garnet.data = droplevels(subset(garnet.data, !subj%in%garnet.exclude))
table(garnet.data$item, garnet.data$cond)[1:8,]

###########################################
#### Exclude trials with over-long RTs ####
###########################################
garnet.data = zscoreRT(garnet.data); nrow(garnet.data) #3264
garnet.data = subset(garnet.data, zRT <=3); nrow(garnet.data) #3192
# Results in 2.2% data exclusion

####################################
#### Calculate by-subject means ####
####################################
garnet.n = length(unique(garnet.data$subj))
garnet.subjs = ddply(garnet.data, .(subj, cond), summarize, resp = mean(response))
garnet.df = ddply(garnet.subjs, .(cond), summarize, sd = sd(resp), mean = mean(resp))
garnet.df$se = garnet.df$sd/sqrt(garnet.n)
garnet.df = merge(garnet.df, garnet.factors, 'cond')
garnet.df$data = paste(round(garnet.df$mean, 2), ' (', round(garnet.df$se, 2), ')', sep = '')
cast(garnet.df, target+verb ~ lure, value = 'data')

###############################################################################################
################################### Experiment 2 (Amethyst) ###################################
###############################################################################################
#######################
#### Factor Coding ####
#######################
amethyst.factors = data.frame(cond = 1:6, 
                              lure = rep(c('+', '-'), 3), 
                              target = rep(c('Name','Speech','Perception'), each = 2))
amethyst.factors$lure = factor(amethyst.factors$lure, levels = c('+', '-'))
amethyst.factors$target = factor(amethyst.factors$target, levels = c('Name', 'Speech', 'Perception'))

######################
#### Read in data ####
######################
amethyst.alldata = read.csv('amethyst_acceptabilityData.txt', header = 0, sep = ',', comment.char = '#', col.names = cols)

#########################################
#### Extract demographic information ####
#########################################
amethyst.bio = droplevels(subset(amethyst.alldata,cond%in%c("consent", "intro", "debrief", "exit"), select = c("SID","sentence","response")))
amethyst.bio = cast(amethyst.bio, SID~sentence, value = 'response')
amethyst.shortBio = subset(amethyst.bio, select = c("SID","age","state","nLang","logLang","priorRefl","worker_id"))
amethyst.shortBio$subj = c(1:nrow(amethyst.shortBio))


###########################
#### Subject Rejection ####
###### Reject subjects who:
###########################
amethyst.exclude = amethyst.shortBio$subj[duplicated(amethyst.shortBio$worker_id) | # participated more than once
                                            as.numeric(as.character(amethyst.shortBio$age)) > 55 | # are older than 55
                                            amethyst.shortBio$nLang != "English" | # are non-native speakers
                                            amethyst.shortBio$logLang == "yes" | # were exposed to logophors
                                            amethyst.shortBio$priorRefl == "yes"] # have participated in a reflexives experiment

########################
#### Label subjects ####
########################
amethyst.subjs = data.frame(cbind('subj' = amethyst.shortBio$subj, 'SID'=amethyst.shortBio$SID))
amethyst.alldata = merge(amethyst.alldata, amethyst.subjs, 'SID')

#############################
#### Extract rating data ####
#############################
amethyst.data = droplevels(subset(amethyst.alldata, trialType == 'AcceptabilityJudgment' & 
                                    !is.na(rt) & !cond == 'practice' & 
                                    rt > 0 & response != 'NULL' & 
                                    substr(cond,0,8) == 'amethyst', 
                                  select = c('cond','item','response','subj','rt')))

#############################
#### Re-format variables ####
#############################
amethyst.data$response = as.numeric(as.character(amethyst.data$response))
amethyst.data$cond = substr(amethyst.data$cond, 10,10)
amethyst.data$item = as.numeric(as.character(amethyst.data$item))

######################################################
#### Exclude subjects and Check counter-balancing ####
######################################################
amethyst.data = droplevels(subset(amethyst.data, !subj%in%amethyst.exclude))
table(amethyst.data$item, amethyst.data$cond)[1:6,]

###########################################
#### Exclude trials with over-long RTs ####
###########################################
amethyst.data = zscoreRT(amethyst.data); nrow(amethyst.data) #1608
amethyst.data = subset(amethyst.data, zRT <=3); nrow(amethyst.data) #1865
# Results in 2.3% data exclusion

####################################
#### Calculate by-subject means ####
####################################
amethyst.n = length(unique(amethyst.data$subj))
amethyst.subjs = ddply(amethyst.data, .(subj, cond), summarize, resp = mean(response))
amethyst.df = ddply(amethyst.subjs, .(cond), summarize, sd = sd(resp), mean = mean(resp))
amethyst.df$se = amethyst.df$sd/sqrt(amethyst.n)
amethyst.df = merge(amethyst.df, amethyst.factors, 'cond')
amethyst.df$data = paste(round(amethyst.df$mean, 2), ' (', round(amethyst.df$se, 2), ')', sep = '')
cast(amethyst.df, target ~ lure, value = 'data')

###############################################################################################
################################### Experiment 3 (Almandine) ##################################
###############################################################################################
almandine.factors = data.frame(cond = 1:6, 
                               lure = rep(c('+', '-'), 3), 
                               target = rep(c('Name','It','I'), each = 2))
almandine.factors$lure = factor(almandine.factors$lure, levels = c('+', '-'))
almandine.factors$target = factor(almandine.factors$target, levels = c('Name', 'It', 'I'))

######################
#### Read in data ####
######################
almandine.alldata = read.csv('almandine_acceptabilityData.txt', header = 0, sep = ',', comment.char = '#', col.names = cols)

#########################################
#### Extract demographic information ####
#########################################
almandine.bio = droplevels(subset(almandine.alldata,cond%in%c("consent", "intro", "debrief", "exit"), select = c("SID","sentence","response")))
almandine.bio = cast(almandine.bio, SID~sentence, value = 'response')
almandine.shortBio = subset(almandine.bio, select = c("SID","age","state","nLang","logLang","priorRefl","worker_id"))
almandine.shortBio$subj = c(1:nrow(almandine.shortBio))


###########################
#### Subject Rejection ####
###### Reject subjects who:
###########################
almandine.exclude = almandine.shortBio$subj[duplicated(almandine.shortBio$worker_id) | # participated more than once
                                              as.numeric(as.character(almandine.shortBio$age)) > 55 | # are older than 55
                                              almandine.shortBio$nLang != "English" | # are non-native speakers
                                              almandine.shortBio$logLang == "yes" | # were exposed to logophors
                                              almandine.shortBio$priorRefl == "yes"] # have participated in a reflexives experiment

########################
#### Label subjects ####
########################
almandine.subjs = data.frame(cbind('subj' = almandine.shortBio$subj, 'SID'=almandine.shortBio$SID))
almandine.alldata = merge(almandine.alldata, almandine.subjs, 'SID')

#############################
#### Extract rating data ####
#############################
almandine.data = droplevels(subset(almandine.alldata, trialType == 'AcceptabilityJudgment' & 
                                     !is.na(rt) & !cond == 'practice' & 
                                     rt > 0 & response != 'NULL' & 
                                     substr(cond,0,9) == 'almandine', 
                                   select = c('cond','item','response','subj','rt')))

#############################
#### Re-format variables ####
#############################
almandine.data$response = as.numeric(as.character(almandine.data$response))
almandine.data$cond = substr(almandine.data$cond, 11,11)
almandine.data$item = as.numeric(as.character(almandine.data$item))

######################################################
#### Exclude subjects and Check counter-balancing ####
######################################################
almandine.data = droplevels(subset(almandine.data, !subj%in%almandine.exclude))
table(almandine.data$item, almandine.data$cond)[1:6,]

###########################################
#### Exclude trials with over-long RTs ####
###########################################
almandine.data = zscoreRT(almandine.data); nrow(almandine.data) #1620
almandine.data = subset(almandine.data, zRT <=3); nrow(almandine.data) #1579
# Results in 2.5% data exclusion

####################################
#### Calculate by-subject means ####
####################################
almandine.n = length(unique(almandine.data$subj))
almandine.subjs = ddply(almandine.data, .(subj, cond), summarize, resp = mean(response))
almandine.df = ddply(almandine.subjs, .(cond), summarize, sd = sd(resp), mean = mean(resp))
almandine.df$se = almandine.df$sd/sqrt(almandine.n)
almandine.df = merge(almandine.df, almandine.factors, 'cond')
almandine.df$data = paste(round(almandine.df$mean, 2), ' (', round(almandine.df$se, 2), ')', sep = '')
cast(almandine.df, target ~ lure, value = 'data')

###############################################################################################
################################### Experiment 3 (Almandine2p) ####################################
###############################################################################################
almandine2p.factors = data.frame(cond = 1:6, lure = rep(c('+', '-'), 3), target = rep(c('Name','They','you'), each = 2))
almandine2p.factors$lure = factor(almandine2p.factors$lure, levels = c('+', '-'))
almandine2p.factors$target = factor(almandine2p.factors$target, levels = c('Name', 'They', 'you'))

######################
#### Read in data ####
######################
almandine2p.alldata = read.csv('almandine2p_acceptabilityData.txt', header = 0, sep = ',', comment.char = '#', col.names = cols)

#########################################
#### Extract demographic information ####
#########################################
almandine2p.bio = droplevels(subset(almandine2p.alldata,cond%in%c("consent", "intro", "debrief", "exit"), select = c("SID","sentence","response")))
almandine2p.bio = cast(almandine2p.bio, SID~sentence, value = 'response')
almandine2p.shortBio = subset(almandine2p.bio, select = c("SID","age","state","nLang","logLang","priorRefl","worker_id"))
almandine2p.shortBio$subj = c(1:nrow(almandine2p.shortBio))


###########################
#### Subject Rejection ####
###### Reject subjects who:
###########################
almandine2p.exclude = almandine2p.shortBio$subj[duplicated(almandine2p.shortBio$worker_id) | # participated more than once
                                                  as.numeric(as.character(almandine2p.shortBio$age)) > 55 | # are older than 55
                                                  almandine2p.shortBio$nLang != "English" | # are non-native speakers
                                                  almandine2p.shortBio$logLang == "yes" | # were exposed to logophors
                                                  almandine2p.shortBio$priorRefl == "yes"] # have participated in a reflexives experiment

########################
#### Label subjects ####
########################
almandine2p.subjs = data.frame(cbind('subj' = almandine2p.shortBio$subj, 'SID'=almandine2p.shortBio$SID))
almandine2p.alldata = merge(almandine2p.alldata, almandine2p.subjs, 'SID')

#############################
#### Extract rating data ####
#############################
almandine2p.data = droplevels(subset(almandine2p.alldata, trialType == 'AcceptabilityJudgment' & 
                                       !is.na(rt) & !cond == 'practice' & 
                                       rt > 0 & response != 'NULL' & 
                                       substr(cond,0,9) == 'almandine', 
                                     select = c('cond','item','response','subj','rt')))

#############################
#### Re-format variables ####
#############################
almandine2p.data$response = as.numeric(as.character(almandine2p.data$response))
almandine2p.data$cond = substr(almandine2p.data$cond, 11,11)
almandine2p.data$item = as.numeric(as.character(almandine2p.data$item))

######################################################
#### Exclude subjects and Check counter-balancing ####
######################################################
almandine2p.data = droplevels(subset(almandine2p.data, !subj%in%almandine2p.exclude))
table(almandine2p.data$item, almandine2p.data$cond)[1:6,]

###########################################
#### Exclude trials with over-long RTs ####
###########################################
almandine2p.data = zscoreRT(almandine2p.data); nrow(almandine2p.data) #1800
almandine2p.data = subset(almandine2p.data, zRT <=3); nrow(almandine2p.data) #1755
# Results in 2.5% data exclusion

####################################
#### Calculate by-subject means ####
####################################
almandine2p.n = length(unique(almandine2p.data$subj))
almandine2p.subjs = ddply(almandine2p.data, .(subj, cond), summarize, resp = mean(response))
almandine2p.df = ddply(almandine2p.subjs, .(cond), summarize, sd = sd(resp), mean = mean(resp))
almandine2p.df$se = almandine2p.df$sd/sqrt(almandine2p.n)
almandine2p.df = merge(almandine2p.df, almandine2p.factors, 'cond')
almandine2p.df$data = paste(round(almandine2p.df$mean, 2), ' (', round(almandine2p.df$se, 2), ')', sep = '')
cast(almandine2p.df, target ~ lure, value = 'data')

###############################################################################################
#################################### Experiment 4 (Pearl) #####################################
###############################################################################################
pearl.factors = data.frame(cond = 1:6, lure = rep(c('+', '-'), 3), target = rep(c('Name','They','I'), each = 2))
pearl.factors$lure = factor(pearl.factors$lure, levels = c('+', '-'))
pearl.factors$target = factor(pearl.factors$target, levels = c('Name', 'They', 'I'))

######################
#### Read in data ####
######################
pearl.alldata = read.csv('pearl_acceptabilityData.txt', header = 0, sep = ',', comment.char = '#', col.names = cols)

#########################################
#### Extract demographic information ####
#########################################
pearl.bio = droplevels(subset(pearl.alldata,cond%in%c("consent", "intro", "debrief", "exit"), select = c("SID","sentence","response")))
pearl.bio = cast(pearl.bio, SID~sentence, value = 'response')
pearl.shortBio = subset(pearl.bio, select = c("SID","age","state","nLang","logLang","priorRefl","worker_id"))
pearl.shortBio$subj = c(1:nrow(pearl.shortBio))


###########################
#### Subject Rejection ####
###### Reject subjects who:
###########################
pearl.exclude = pearl.shortBio$subj[duplicated(pearl.shortBio$worker_id) | # participated more than once
                                      as.numeric(as.character(pearl.shortBio$age)) > 55 | # are older than 55
                                      pearl.shortBio$nLang != "English" | # are non-native speakers
                                      pearl.shortBio$logLang == "yes" | # were exposed to logophors
                                      pearl.shortBio$priorRefl == "yes"] # have participated in a reflexives experiment

########################
#### Label subjects ####
########################
pearl.subjs = data.frame(cbind('subj' = pearl.shortBio$subj, 'SID'=pearl.shortBio$SID))
pearl.alldata = merge(pearl.alldata, pearl.subjs, 'SID')

#############################
#### Extract rating data ####
#############################
pearl.data = droplevels(subset(pearl.alldata, trialType == 'AcceptabilityJudgment' & 
                                 !is.na(rt) & !cond == 'practice' & 
                                 rt > 0 & response != 'NULL' & 
                                 substr(cond,0,5) == 'pearl', 
                               select = c('cond','item','response','subj','rt')))

#############################
#### Re-format variables ####
#############################
pearl.data$response = as.numeric(as.character(pearl.data$response))
pearl.data$cond = substr(pearl.data$cond, 7,7)
pearl.data$item = as.numeric(as.character(pearl.data$item))

######################################################
#### Exclude subjects and Check counter-balancing ####
######################################################
pearl.data = droplevels(subset(pearl.data, !subj%in%pearl.exclude))
table(pearl.data$item, pearl.data$cond)[1:6,]

###########################################
#### Exclude trials with over-long RTs ####
###########################################
pearl.data = zscoreRT(pearl.data); nrow(pearl.data) #1692
pearl.data = subset(pearl.data, zRT <=3); nrow(pearl.data) #1653
# Results in 2.3% data exclusion

####################################
#### Calculate by-subject means ####
####################################
pearl.n = length(unique(pearl.data$subj))
pearl.subjs = ddply(pearl.data, .(subj, cond), summarize, resp = mean(response))
pearl.df = ddply(pearl.subjs, .(cond), summarize, sd = sd(resp), mean = mean(resp))
pearl.df$se = pearl.df$sd/sqrt(pearl.n)
pearl.df = merge(pearl.df, pearl.factors, 'cond')
pearl.df$data = paste(round(pearl.df$mean, 2), ' (', round(pearl.df$se, 2), ')', sep = '')
cast(pearl.df, target ~ lure, value = 'data')
