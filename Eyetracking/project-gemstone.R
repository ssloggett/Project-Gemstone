rm(list=ls())
setwd('~/documents/project-gemstone/eyetracking')
library(dplyr)
library(ggplot2)
library(gtable)

###############################################################################################
################################# ROI and Fixation Definitions ################################
###############################################################################################
regs = c('Start', 'Reflexive', 'Spillover', 'End')
reg.labs = data.frame(region = 1:4, regs = regs)
reg.labs$regs = factor(reg.labs$regs, levels = regs)

fix = c('First Fixation', 'First Pass', 'p(Regression)', 'Go-Past', 'Re-Reading', 'Total Time')
fix.labs = data.frame(fixations = fix, fixationtype = c('ff','fp','pr','rp','rr','tt'))
fix.labs$fixations = factor(fix.labs$fixations, levels = fix)

###############################################################################################
######################################## GGPlot Theme #########################################
###############################################################################################
my.theme = theme(text = element_text(size = 12, colour = 'black', face = 'bold'),
                 axis.text = element_text(size = 12, colour = 'black', face = 'italic'),
                 axis.line = element_line(colour ='black'),
                 legend.text = element_text(size = 8, colour = 'black', face = 'italic'),
                 strip.text = element_text(size = 12, colour = 'black', face = 'bold'),
                 strip.background = element_rect(fill = NA, colour = NA),
                 panel.background = element_rect(fill = NA),
                 panel.grid = element_blank(),
                 panel.spacing.x = unit(.5, 'cm'),
                 panel.border = element_rect(fill = NA, colour = NA),
                 legend.position = 'none',
                 plot.title = element_text(hjust = 0.5))

###############################################################################################
#################################### Experiment 1 (Garnet) ####################################
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
garnet.data = read.table('garnet_data.txt',  header = T)
garnet.parafoveal.data = read.table('garnet_parafoveal_data.txt',  header = T)

#######################
#### Skipping Rate ####
#######################
garnet.skips = subset(garnet.data, fixationtype == 'ff' & is.na(value) & region == 2)
garnet.total = subset(garnet.data, fixationtype == 'ff' & region == 2); nrow(garnet.skips)/nrow(garnet.total)
# Standard: 16%
garnet.parafoveal.skips = subset(garnet.parafoveal.data, fixationtype == 'ff' & is.na(value) & region == 2)
garnet.parafoveal.total = subset(garnet.parafoveal.data, fixationtype == 'ff' & region == 2); nrow(garnet.parafoveal.skips)/nrow(garnet.parafoveal.total)
# Parafoveal: 6%

###########################
#### Subject Rejection ####
###########################
# Reject subjects with >11 missing trials; <75% question accuracy
garnet.badSubjs = c(17,19) #Same for parafoveal and standard regioning
garnet = droplevels(subset(garnet.data, !(fixationtype == 'rr' & value == 0) &
                             !(fixationtype == 'tt' & value > 4000) & 
                             !(fixationtype == 'fp' & value > 2000) &
                             !is.na(value) & !subj%in%garnet.badSubjs))

#######################
#### Factor Coding ####
#######################
garnet = merge(garnet, garnet.factors, 'cond')

## Set Contrasts
contrasts(garnet$lure) = c(-.5, .5)
contrasts(garnet$verb) = c(-.5, .5)
contrasts(garnet$target) = c(-.5,.5)

## Create numeric factors to decorrelte random slopes/intercepts
garnet$l = ifelse(garnet$lure == '+', -.5, .5)
garnet$v = ifelse(garnet$verb == 'speech', -.5, .5)
garnet$t = ifelse(garnet$target == '+', -.5, .5)

############################
#### Data Visualization ####
############################
garnet.n = length(unique(garnet$subj))
garnet.subjs = ddply(garnet, .(subj, region, fixationtype, cond), summarise, rt = mean(value, na.rm = 1))
garnet.df = ddply(garnet.subjs, .(region, fixationtype, cond), summarise, sd = sd(rt, na.rm = 1), rt = mean(rt, na.rm = 1))
garnet.df$se = garnet.df$sd/sqrt(garnet.n)

garnet.df = merge(garnet.df, fix.labs, 'fixationtype')
garnet.df = merge(garnet.df, reg.labs, 'region')
garnet.df = merge(garnet.df, garnet.factors, 'cond')

# Create a condition label to accomodate error bar distribution
garnet.df$condLab = paste(garnet.df$lure, garnet.df$target, sep= '')
garnet.df$condLab = factor(garnet.df$condLab, levels = c('++', '-+', '+-', '--'))

garnet.plot = ggplotGrob(ggplot(data = droplevels(subset(garnet.df, region%in%c(2,3) & fixationtype%in%c('rp', 'tt'))),
                                aes(x = verb, y = rt, fill = condLab, alpha = condLab)) + 
                           geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
                           geom_bar(stat = 'identity', position = position_dodge(width = 0.9), colour = 'black') +
                           geom_errorbar(position = position_dodge(width = 0.9), aes(ymax = rt+se, ymin = rt-se),
                                         width = .05, colour = 'black', alpha = 1) +
                           scale_fill_manual(values = rep(c('#212721', '#85274e'), each = 2)) +
                           scale_alpha_manual(values = rep(c(.35, .95), 2)) +
                           facet_grid(fixations~regs, scales = 'free') +
                           labs(x = '', y = 'Time (ms)') +
                           my.theme)

garnet.legend = ggplotGrob(ggplot(unique(subset(garnet.df, region%in%c(3) & fixationtype%in%c('rp'), select = c('lure', 'target'))),
                                  aes(lure, target, colour=target, alpha=lure)) +
                             geom_point(size = 10) +
                             coord_equal(ratio = 1.5/1) +
                             scale_colour_manual(values = c('#212721', '#85274e')) +
                             scale_alpha_manual(values = c(.35, .95)) +
                             scale_x_discrete(position = "top") +
                             scale_y_discrete(limits = rev(levels(garnet.df$target))) +
                             labs(x = 'Lure Match', y = 'Target Match') +
                             theme_minimal() + 
                             my.theme + 
                             theme(axis.ticks = element_blank(),
                                axis.text = element_text(size = 14, colour = 'black', face = 'bold')))

grid.newpage()
pushViewport(vp = viewport(width = 0.7, x = 0.65))
grid.draw(garnet.plot)
popViewport()
pushViewport(vp = viewport(height = .5, x = 0.15))
grid.draw(garnet.legend)
popViewport()

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
amethyst.data = read.table('amethyst_data.txt',  header = T)
amethyst.parafoveal.data = read.table('amethyst_parafoveal_data.txt',  header = T)

#######################
#### Skipping Rate ####
#######################
amethyst.skips = subset(amethyst.data, fixationtype == 'ff' & is.na(value) & region == 2)
amethyst.total = subset(amethyst.data, fixationtype == 'ff' & region == 2); nrow(amethyst.skips)/nrow(amethyst.total)
# Standard: 20%
amethyst.parafoveal.skips = subset(amethyst.parafoveal.data, fixationtype == 'ff' & is.na(value) & region == 2)
amethyst.parafoveal.total = subset(amethyst.parafoveal.data, fixationtype == 'ff' & region == 2); nrow(amethyst.parafoveal.skips)/nrow(amethyst.parafoveal.total)
# Parafoveal: 10%

###########################
#### Subject Rejection ####
###########################
# Reject subjects with >8 missing trials; <75% question accuracy
amethyst.badSubjs = c('017','031','034', '014') #Standard regioning
amethyst.badSubjs = c('006','017','031','034', '037', '014') #Parafoveal regioning
amethyst = droplevels(subset(amethyst.parafoveal.data, !(fixationtype == 'rr' & value == 0) &
                               !(fixationtype == 'tt' & value > 4000) & 
                               !(fixationtype == 'fp' & value > 2000) &
                               !is.na(value) & !subj%in%amethyst.badSubjs))

###########################
#### Subject Rejection ####
###########################
amethyst = merge(amethyst, amethyst.factors, 'cond')

## Set Contrasts
contrasts(amethyst$lure) = c(-1, 1)
contrasts(amethyst$target) = matrix(c(-1,.5,.5,0,-1, 1), nrow=3)
colnames(contrasts(amethyst$target)) = c('gram', 'verb')

## Create numeric factors to decorrelte random slopes/intercepts
amethyst$l = ifelse(amethyst$lure == '+', -.5, .5)
amethyst$t = ifelse(amethyst$target == 'Name', -1, .5)
amethyst$v = ifelse(amethyst$target == 'Name', 0, ifelse(amethyst$target == 'Speech', -1, 1))

############################
#### Data Visualization ####
############################
amethyst.n = length(unique(amethyst$subj))
amethyst.subjs = ddply(amethyst, .(subj, region, fixationtype, cond), summarise, rt = mean(value, na.rm = 1))
amethyst.df = ddply(amethyst.subjs, .(region, fixationtype, cond), summarise, sd = sd(rt, na.rm = 1), rt = mean(rt, na.rm = 1))
amethyst.df$se = amethyst.df$sd/sqrt(amethyst.n)

amethyst.df = merge(amethyst.df, fix.labs, 'fixationtype')
amethyst.df = merge(amethyst.df, reg.labs, 'region')
amethyst.df = merge(amethyst.df, amethyst.factors, 'cond')

# Create a condition label to accomodate error bar distribution
amethyst.df$condLab = paste(amethyst.df$lure, amethyst.df$target, sep= '')
amethyst.df$condLab = factor(amethyst.df$condLab, levels = c('+Name', '-Name', '+Speech', '-Speech', '+Perception', '-Perception'))

amethyst.plot = ggplotGrob(ggplot(data = droplevels(subset(amethyst.df, region%in%c(2,3) & fixationtype%in%c('rp', 'tt'))),
                                  aes(x = target, y = rt, fill = condLab, alpha = condLab)) + 
                             geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
                             geom_bar(stat = 'identity', position = position_dodge(width = 0.9), colour = 'black') +
                             geom_errorbar(position = position_dodge(width = 0.9), aes(ymax = rt+se, ymin = rt-se),
                                           width = .05, colour = 'black', alpha = 1) +
                             scale_fill_manual(values = rep(c('#b4b5df','#212721', '#85274e'), each = 2)) +
                             scale_alpha_manual(values = rep(c(.35, .95), 3)) +
                             facet_grid(fixations~regs, scales = 'free') +
                             labs(x = '', y = 'Time (ms)') +
                             my.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()))

amethyst.legend = ggplotGrob(ggplot(unique(subset(amethyst.df, region%in%c(3) & fixationtype%in%c('rp'), select = c('lure', 'target'))),
                                    aes(lure, target, colour=target, alpha=lure)) +
                               geom_point(size = 10) +
                               coord_equal(ratio = 1.5/1) +
                               scale_colour_manual(values = c('#b4b5df','#212721', '#85274e')) +
                               scale_alpha_manual(values = c(.35, .95)) +
                               scale_x_discrete(position = "top") +
                               scale_y_discrete(limits = rev(levels(amethyst.df$target))) +
                               labs(x = 'Lure Match', y = 'Target Match') +
                               theme_minimal() + 
                               my.theme + 
                               theme(axis.ticks = element_blank(),
                                     axis.title.y=element_blank(),
                                     axis.text.y = element_text(angle = 0, hjust = 1, face = 'italic'),
                                     axis.text.x = element_text(size = 14, hjust = 0.5, vjust=0, face = 'plain')))

grid.newpage()
pushViewport(vp = viewport(width = 0.7, x = 0.65))
grid.draw(amethyst.plot)
popViewport()
pushViewport(vp = viewport(height = .5, x = 0.15))
grid.draw(amethyst.legend)
popViewport()

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
almandine.data = read.table('almandine_data.txt',  header = T)
almandine.parafoveal.data = read.table('almandine_parafoveal_data.txt',  header = T)

#######################
#### Skipping Rate ####
#######################
almandine.skips = subset(almandine.data, fixationtype == 'ff' & is.na(value) & region == 2)
almandine.total = subset(almandine.data, fixationtype == 'ff' & region == 2); nrow(almandine.skips)/nrow(almandine.total)
# Standard: 21%
almandine.parafoveal.skips = subset(almandine.parafoveal.data, fixationtype == 'ff' & is.na(value) & region == 2)
almandine.parafoveal.total = subset(almandine.parafoveal.data, fixationtype == 'ff' & region == 2); nrow(almandine.parafoveal.skips)/nrow(almandine.parafoveal.total)
# Parafoveal: 10%

###########################
#### Subject Rejection ####
###########################
# Reject subjects with >8 missing trials; <75% question accuracy
#almandine.badSubjs = c(6,17) #Standard regioning
almandine.badSubjs = c(6,16,17,24) #Same for parafoveal and standard regioning
almandine = droplevels(subset(almandine.parafoveal.data, !(fixationtype == 'rr' & value == 0) &
                                !(fixationtype == 'tt' & value > 4000) & 
                                !(fixationtype == 'fp' & value > 2000) &
                                !is.na(value) & !subj%in%almandine.badSubjs))

###########################
#### Subject Rejection ####
###########################
almandine = merge(almandine, almandine.factors, 'cond')

## Set Contrasts
contrasts(almandine$lure) = c(-1, 1)
contrasts(almandine$target) = matrix(c(-1,.5,.5,0,-1, 1), nrow=3)
colnames(contrasts(almandine$target)) = c('gram', 'person')


## Create numeric factors to decorrelte random slopes/intercepts
almandine$l = ifelse(almandine$lure == '+', -.5, .5)
almandine$t = ifelse(almandine$target == 'Name', -1, .5)
almandine$p = ifelse(almandine$target == 'Name', 0, ifelse(almandine$target == 'It', -1, 1))

############################
#### Data Visualization ####
############################
almandine.n = length(unique(almandine$subj))
almandine.subjs = ddply(almandine, .(subj, region, fixationtype, cond), summarise, rt = mean(value, na.rm = 1))
almandine.df = ddply(almandine.subjs, .(region, fixationtype, cond), summarise, sd = sd(rt, na.rm = 1), rt = mean(rt, na.rm = 1))
almandine.df$se = almandine.df$sd/sqrt(almandine.n)

almandine.df = merge(almandine.df, fix.labs, 'fixationtype')
almandine.df = merge(almandine.df, reg.labs, 'region')
almandine.df = merge(almandine.df, almandine.factors, 'cond')

# Create a condition label to accomodate error bar distribution
almandine.df$condLab = paste(almandine.df$lure, almandine.df$target, sep= '')
almandine.df$condLab = factor(almandine.df$condLab, levels = c('+Name', '-Name', '+It', '-It', '+I', '-I'))

almandine.plot = ggplotGrob(ggplot(data = droplevels(subset(almandine.df, region%in%c(2,3) & fixationtype%in%c('rp', 'tt'))),
                                   aes(x = target, y = rt, fill = condLab, alpha = condLab)) + 
                              geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
                              geom_bar(stat = 'identity', position = position_dodge(width = 0.9), colour = 'black') +
                              geom_errorbar(position = position_dodge(width = 0.9), aes(ymax = rt+se, ymin = rt-se),
                                            width = .05, colour = 'black', alpha = 1) +
                              scale_fill_manual(values = rep(c('#b4b5df','#212721', '#85274e'), each = 2)) +
                              scale_alpha_manual(values = rep(c(.35, .95), 3)) +
                              facet_grid(fixations~regs, scales = 'free') +
                              labs(x = '', y = 'Time (ms)') +
                              my.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()))

almandine.legend = ggplotGrob(ggplot(unique(subset(almandine.df, region%in%c(3) & fixationtype%in%c('rp'), select = c('lure', 'target'))),
                                     aes(lure, target, colour=target, alpha=lure)) +
                                geom_point(size = 10) +
                                coord_equal(ratio = 1.5/1) +
                                scale_colour_manual(values = c('#b4b5df','#212721', '#85274e')) +
                                scale_alpha_manual(values = c(.35, .95)) +
                                scale_x_discrete(position = "top") +
                                scale_y_discrete(limits = rev(levels(almandine.df$target))) +
                                labs(x = 'Lure Match', y = 'Target Match') +
                                theme_minimal() + 
                                my.theme + 
                                theme(axis.ticks = element_blank(),
                                      axis.title.y=element_blank(),
                                      axis.text.y = element_text(angle = 0, hjust = 1, face = 'italic'),
                                      axis.text.x = element_text(size = 14, hjust = 0.5, vjust=0, face = 'plain')))

grid.newpage()
pushViewport(vp = viewport(width = 0.7, x = 0.65))
grid.draw(almandine.plot)
popViewport()
pushViewport(vp = viewport(height = .5, x = 0.15))
grid.draw(almandine.legend)
popViewport()

almandine$regionLength = almandine$Xend-almandine$Xstart
region.check = unique(subset(almandine, region == 3, select = c('item','regionLength')))

###############################################################################################
#################################### Experiment 4 (Pearl) #####################################
###############################################################################################
pearl.factors = data.frame(cond = 1:6, lure = rep(c('+', '-'), 3), target = rep(c('Name','They','I'), each = 2))
pearl.factors$lure = factor(pearl.factors$lure, levels = c('+', '-'))
pearl.factors$target = factor(pearl.factors$target, levels = c('Name', 'They', 'I'))

######################
#### Read in data ####
######################
pearl.data = read.table('pearl_data.txt',  header = T)
pearl.parafoveal.data = read.table('pearl_parafoveal_data.txt',  header = T)

#######################
#### Skipping Rate ####
#######################
pearl.skips = subset(pearl.data, fixationtype == 'ff' & is.na(value) & region == 2)
pearl.total = subset(pearl.data, fixationtype == 'ff' & region == 2); nrow(pearl.skips)/nrow(pearl.total)
# Standard: 19%
pearl.parafoveal.skips = subset(pearl.parafoveal.data, fixationtype == 'ff' & is.na(value) & region == 2)
pearl.parafoveal.total = subset(pearl.parafoveal.data, fixationtype == 'ff' & region == 2); nrow(pearl.parafoveal.skips)/nrow(pearl.parafoveal.total)
# Parafoveal: 10%

###########################
#### Subject Rejection ####
###########################
# Reject subjects with >8 missing trials; <75% question accuracy
pearl.badSubjs = c(9,11,38) #Standard regioning
pearl.badSubjs = c(9,11,35,38) #Parafoveal regioning
pearl = droplevels(subset(pearl.parafoveal.data, !(fixationtype == 'rr' & value == 0) &
                            !(fixationtype == 'tt' & value > 4000) & 
                            !(fixationtype == 'fp' & value > 2000) &
                            !is.na(value) & !subj%in%pearl.badSubjs))

###########################
#### Subject Rejection ####
###########################
pearl = merge(pearl, pearl.factors, 'cond')

## Set Contrasts
contrasts(pearl$lure) = c(-1, 1)
contrasts(pearl$target) = matrix(c(-1,.5,.5,0,-1, 1), nrow=3)
colnames(contrasts(pearl$target)) = c('gram', 'person')

## Create numeric factors to decorrelte random slopes/intercepts
pearl$l = ifelse(pearl$lure == '+', -.5, .5)
pearl$t = ifelse(pearl$target == 'Name', -1, .5)
pearl$p = ifelse(pearl$target == 'Name', 0, ifelse(pearl$target == 'They', -1, 1))

############################
#### Data Visualization ####
############################
pearl.n = length(unique(pearl$subj))
pearl.subjs = ddply(pearl, .(subj, region, fixationtype, cond), summarise, rt = mean(value, na.rm = 1))
pearl.df = ddply(pearl.subjs, .(region, fixationtype, cond), summarise, sd = sd(rt, na.rm = 1), rt = mean(rt, na.rm = 1))
pearl.df$se = pearl.df$sd/sqrt(pearl.n)

pearl.df = merge(pearl.df, fix.labs, 'fixationtype')
pearl.df = merge(pearl.df, reg.labs, 'region')
pearl.df = merge(pearl.df, pearl.factors, 'cond')

# Create a condition label to accomodate error bar distribution
pearl.df$condLab = paste(pearl.df$lure, pearl.df$target, sep= '')
pearl.df$condLab = factor(pearl.df$condLab, levels = c('+Name', '-Name', '+They', '-They', '+I', '-I'))

pearl.plot = ggplotGrob(ggplot(data = droplevels(subset(pearl.df, region%in%c(2,3) & fixationtype%in%c('rp', 'tt'))),
                               aes(x = target, y = rt, fill = condLab, alpha = condLab)) + 
                          geom_bar(stat = 'identity', position = position_dodge(width = 0.9)) + 
                          geom_bar(stat = 'identity', position = position_dodge(width = 0.9), colour = 'black') +
                          geom_errorbar(position = position_dodge(width = 0.9), aes(ymax = rt+se, ymin = rt-se),
                                        width = .05, colour = 'black', alpha = 1) +
                          scale_fill_manual(values = rep(c('#b4b5df','#212721', '#85274e'), each = 2)) +
                          scale_alpha_manual(values = rep(c(.35, .95), 3)) +
                          facet_grid(fixations~regs, scales = 'free') +
                          labs(x = '', y = 'Time (ms)') +
                          my.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()))

pearl.legend = ggplotGrob(ggplot(unique(subset(pearl.df, region%in%c(3) & fixationtype%in%c('rp'), select = c('lure', 'target'))),
                                 aes(lure, target, colour=target, alpha=lure)) +
                            geom_point(size = 10) +
                            coord_equal(ratio = 1.5/1) +
                            scale_colour_manual(values = c('#b4b5df','#212721', '#85274e')) +
                            scale_alpha_manual(values = c(.35, .95)) +
                            scale_x_discrete(position = "top") +
                            scale_y_discrete(limits = rev(levels(pearl.df$target))) +
                            labs(x = 'Lure Match', y = 'Target Match') +
                            theme_minimal() + 
                            my.theme + 
                            theme(axis.ticks = element_blank(),
                                  axis.title.y=element_blank(),
                                  axis.text.y = element_text(angle = 0, hjust = 1, face = 'italic'),
                                  axis.text.x = element_text(size = 14, hjust = 0.5, vjust=0, face = 'plain')))

grid.newpage()
pushViewport(vp = viewport(width = 0.7, x = 0.65))
grid.draw(pearl.plot)
popViewport()
pushViewport(vp = viewport(height = .5, x = 0.15))
grid.draw(pearl.legend)
popViewport()
