# This script creates the figures for the paper, reading data and model estimates from the data and model folder

library(brms);library(tidyverse);library(bayestestR);library(emmeans);library(see);library(ggdist);library(cowplot);library(fishualize)

theme_set(theme_minimal(9))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

same.ILL <- read.csv("data/same_place_ILL_frames.csv")
same.ALE <- read.csv("data/same_place_ALE_frames.csv")
switch.ILL <- read.csv("data/switch_place_ILL_frames.csv")
switch.ILL.within <- read.csv("data/within_subject_switch_place_ILL_frames.csv")
switch.ALE <- read.csv("data/switch_place_ALE_frames.csv") 
switch.ALE.within <- read.csv("data/within_subject_switch_place_ALE_frames.csv") 


table(
      switch.ILL.within$F0_step,
      switch.ILL.within$VOT_step,
      switch.ILL.within$dist_block)/108


brm.same.ILL<-readRDS("models/same.ILL.place.rds")
brm.same.ILL.draws <- as_draws_df(brm.same.ILL)
brm.same.ILL.draws$model<-"same place ILL\nbetween"
brm.same.ILL.draws$design<-"between"
brm.same.ILL.draws$same_switch<-"same"
brm.same.ILL.draws %>% 
  select(`b_F0_step1:dist1`,model,design,same_switch)->brm.same.ILL.draws
colnames(brm.same.ILL.draws)<-c("F0 step:distributional condition interaction","model","design","same_switch")


brm.same.ALE<-readRDS("models/same.ALE.place.rds")
brm.same.ALE.draws <- as_draws_df(brm.same.ALE)
brm.same.ALE.draws$model<-"same place ALE\nbetween"
brm.same.ALE.draws$design<-"between"
brm.same.ALE.draws$same_switch<-"same"
brm.same.ALE.draws %>% 
  select(`b_F0_step1:dist1`,model,design,same_switch)->brm.same.ALE.draws
colnames(brm.same.ALE.draws)<-c("F0 step:distributional condition interaction","model","design","same_switch")

brm.switch.ILL.within<-readRDS("models/within.subj.switch.ILL.place.rds")
brm.switch.ILL.within.draws <- as_draws_df(brm.switch.ILL.within)
brm.switch.ILL.within.draws$model<-"switch place ILL\nwithin"
brm.switch.ILL.within.draws$design<-"within"
brm.switch.ILL.within.draws$same_switch<-"switch"
brm.switch.ILL.within.draws %>% 
  select(`b_F0_step1:dist_block1`,model,design,same_switch)->brm.switch.ILL.within.draws
colnames(brm.switch.ILL.within.draws)<-c("F0 step:distributional condition interaction","model","design","same_switch")

brm.switch.ILL<-readRDS("models/switch.ILL.place.rds")
brm.switch.ILL.draws <- as_draws_df(brm.switch.ILL)
brm.switch.ILL.draws$model<-"switch place ILL\nbetween"
brm.switch.ILL.draws$design<-"between"
brm.switch.ILL.draws$same_switch<-"switch"
brm.switch.ILL.draws %>% 
  select(`b_F0_step1:dist1`,model,design,same_switch)->brm.switch.ILL.draws
colnames(brm.switch.ILL.draws)<-c("F0 step:distributional condition interaction","model","design","same_switch")


brm.switch.ALE<-readRDS("models/switch.ALE.place.rds")
brm.switch.ALE.draws <- as_draws_df(brm.switch.ALE)
brm.switch.ALE.draws$model<-"switch place ALE\nbetween"
brm.switch.ALE.draws$design<-"between"
brm.switch.ALE.draws$same_switch<-"switch"
brm.switch.ALE.draws %>% 
  select(`b_F0_step1:dist1`,model,design,same_switch)->brm.switch.ALE.draws
colnames(brm.switch.ALE.draws)<-c("F0 step:distributional condition interaction","model","design","same_switch")



brm.switch.ALE.within<-readRDS("models/within.subj.switch.ALE.place.rds")
brm.switch.ALE.within.draws <- as_draws_df(brm.switch.ALE.within)
brm.switch.ALE.within.draws$model<-"switch place ALE\nwithin"
brm.switch.ALE.within.draws$design<-"within"
brm.switch.ALE.within.draws$same_switch<-"switch"
brm.switch.ALE.within.draws %>% 
  select(`b_F0_step1:dist_block1`,model,design,same_switch)->brm.switch.ALE.within.draws
colnames(brm.switch.ALE.within.draws)<-c("F0 step:distributional condition interaction","model","design","same_switch")


rbind(brm.same.ILL.draws,
      brm.same.ALE.draws,
      brm.switch.ILL.draws,
      brm.switch.ALE.draws,
      brm.switch.ALE.within.draws,
      brm.switch.ILL.within.draws
      
      ) -> draws.int


draws.int %>% ggplot(aes(x=model,
                         y=`F0 step:distributional condition interaction`,
                         fill=design,
                         alpha=same_switch))+
  geom_hline(yintercept = 0, lty=2, color ="gray65")+
  theme(legend.position = "top")+
  scale_alpha_manual(values = c(0.6,1))+
  labs(alpha = "same/switch\nPoA")+
  scale_fill_manual(values =c("firebrick4","seagreen"))+
  stat_halfeye() -> interaction.figure



### 


plot_grid(
  same.ILL %>% 
    group_by(id,F0_step,dist,VOT_step) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    ggplot(aes(x=dist, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    theme(legend.position = "left")+
    labs(shape ="F0 step")+
    #stat_summary(aes(group = paste(id,F0_step), 
    #                shape =as.factor(F0_step )),
    #           position = position_dodge(0.3),
    #          geom="point",alpha = 0.25)+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step)),
  
  same.ILL %>% 
    group_by(id,F0_step,dist,VOT_step,place) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    mutate(place = ifelse(place == "BP","/b/-/p/",
                          ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
    ggplot(aes(x=dist,color=place, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step))+
    
    scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
    facet_wrap(~place)+theme(legend.position = "none"),
    labels = c("A","B"),rel_widths = c(1.25,2)) -> same.ILL.plot




plot_grid(
  same.ALE %>% 
    group_by(id,F0_step,dist,VOT_step) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    ggplot(aes(x=dist, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    theme(legend.position = "left")+
    labs(shape ="F0 step")+
    #stat_summary(aes(group = paste(id,F0_step), 
    #                shape =as.factor(F0_step )),
    #           position = position_dodge(0.3),
    #          geom="point",alpha = 0.25)+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step)),
  
  same.ALE %>% 
    group_by(id,F0_step,dist,VOT_step,place) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    mutate(place = ifelse(place == "BP","/b/-/p/",
                          ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
    ggplot(aes(x=dist,color=place, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step))+
    
    scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
    facet_wrap(~place)+theme(legend.position = "none"), labels = c("D","E"),
    rel_widths = c(1.25,2)) -> same.ALE.plot




plot_grid(
  switch.ILL %>% 
    group_by(id,F0_step,dist,VOT_step) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    ggplot(aes(x=dist, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    theme(legend.position = "left")+
    labs(shape ="F0 step")+
    #stat_summary(aes(group = paste(id,F0_step), 
    #                shape =as.factor(F0_step )),
    #           position = position_dodge(0.3),
    #          geom="point",alpha = 0.25)+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step)),
  
  switch.ILL %>% 
    group_by(id,F0_step,dist,VOT_step,place) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    mutate(place = ifelse(place == "BP","/b/-/p/",
                          ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
    ggplot(aes(x=dist,color=place, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step))+
    
    scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
    facet_wrap(~place)+theme(legend.position = "none"), labels = c("A","B"),
    rel_widths = c(1.25,2)) -> switch.ILL.plot




plot_grid(
  switch.ALE %>% 
    group_by(id,F0_step,dist,VOT_step) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    ggplot(aes(x=dist, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    theme(legend.position = "left")+
    labs(shape ="F0 step")+
    #stat_summary(aes(group = paste(id,F0_step), 
    #                shape =as.factor(F0_step )),
    #           position = position_dodge(0.3),
    #          geom="point",alpha = 0.25)+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step)),
  
  switch.ALE %>% 
    group_by(id,F0_step,dist,VOT_step,place) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    mutate(place = ifelse(place == "bilabial","/b/-/p/",
                          ifelse(place == "alveolar", "/d/-/t/","/g/-/k/"))) %>%
    ggplot(aes(x=dist,color=place, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step))+

    scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
    facet_wrap(~place)+theme(legend.position = "none"), labels = c("D","E"),
  rel_widths = c(1.25,2)) -> switch.ALE.plot






plot_grid(
  switch.ILL.within %>% 
    group_by(id,F0_step,dist_block,VOT_step) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    
    ggplot(aes(x=dist_block, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    theme(legend.position = "left")+
    labs(shape ="F0 step")+
    #stat_summary(aes(group = paste(id,F0_step), 
    #                shape =as.factor(F0_step )),
    #           position = position_dodge(0.3),
    #          geom="point",alpha = 0.25)+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step)),
  
  switch.ILL.within %>% 
    group_by(id,F0_step,dist_block,VOT_step,place) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    mutate(place = ifelse(place == "BP","/b/-/p/",
                          ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
    ggplot(aes(x=dist_block,color=place, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step))+
    
    scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
    facet_wrap(~place)+theme(legend.position = "none"),
  labels = c("A","B"),rel_widths = c(1.25,2)) -> switch.ILL.within.plot




plot_grid(
  switch.ALE.within %>% 
    group_by(id,F0_step,dist_block,VOT_step) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    ggplot(aes(x=dist_block, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    theme(legend.position = "left")+
    labs(shape ="F0 step")+
    #stat_summary(aes(group = paste(id,F0_step), 
    #                shape =as.factor(F0_step )),
    #           position = position_dodge(0.3),
    #          geom="point",alpha = 0.25)+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step)),
  
  switch.ALE.within %>% 
    group_by(id,F0_step,dist_block,VOT_step,place) %>% 
    mutate(response_num = mean(response_num)) %>% slice(1) %>% 
    filter(trial_type=="test") %>% 
    mutate(place = ifelse(place == "BP","/b/-/p/",
                          ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
    ggplot(aes(x=dist_block,color=place, y= response_num,shape =as.factor(F0_step )))+
    stat_summary(fun.data = mean_cl_boot,
                 position = position_dodge(0.5))+
    coord_cartesian(ylim=c(0.05,0.95))+
    ylab("prop. voiceless\nresponse")+
    xlab("distributional condition")+
    stat_summary(geom="line",
                 position = position_dodge(0.5),
                 aes(group= F0_step))+
    
    scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
    facet_wrap(~place)+theme(legend.position = "none"),
  labels = c("D","E"),rel_widths = c(1.25,2)) -> switch.ALE.within.plot


### plot stimuli
library(fishualize)
library(cowplot)
fish_palettes()


same.ILL %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  mutate(target_place = ifelse(subjectGroup =="1"|subjectGroup =="2","/b/-/p/",
                               ifelse(subjectGroup =="3"|subjectGroup =="4","/d/-/t/",
                                      ifelse(subjectGroup =="5"|subjectGroup =="6","/g/-/k/","xxx")))) %>%
  group_by(subjectGroup,dist,F0_step,place,VOT_step) %>% slice(1) %>%
  ggplot(aes(x=as.numeric(VOT_step),y=F0_step,color=place,shape=place,size= trial_type))+
  scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
  # geom_hline(yintercept = 0.95)+
  #
  scale_size_manual(values = c(2.1,1.2))+
  guides(size = "none")+
  scale_x_continuous(limits=c(0.5,9.5),breaks = c(1,3,5,7,9))+scale_y_continuous(limits=c(0.5,9.5),
                                                                                 breaks = c(1,3,5,7,9))+
  scale_shape_manual(values = c(3,1,4))+
  xlab("VOT step")+ylab("F0 step")+
  theme(legend.position = "left")+
  geom_point(alpha=1,stroke =0.8,
             position = position_dodge(0.0))+facet_wrap(~paste(target_place,dist),nrow=1) -> same_plot;same_plot



switch.ILL %>%
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  mutate(target_place = ifelse(subjectGroup =="1"|subjectGroup =="2","/b/-/p/",
                               ifelse(subjectGroup =="3"|subjectGroup =="4","/d/-/t/",
                                      ifelse(subjectGroup =="5"|subjectGroup =="6","/g/-/k/","xxx")))) %>%
  group_by(subjectGroup,dist,F0_step,place,VOT_step) %>% slice(1) %>%
  ggplot(aes(x=as.numeric(VOT_step),y=F0_step,color=place,shape=place,size= trial_type))+
  scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
  # geom_hline(yintercept = 0.95)+
  
  scale_size_manual(values = c(2.1,1.2))+
  guides(size = "none")+
  scale_x_continuous(limits=c(0.5,9.5),breaks = c(1,3,5,7,9))+
  scale_y_continuous(limits=c(0.5,9.5),breaks = c(1,3,5,7,9))+
  scale_shape_manual(values = c(3,1,4))+
  xlab("VOT step")+ylab("F0 step")+
  theme(legend.position = "none")+
  geom_point(alpha=1,
             stroke =0.8, 
             position = position_dodge(0.0))+facet_wrap(~paste(target_place,dist),nrow=1) -> switch_plot


stim.legend <- get_legend(same_plot)

plot_grid(same_plot+theme(legend.position = "top"),
          switch_plot,
          nrow=2,
          labels = "AUTO",
          rel_heights  = c(1.3,1)) -> stimuli.plot

### emmeans

emmeans(brm.same.ILL, pairwise ~ F0_step|place*dist) -> emm.same.ILL

emm.same.ILL %>% as.data.frame() %>% 
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  filter(contrast!=".") %>% 
  ggplot(aes(x=dist,color=place,y=emmean))+
  geom_point(position = position_dodge(0.5),pch=18,size=3)+
  geom_hline(lty=2,yintercept = 0)+
  ylab("marginal contrast\nfor F0 step")+
  scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
  geom_errorbar(aes(ymin = lower.HPD,ymax = upper.HPD),
                position = position_dodge(0.5),width=0)+facet_wrap(~place) ->emm.same.ILL.plot

emmeans(brm.same.ALE, pairwise ~ F0_step|place*dist) -> emm.same.ALE
emm.same.ALE %>% as.data.frame() %>% 
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  filter(contrast!=".") %>% 
  ggplot(aes(x=dist,color=place,y=emmean))+
  geom_point(position = position_dodge(0.5),pch=18,size=3)+
  geom_hline(lty=2,yintercept = 0)+
  ylab("marginal contrast\nfor F0 step")+
  xlab("distributional condition")+
  scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
  geom_errorbar(aes(ymin = lower.HPD,ymax = upper.HPD),
                position = position_dodge(0.5),width=0) +facet_wrap(~place) ->emm.same.ALE.plot




emmeans(brm.switch.ILL, pairwise ~ F0_step|place*dist) -> emm.switch.ILL

emm.switch.ILL %>% as.data.frame() %>% 
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  filter(contrast!=".") %>% 
  ggplot(aes(x=dist,color=place,y=emmean))+
  geom_point(position = position_dodge(0.5),pch=18,size=3)+
  geom_hline(lty=2,yintercept = 0)+
  ylab("marginal contrast\nfor F0 step")+
  xlab("distributional condition")+
  scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
  geom_errorbar(aes(ymin = lower.HPD,ymax = upper.HPD),
                position = position_dodge(0.5),width=0) +facet_wrap(~place) ->emm.switch.ILL.plot

emmeans(brm.switch.ALE, pairwise ~ F0_step|place*dist) -> emm.switch.ALE
emm.switch.ALE %>% as.data.frame() %>% 
  mutate(place = ifelse(place == "bilabial","/b/-/p/",
                        ifelse(place == "alveolar", "/d/-/t/","/g/-/k/"))) %>%
  filter(contrast!=".") %>% 
  ggplot(aes(x=dist,color=place,y=emmean))+
  geom_point(position = position_dodge(0.5),pch=18,size=3)+
  geom_hline(lty=2,yintercept = 0)+
  ylab("marginal contrast\nfor F0 step")+
  xlab("distributional condition")+
  scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
  geom_errorbar(aes(ymin = lower.HPD,ymax = upper.HPD),
                position = position_dodge(0.5),width=0)+facet_wrap(~place) ->emm.switch.ALE.plot




emmeans(brm.switch.ILL.within, pairwise ~ F0_step|place*dist_block) -> emm.switch.ILL.within

emm.switch.ILL.within %>% as.data.frame() %>% 
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  filter(contrast!=".") %>% 
  ggplot(aes(x=dist_block,color=place,y=emmean))+
  geom_point(position = position_dodge(0.5),pch=18,size=3)+
  geom_hline(lty=2,yintercept = 0)+
  ylab("marginal contrast\nfor F0 step")+
  xlab("distributional condition")+
  scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
  geom_errorbar(aes(ymin = lower.HPD,ymax = upper.HPD),
                position = position_dodge(0.5),width=0)+facet_wrap(~place) ->emm.switch.ILL.within.plot



emmeans(brm.switch.ALE.within, pairwise ~ F0_step|place*dist_block) -> emm.switch.ALE.within

emm.switch.ALE.within %>% as.data.frame() %>% 
  mutate(place = ifelse(place == "BP","/b/-/p/",
                        ifelse(place == "DT", "/d/-/t/","/g/-/k/"))) %>%
  filter(contrast!=".") %>% 
  ggplot(aes(x=dist_block,color=place,y=emmean))+
  geom_point(position = position_dodge(0.5),pch=18,size=3)+
  geom_hline(lty=2,yintercept = 0)+
  ylab("marginal contrast\nfor F0 step")+
  xlab("distributional condition")+
  scale_color_fish(option = "Xyrichthys_novacula", discrete = T)+
  geom_errorbar(aes(ymin = lower.HPD,ymax = upper.HPD),
                position = position_dodge(0.5),width=0)+facet_wrap(~place) ->emm.switch.ALE.within.plot




### save figures into the figures folder

jpeg(filename = "figures/same.results.jpg",
     res = 400,units = "in",height = 7,width = 6.5)
plot_grid(NULL,
          same.ILL.plot,
          plot_grid(NULL,emm.same.ILL.plot+theme(legend.position = "none"),
                    rel_widths = c(1.25,2),
                    labels = c("","C")),
          NULL,
          same.ALE.plot,
          plot_grid(NULL,emm.same.ALE.plot+theme(legend.position = "none"),
                    rel_widths = c(1.25,2),
                    labels = c("","F")),
          ncol=1,
          labels = c("ILL frames","","","ALE frames"),
          rel_heights = c(0.2,1,1,0.2,1,1),
          rel_widths = c(1.25,2))
dev.off()


jpeg(filename = "figures/switch.results.jpg",
     res = 400,units = "in",height = 7,width = 6.5)
plot_grid(NULL,
          switch.ILL.plot,
          plot_grid(NULL,emm.switch.ILL.plot+theme(legend.position = "none"),
                    rel_widths = c(1.25,2),
                    labels = c("","C")),
          NULL,
          switch.ALE.plot,
          plot_grid(NULL,emm.switch.ALE.plot+theme(legend.position = "none"),
                    rel_widths = c(1.25,2),
                    labels = c("","F")),
          ncol=1,
          labels = c("ILL frames","","","ALE frames"),
          rel_heights = c(0.2,1,1,0.2,1,1),
          rel_widths = c(1.25,2))
dev.off()



jpeg(filename = "figures/switch.within.results.jpg",
     res = 400,units = "in",height = 7,width = 6.5)
plot_grid(NULL,
          switch.ILL.within.plot,
          plot_grid(NULL,emm.switch.ILL.within.plot+theme(legend.position = "none"),
          rel_widths = c(1.25,2),
          labels = c("","C")),
          NULL,
          switch.ALE.within.plot,
          plot_grid(NULL,emm.switch.ALE.within.plot+theme(legend.position = "none"),
                    rel_widths = c(1.25,2),
                    labels = c("","F")),
          ncol=1,
          labels = c("ILL frames","","","ALE frames"),
          rel_heights = c(0.2,1,1,0.2,1,1),
          rel_widths = c(1.25,2))
dev.off()


jpeg(filename = "figures/stimuli.figure.jpg",
     res = 400,units = "in",height = 4.25,width = 6.5)
stimuli.plot
dev.off()

jpeg(filename = "figures/interaction.figure.jpg",
     res = 400,units = "in",height = 3.5,width = 6.5)
interaction.figure
dev.off()


