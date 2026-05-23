
# This script runs the models for the paper

# load packages and set wd
library(brms);library(tidyverse);library(bayestestR);library(emmeans)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load in data from the data folder
same.ILL <- read.csv("data/same_place_ILL_frames.csv")
same.ALE <- read.csv("data/same_place_ALE_frames.csv")

switch.ILL <- read.csv("data/switch_place_ILL_frames.csv")
switch.ALE <- read.csv("data/switch_place_ALE_frames.csv") 

switch.ILL.within <- read.csv("data/within_subject_switch_place_ILL_frames.csv")
switch.ALE.within <- read.csv("data/within_subject_switch_place_ALE_frames.csv")

# I. same place models 

# same place ILL frames

same.ILL %>% filter(trial_type=="test") -> same.ILL.test

same.ILL.test$F0_step<-as.factor(same.ILL.test$F0_step)
contrasts(same.ILL.test$F0_step) <-c(-0.5,0.5)
same.ILL.test$dist<-as.factor(same.ILL.test$dist) 
contrasts(same.ILL.test$dist) <-c(-0.5,0.5)
same.ILL.test$place<-as.factor(same.ILL.test$place)
contrasts(same.ILL.test$place) <-contr.sum(3)


model.same.place.ILL<-brm(formula= response_num~F0_step*dist*place+
                        (1+F0_step|id),
                      data = same.ILL.test,
                      prior = c(prior(normal(0,1.5), class = Intercept),
                                prior(normal(0,1.5), class = b)),
                      family = bernoulli(link = "logit"),
                      file = "models/same.ILL.place",
                      iter = 11000, warmup = 1000,cores=4,chains = 4,
                      control = list(adapt_delta=0.99,max_treedepth=15))

pd(model.same.place.ILL)
pd(emmeans(model.same.place.ILL, pairwise ~ F0_step|dist))
emmeans(model.same.place.ILL,  "place")

# same place ALE frames
same.ALE %>% filter(trial_type=="test") -> same.ALE.test

same.ALE.test$F0_step<-as.factor(same.ALE.test$F0_step)
contrasts(same.ALE.test$F0_step) <-c(-0.5,0.5)
same.ALE.test$dist<-as.factor(same.ALE.test$dist) 
contrasts(same.ALE.test$dist) <-c(-0.5,0.5)
same.ALE.test$place<-as.factor(same.ALE.test$place)
contrasts(same.ALE.test$place) <-contr.sum(3)


model.same.place.ALE<-brm(formula= response_num~F0_step*dist*place+
                            (1+F0_step|id),
                          data = same.ALE.test,
                          prior = c(prior(normal(0,1.5), class = Intercept),
                                    prior(normal(0,1.5), class = b)),
                          family = bernoulli(link = "logit"),
                          file = "models/same.ALE.place",
                          iter = 11000, warmup = 1000,cores=4,chains = 4,
                          control = list(adapt_delta=0.99,max_treedepth=15))

pd(model.same.place.ALE)
pd(emmeans(model.same.place.ALE, pairwise ~ F0_step|dist))



# II. switched place models - between subjects 

# switched place ILL frames

switch.ILL %>% filter(trial_type=="test") -> switch.ILL.test

switch.ILL.test$F0_step<-as.factor(switch.ILL.test$F0_step)
contrasts(switch.ILL.test$F0_step) <-c(-0.5,0.5)
switch.ILL.test$dist<-as.factor(switch.ILL.test$dist)
contrasts(switch.ILL.test$dist) <-c(-0.5,0.5)
switch.ILL.test$place<-as.factor(switch.ILL.test$place)
contrasts(switch.ILL.test$place) <-contr.sum(3)


model.switch.place.ILL<-brm(formula= response_num~F0_step*dist*place+
                            (1+F0_step|id),
                          data = switch.ILL.test,
                          prior = c(prior(normal(0,1.5), class = Intercept),
                                    prior(normal(0,1.5), class = b)),
                          family = bernoulli(link = "logit"),
                          file = "models/switch.ILL.place",
                          iter = 11000, warmup = 1000,cores=4,chains = 4,
                          control = list(adapt_delta=0.99,max_treedepth=15))

pd(model.switch.place.ILL)
pd(emmeans(model.switch.place.ILL, pairwise ~ F0_step|dist))


# switched place ALE frames

switch.ALE$F0_step<-as.factor(switch.ALE$F0_step)
contrasts(switch.ALE$F0_step) <-c(-0.5,0.5)
switch.ALE$dist<-as.factor(switch.ALE$dist)
contrasts(switch.ALE$dist) <-c(-0.5,0.5)
switch.ALE$place<-as.factor(switch.ALE$place)
contrasts(switch.ALE$place) <-contr.sum(3)



model.switch.place.ALE<-brm(formula= response_num~F0_step*dist*place+
                              (1+F0_step|id),
                            data = switch.ALE,
                            prior = c(prior(normal(0,1.5), class = Intercept),
                                      prior(normal(0,1.5), class = b)),
                            family = bernoulli(link = "logit"),
                            file = "models/switch.ALE.place",
                            iter = 11000, warmup = 1000,cores=4,chains = 4,
                            control = list(adapt_delta=0.99,max_treedepth=15))


pd(model.switch.place.ALE)
  
emmeans <- emmeans(model.switch.place.ALE, ~ F0_step * dist)
contrasts <- contrast(emmeans, method = "pairwise", by = "dist")

# III. switched place models - within subjects 

# switched place ILL frames

switch.ILL.within %>% filter(trial_type=="test") -> switch.ILL.within.test

switch.ILL.within.test$F0_step<-as.factor(switch.ILL.within.test$F0_step)
contrasts(switch.ILL.within.test$F0_step) <-c(-0.5,0.5)
switch.ILL.within.test$dist_block<-as.factor(switch.ILL.within.test$dist_block)
contrasts(switch.ILL.within.test$dist_block) <-c(-0.5,0.5)
switch.ILL.within.test$block_order<-as.factor(switch.ILL.within.test$block_order)
contrasts(switch.ILL.within.test$block_order) <-c(-0.5,0.5)
switch.ILL.within.test$place<-as.factor(switch.ILL.within.test$place)
contrasts(switch.ILL.within.test$place) <-contr.sum(3)


model.switch.within.ILL<-brm(formula = response_num ~ F0_step * dist_block * place * block_order + 
                               (1 + F0_step * dist_block | id ),
                             data = switch.ILL.within.test,
                             prior = c(prior(normal(0,1.5), class = Intercept),
                                       prior(normal(0,1.5), class = b)),
                             family = bernoulli(link = "logit"),
                             file = "models/within.subj.switch.ILL.place",
                             iter = 11000,
                             warmup = 1000,
                             cores=4,
                             chains = 4,
                             control = list(adapt_delta=0.99,max_treedepth=15))

pd(model.switch.within.ILL)
pd(emmeans(model.switch.within.ILL, pairwise ~ F0_step|dist_block))


emmeans(model.switch.within.ILL, c("dist_block","block_order")) %>%  as.data.frame()  %>% 
  ggplot(aes(x=block_order,y = emmean, ymin = lower.HPD, ymax = upper.HPD,color = dist_block))+
  geom_point()+geom_errorbar()


# switched place ALE frames
switch.ALE.within %>% filter(trial_type=="test") -> switch.ALE.within.test

switch.ALE.within.test$F0_step<-as.factor(switch.ALE.within.test$F0_step)
contrasts(switch.ALE.within.test$F0_step) <-c(-0.5,0.5)
switch.ALE.within.test$dist_block<-as.factor(switch.ALE.within.test$dist_block)
contrasts(switch.ALE.within.test$dist_block) <-c(-0.5,0.5)
switch.ALE.within.test$block_order<-as.factor(switch.ALE.within.test$block_order)
contrasts(switch.ALE.within.test$block_order) <-c(-0.5,0.5)
switch.ALE.within.test$place<-as.factor(switch.ALE.within.test$place)
contrasts(switch.ALE.within.test$place) <-contr.sum(3)


model.switch.within.ALE<-brm(formula = response_num ~ 
                               F0_step * dist_block * place * block_order + 
                               (1 + F0_step * dist_block | id ),
                             data = switch.ALE.within.test,
                             prior = c(prior(normal(0,1.5), class = Intercept),
                                       prior(normal(0,1.5), class = b)),
                             family = bernoulli(link = "logit"),
                             file = "models/within.subj.switch.ALE.place",
                             iter = 11000,
                             warmup = 1000,
                             cores=4,
                             chains = 4,
                             control = list(adapt_delta=0.99,max_treedepth=15))

pd(model.switch.within.ALE) 
pd(emmeans(model.switch.within.ALE, pairwise ~ F0_step|dist_block))

## IV. combined model with all data assessing differences for same vs. switched place learning across experiments, the three way interaction between same/switch, F0, and distribution

# first the data frames are standardised and subset to have just the critical variables, in order to combine them

switch.ALE$frame <-"ALE"
switch.ALE$same_switch <-"switch"

switch.ALE %>%  # this data frame used different labels for PoA, which are standardised here
  mutate(place = ifelse(place =="bilabial","BP",
                        ifelse(place =="alveolar","DT", 
                               ifelse(place =="velar","GK",place)))) -> switch.ALE
                                      
switch.ALE$place<-as.factor(switch.ALE$place)                        

switch.ALE %>% 
  select(F0_step,dist,place,id,frame,same_switch,response_num) -> switch.ALE.to.combine

switch.ILL.test$frame <-"ILL"
switch.ILL.test$same_switch <-"switch"
switch.ILL.test %>% 
  select(F0_step,dist,place,id,frame,same_switch,response_num) -> switch.ILL.to.combine

same.ALE.test$frame <-"ALE"
same.ALE.test$same_switch <-"same"
same.ALE.test %>% 
  select(F0_step,dist,place,id,frame,same_switch,response_num) -> same.ALE.to.combine

same.ILL.test$frame <-"ILL"
same.ILL.test$same_switch <-"same"
same.ILL.test %>% 
  select(F0_step,dist,place,id,frame,same_switch,response_num) -> same.ILL.to.combine

switch.ALE.within.test$frame <-"ALE"
switch.ALE.within.test$same_switch <-"switch"
switch.ALE.within.test %>% 
  mutate(dist = dist_block) %>% # dist_block renamed to dist, to standardise
  select(F0_step,dist,place,id,frame,same_switch,response_num) -> switch.ALE.within.to.combine

switch.ILL.within.test$frame <-"ILL"
switch.ILL.within.test$same_switch <-"switch"
switch.ILL.within.test %>% 
  mutate(dist = dist_block) %>% # dist_block renamed to dist, to standardise
  select(F0_step,dist,place,id,frame,same_switch,response_num) -> switch.ILL.within.to.combine

# then all data is combined

combined.data<-rbind(switch.ALE.within.to.combine,
                     switch.ILL.within.to.combine,
                     switch.ALE.to.combine,
                     switch.ILL.to.combine,
                     same.ALE.to.combine,
                     same.ILL.to.combine) 



# factor coding for the model
combined.data$F0_step<-as.factor(combined.data$F0_step)
contrasts(combined.data$F0_step) <-c(-0.5,0.5)
combined.data$dist<-as.factor(combined.data$dist)
contrasts(combined.data$dist) <-c(-0.5,0.5)
combined.data$same_switch<-as.factor(combined.data$same_switch)
contrasts(combined.data$same_switch) <-c(-0.5,0.5)
combined.data$frame<-as.factor(combined.data$frame)
contrasts(combined.data$frame) <-c(-0.5,0.5)
contrasts(combined.data$place) <-contr.sum(3)

# un-commenting the below code will produce a visualisation of the overall effect. 
# combined.data %>%
#   ggplot(aes(x= F0_step,y=response_num,color = dist))+
#   stat_summary()+facet_wrap(~same_switch)+ylim(0,1)


model.combined<-brm(formula= response_num ~
                              F0_step*dist*place*same_switch*frame+
                              (1+F0_step|id),
                            data = combined.data,
                            prior = c(prior(normal(0,1.5), class = Intercept),
                                      prior(normal(0,1.5), class = b)),
                            family = bernoulli(link = "logit"),
                            file = "models/combined.model",
                            iter = 11000, warmup = 1000,cores=4,chains = 4,
                            control = list(adapt_delta=0.99,max_treedepth=15)) 
summary(model.combined)
pd(model.combined) %>% filter(pd > 0.95)

same.switch.em <- emmeans(model.combined, ~ F0_step:dist |same_switch )
same.switch.em.con <- as.data.frame(contrast(same.switch.em, interaction = "pairwise"))




## V. Computing Bayes Factors for all models - this takes some time! 


# BF.1 <- bayesfactor_parameters(model.same.place.ILL, null = 0)
# BF.2 <- bayesfactor_parameters(model.switch.place.ILL, null = 0)
# BF.3<- bayesfactor_parameters(model.switch.within.ILL, null = 0)
# BF.4 <- bayesfactor_parameters(model.same.place.ALE, null = 0)
# BF.5<- bayesfactor_parameters(model.switch.place.ALE, null = 0)
# BF.6<- bayesfactor_parameters(model.switch.within.ALE, null = 0)
# BF.7<- bayesfactor_parameters(model.combined, null = 0)
