library(readxl)
setwd("C:/Users/walkerro/OneDrive - University of Missouri/Desktop/R scripts/metin/tcsa")
d <- read_xlsx('DataS1.xlsx')

#summary stats
hist(d$penetration)
table(d$spear)
d$spear <- factor(d$spear)
colSums(is.na(d))

library(tidyverse)
d |> group_by(spear) |> summarize(penetration = mean(penetration,na.rm=T))
d |> group_by(spear) |> summarize(tcsp = mean(TCSP,na.rm=T)) |> arrange(spear)
d |> group_by(spear) |> summarize(tcsa = mean(TCSA,na.rm=T)) |> arrange(spear)
d |> group_by(spear) |> summarize(wound_width = mean(wound_width,na.rm=T))

out <- d |> group_by(spear) |> 
  summarize(wound_width_mean = mean(wound_width,na.rm=T),
            wound_width_min = min(wound_width,na.rm=T),
            wound_width_max = max(wound_width,na.rm=T),
            penetration_mean = mean(penetration,na.rm=T),
            penetration_min = min(penetration,na.rm=T),
            penetration_max = max(penetration,na.rm=T))
out
#write.csv(out, "means_min_max.csv", row.names = F)

penetration <- d |> group_by(spear) |> 
  summarize(Sample_size = sum(!is.na(penetration)),
            Mean = mean(penetration,na.rm=T),
            Standard_Deviation = sd(penetration,na.rm=T),
            Min = min(penetration,na.rm=T),
            Q1 = quantile(penetration, .25, na.rm=T),
            Median = quantile(penetration, .5, na.rm=T),
            Q3 = quantile(penetration, .75, na.rm=T),
            Max = max(penetration, na.rm=T),
            Range = Max-Min
            )
penetration
#write.csv(penetration, "penetration.csv", row.names = F)

wound <- d |> group_by(spear) |> 
  summarize(Sample_size = sum(!is.na(wound_width)),
            Mean = mean(wound_width,na.rm=T),
            Standard_Deviation = sd(wound_width,na.rm=T),
            Min = min(wound_width,na.rm=T),
            Q1 = quantile(wound_width, .25, na.rm=T),
            Median = quantile(wound_width, .5, na.rm=T),
            Q3 = quantile(wound_width, .75, na.rm=T),
            Max = max(wound_width, na.rm=T),
            Range = Max-Min
  )
wound
#write.csv(wound, "wound.csv", row.names = F)

d <- d |> group_by(Day) |>
  mutate(order=1:n()) |>
  ungroup()

d <- d |> arrange(Day) |>
  group_by(Day) |>
  mutate(day = cur_group_id()) |>
  ungroup()

#preliminary odels
summary(m <- lm(penetration ~ 1 + d_top_block + TCSA + mass + order + day, d))
summary(m <- lm(wound_width ~ 1 + penetration, d))
cor(d$penetration, d$wound_width, use = "pairwise.complete.obs")

#library(lme4) #https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified
#summary(m <- lmer(penetration ~ 1 + d_top_block + TCSA + (1+d_top_block|spear) + mass, d))
#ranef(m)
#library(ggeffects)
#g <- ggpredict(m, terms=c("TCSA", "d_top_block"), type = 'random') 
#plot(g)

#figures
library(ggplot2);library(cowplot);theme_set(theme_cowplot());library(ggplot2)
p1 <- ggplot(d, aes(x=TCSA, y=penetration)) + 
  geom_point(aes(color=spear)) + 
  geom_smooth(method=lm) +
  xlab("TCSA") + ylab("Penetration depth (cm)") + 
  #geom_rug() +
  #facet_grid(~aspec, scale="free") + 
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F)) +
  theme(legend.position=c(0.8, 0.55))
p1

p2 <- ggplot(d, aes(x=TCSP, y=penetration)) + 
  geom_point(aes(color=spear)) + 
  geom_smooth(method=lm) +
  xlab("TCSP") + ylab("Penetration depth (cm)") + 
  #geom_rug() +
  #facet_grid(~aspec, scale="free") + 
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F)) +
  theme(legend.position=c(0.8, 0.55))
p2
#ggsave("plot.jpeg", height =4, width=6)

ggplot(d, aes(x=TCSA, y= wound_width)) + 
  geom_point(aes(color=spear)) + 
  geom_smooth(method=lm) +
  xlab("TCSA") + #ylab("Penetration depth (cm)") + 
  #geom_rug() +
  #facet_grid(~aspec, scale="free") + 
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F)) +
  theme(legend.position=c(0.8, 0.55))

ggplot(d, aes(x=penetration, y= wound_width)) + 
  geom_point(aes(color=spear)) + 
  geom_smooth(method=lm) +
  #xlab("TCSA") + #ylab("Penetration depth (cm)") + 
  #geom_rug() +
  #facet_grid(~aspec, scale="free") + 
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F)) +
  theme(legend.position=c(0.8, 0.55))

ggplot() + 
  geom_point(data=d, aes(x=penetration, y= wound_width, color=spear)) + 
  geom_smooth(data=d, aes(x=penetration, y= wound_width, color=spear), method=lm, se=F) +
  #geom_smooth(data=d, aes(x=penetration, y=wound_width), method=lm, se=F) +
  xlab("Penetration depth (cm)") + ylab("Wound width (cm)") +
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F)) 
  #theme(legend.position=c(0.8, 0.55))
#ggsave("wound width by penetration.jpeg", height =4, width=6)

#bayesian models
library(brms);library(tidyverse)
get_prior(data = d, 
          family = gaussian, #Gamma(link=log),
          bf(penetration ~ 1 + TCSA + (1|spear), sigma ~ 0 + (1|spear)))
m1 <- brm(data = d,
           family = gaussian, #Gamma(link=log),
           bf(penetration ~ 1 + TCSA + (1|spear),
              sigma ~ 0 + (1|spear)),
           sample_prior = T,
           prior = c(prior("normal(0, .1)", class = "b"),
                     #prior("lkj(2)", class = 'cor'),
                     prior("exponential(1)", class = sd),
                     #prior("exponential(1)", class = sds)
                     #prior("exponential(1)", class = sigma),
                     prior("normal(0,1)", class = sd, dpar = sigma)
           ),
           #control = list(adapt_delta = .99, max_treedepth = 20),
           iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
           seed = 4)
print(summary(m1), digits=3)
saveRDS(m1, "m1.Rds")
m1 <- readRDS("m1.Rds")
prior_summary(m1)
ranef(m1)
plot(m1, widths = c(1, 2))
posterior_summary(m1)[1:4, ] %>% round(digits = 4)
bayes_R2(m1) #bayesian rsq (explained variance) .79
pp_check(m1, ndraws=2e2) #posterior predictive check
plot(hypothesis(m1, "TCSA = 0")) # posterior vs. prior
conditional_effects(m1)

library(DHARMa);library(DHARMa.helpers)
simres <- dh_check_brms(m1,integer=F)

library(sjPlot)
tab_model(m1, digits=3, file="m1.html")



get_prior(data = d, 
          family = gaussian, #Gamma(link=log),
          bf(penetration ~ 1 + TCSP + (1|spear), sigma ~ 0 + (1|spear)))
m2 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(penetration ~ 1 + TCSP + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .1)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m2), digits=3)
saveRDS(m2, "m2.Rds")
m2 <- readRDS("m2.Rds")
prior_summary(m2)
ranef(m2)
plot(m2, widths = c(1, 2))
posterior_summary(m2)[1:4, ] %>% round(digits = 4)
bayes_R2(m2) #bayesian rsq (explained variance) .79
pp_check(m2, ndraws=2e2) #posterior predictive check
plot(hypothesis(m2, "TCSP = 0")) # posterior vs. prior
conditional_effects(m2)
simres <- dh_check_brms(m2,integer=F)
tab_model(m2, digits=3, file="m2.html")


summary(lm(penetration ~ 1 + order + mass + d_top_block + TCSA,d))
get_prior(data = d, 
          family = gaussian, #Gamma(link=log),
          bf(penetration ~ 1 + order + mass + d_top_block + TCSA + (1|spear), sigma ~ 0 + (1|spear)))
m3 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(penetration ~ 1 + day + order + mass + d_top_block + TCSA + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .2)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m3), digits=3)
saveRDS(m3, "m3.Rds")
m3 <- readRDS("m3.Rds")
prior_summary(m3)
ranef(m3)
plot(m3, widths = c(1, 2))
posterior_summary(m3)[1:4, ] %>% round(digits = 4)
bayes_R2(m3) #bayesian rsq (explained variance) .81
pp_check(m3, ndraws=2e2) #posterior predictive check
plot(hypothesis(m3, "day = 0")) # posterior vs. prior
conditional_effects(m3)
simres <- dh_check_brms(m3,integer=F)
tab_model(m3, digits=3, file="m3.html")



summary(lm(penetration ~ 1 + order + mass + d_top_block + TCSP,d))
get_prior(data = d, 
          family = gaussian, #Gamma(link=log),
          bf(penetration ~ 1 + order + mass + d_top_block + TCSP + (1|spear), sigma ~ 0 + (1|spear)))
m4 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(penetration ~ 1 + day + order + mass + d_top_block + TCSP + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .2)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m4), digits=3)
saveRDS(m4, "m4.Rds")
m4 <- readRDS("m4.Rds")
prior_summary(m4)
ranef(m4)
plot(m4, widths = c(1, 2))
posterior_summary(m4)[1:4, ] %>% round(digits = 4)
bayes_R2(m4) #bayesian rsq (explained variance) .81
pp_check(m4, ndraws=2e2) #posterior predictive check
plot(hypothesis(m4, "day = 0")) # posterior vs. prior
conditional_effects(m4)
simres <- dh_check_brms(m4,integer=F)
tab_model(m4, digits=3, file="m4.html")


get_prior(data = d, 
          family = gaussian, #Gamma(link=log),
          bf(wound_width ~ 1 + TCSA + (1|spear), sigma ~ 0 + (1|spear)))
m5 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(wound_width ~ 1 + TCSA + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .1)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m5), digits=3)
saveRDS(m5, "m5.Rds")
m5 <- readRDS("m5.Rds")
prior_summary(m5)
ranef(m5)
plot(m5, widths = c(1, 2))
posterior_summary(m5)[1:4, ] %>% round(digits = 4)
bayes_R2(m5) #bayesian rsq (explained variance) .93
pp_check(m5, ndraws=2e2) #posterior predictive check
plot(hypothesis(m5, "TCSA = 0")) # posterior vs. prior
conditional_effects(m5)
simres <- dh_check_brms(m5,integer=F)
tab_model(m5, digits=3, file="m5.html")



get_prior(data = d, 
          family = gaussian, #Gamma(link=log),
          bf(wound_width ~ 1 + TCSP + (1|spear), sigma ~ 0 + (1|spear)))
m6 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(wound_width ~ 1 + TCSP + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .1)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m6), digits=3)
saveRDS(m6, "m6.Rds")
m6 <- readRDS("m6.Rds")
prior_summary(m6)
ranef(m6)
plot(m6, widths = c(1, 2))
posterior_summary(m6)[1:4, ] %>% round(digits = 4)
bayes_R2(m6) #bayesian rsq (explained variance) .93
pp_check(m6, ndraws=2e2) #posterior predictive check
plot(hypothesis(m6, "TCSP = 0")) # posterior vs. prior
conditional_effects(m6)
simres <- dh_check_brms(m6,integer=F)
tab_model(m6, digits=3, file="m6.html")




m7 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(wound_width ~ 1 + day + order + mass + d_top_block + TCSA + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .2)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m7), digits=3)
saveRDS(m7, "m7.Rds")
m7 <- readRDS("m7.Rds")
prior_summary(m7)
ranef(m7)
plot(m7, widths = c(1, 2))
posterior_summary(m7)[1:4, ] %>% round(digits = 4)
bayes_R2(m7) #bayesian rsq (explained variance) .93
pp_check(m7, ndraws=2e2) #posterior predictive check
plot(hypothesis(m7, "day = 0")) # posterior vs. prior
conditional_effects(m7)
simres <- dh_check_brms(m7,integer=F)
tab_model(m7, digits=3, file="m7.html")



m8 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(wound_width ~ 1 + day + order + mass + d_top_block + TCSP + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .2)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m8), digits=3)
saveRDS(m8, "m8.Rds")
m8 <- readRDS("m8.Rds")
prior_summary(m8)
ranef(m8)
plot(m8, widths = c(1, 2))
posterior_summary(m8)[1:4, ] %>% round(digits = 4)
bayes_R2(m8) #bayesian rsq (explained variance) .93
pp_check(m8, ndraws=2e2) #posterior predictive check
plot(hypothesis(m8, "day = 0")) # posterior vs. prior
conditional_effects(m8)
simres <- dh_check_brms(m8,integer=F)
tab_model(m8, digits=3, file="m8.html")



m9 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(wound_width ~ 1 + penetration + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .1)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m9), digits=3)
saveRDS(m9, "m9.Rds")
m9 <- readRDS("m9.Rds")
prior_summary(m9)
ranef(m9)
plot(m9, widths = c(1, 2))
posterior_summary(m9)[1:4, ] %>% round(digits = 4)
bayes_R2(m9) #bayesian rsq (explained variance) .93
pp_check(m9, ndraws=2e2) #posterior predictive check
plot(hypothesis(m9, "penetration = 0")) # posterior vs. prior
conditional_effects(m9)
simres <- dh_check_brms(m9,integer=F)
tab_model(m9, digits=3, file="m9.html")



m10 <- brm(data = d,
          family = gaussian, #Gamma(link=log),
          bf(wound_width ~ 1 + day + order + mass + d_top_block + penetration + (1|spear),
             sigma ~ 0 + (1|spear)),
          sample_prior = T,
          prior = c(prior("normal(0, .2)", class = "b"),
                    #prior("lkj(2)", class = 'cor'),
                    prior("exponential(1)", class = sd),
                    #prior("exponential(1)", class = sds)
                    #prior("exponential(1)", class = sigma),
                    prior("normal(0,1)", class = sd, dpar = sigma)
          ),
          #control = list(adapt_delta = .99, max_treedepth = 20),
          iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
          seed = 4)
print(summary(m10), digits=3)
saveRDS(m10, "m10.Rds")
m10 <- readRDS("m10.Rds")
prior_summary(m10)
ranef(m10)
plot(m10, widths = c(1, 2))
posterior_summary(m10)[1:4, ] %>% round(digits = 4)
bayes_R2(m10) #bayesian rsq (explained variance) .
pp_check(m10, ndraws=2e2) #posterior predictive check
plot(hypothesis(m10, "day = 0")) # posterior vs. prior
conditional_effects(m10)
simres <- dh_check_brms(m10,integer=F)
tab_model(m10, digits=3, file="m10.html")


m11 <- brm(data = d,
           family = gaussian, #Gamma(link=log),
           bf(wound_width ~ 1 + penetration),
              #sigma ~ 0 + (1|spear)),
           sample_prior = T,
           prior = c(prior("normal(0, .2)", class = "b"),
                     #prior("lkj(2)", class = 'cor'),
                     #prior("exponential(1)", class = sd)
                     #prior("exponential(1)", class = sds)
                     prior("exponential(1)", class = sigma)
                     #prior("normal(0,1)", class = sd, dpar = sigma)
           ),
           #control = list(adapt_delta = .99, max_treedepth = 20),
           iter = 1e4, warmup = 1e3, chains = 4, cores = 4,
           seed = 4)
print(summary(m11), digits=3)
saveRDS(m11, "m11.Rds")
m11 <- readRDS("m11.Rds")
prior_summary(m11)
#ranef(m11)
plot(m11, widths = c(1, 2))
posterior_summary(m11)[1:4, ] %>% round(digits = 4)
bayes_R2(m11) #bayesian rsq (explained variance) 
sqrt(bayes_R2(m11)) #correlation 
pp_check(m11, ndraws=2e2) #posterior predictive check
plot(hypothesis(m11, "penetration = 0")) # posterior vs. prior
conditional_effects(m11)
simres <- dh_check_brms(m11,integer=F)
tab_model(m11, digits=3, file="m11.html")

#compare model fits
#m1 <- add_criterion(m1, criterion = "loo")
#m2 <- add_criterion(m2, criterion = "loo")
#loo_compare(m1, m2)
#bayes_factor(m1, m2)


#final graphs
conditional_effects(m3, effects = "TCSA", prob=.5)
set.seed(1)
me1 <- conditional_effects(m3, effects = "TCSA", ndraws=200,
                           #prob = .95, 
                           spaghetti = T)
me1

gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]}

p1 <- plot(me1, plot = FALSE)[[1]] +
  geom_point(data=d, aes(y=penetration, x=TCSA, color=factor(spear)), show.legend = F) +
  ylab("Penetration depth (cm)") + xlab("TCSA") +
  theme(legend.position="none") +
  #guides(color= guide_legend(title = "Kinetic energy")) +
  #scale_y_continuous(limits=c(9,25)) +
  annotate(geom="text", x=158, y=3, label="1", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=57.8, y=5.5, label="2", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=92.1, y=4.2, label="3", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=160, y=2.7, label="4", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=57.4, y=12, label="5", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=92.4, y=12, label="6", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=86.2, y=5, label="7", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=47.2, y=12.2, label="8", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=185, y=5, label="9", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=193, y=2.2, label="10", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=122, y=2.8, label="11", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=120, y=4, label="12", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=104, y=4.6, label="13", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=107, y=9.2, label="14", color='black', size =3) + #gg_color_hue(3)) +
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F))
  #ggsave("plot2.jpeg", height =4, width=6)
p1


set.seed(1)
me2 <- conditional_effects(m4, effects = "TCSP", ndraws=200,
                           #prob = .95, 
                           spaghetti = T)
me2

p2 <- plot(me2, plot = FALSE)[[1]] +
  geom_point(data=d, aes(y=penetration, x=TCSP, color=factor(spear)), show.legend = F) +
  ylab("Penetration depth (cm)") + xlab("TCSP") +
  theme(legend.position="none") +
  #guides(color= guide_legend(title = "Kinetic energy")) +
  #scale_y_continuous(limits=c(9,25)) +
  annotate(geom="text", x=81.8, y=3.1, label="1", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=41, y=5.5, label="2", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=56., y=10.3, label="3", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=78.7, y=2.7, label="4", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=40, y=5.9, label="5", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=57.1, y=12, label="6", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=52.4, y=5, label="7", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=34.9, y=12.2, label="8", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=99.8, y=5, label="9", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=91.8, y=2.2, label="10", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=69.2, y=2.7, label="11", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=75.6, y=4, label="12", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=55.2, y=4.3, label="13", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=58.5, y=9., label="14", color='black', size =3) + #gg_color_hue(3)) +
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F))
#ggsave("plot2.jpeg", height =4, width=6)
p2

library(patchwork)
(p1 / p2) + plot_annotation(tag_levels ="A")
ggsave("plot.jpeg", height = 7, width=6, dpi=600)


#wound width graphs
conditional_effects(m7, effects = "TCSA", prob=.5)
set.seed(1)
me1 <- conditional_effects(m7, effects = "TCSA", ndraws=200,
                           #prob = .95, 
                           spaghetti = T)
me1

gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1)
hcl(h = hues, l = 65, c = 100)[1:n]}

p1 <- plot(me1, plot = FALSE)[[1]] +
  geom_point(data=d, aes(y=wound_width, x=TCSA, color=factor(spear)), show.legend = F) +
  ylab("Wound width (cm)") + xlab("TCSA") +
  theme(legend.position="none") +
  #guides(color= guide_legend(title = "Kinetic energy")) +
  #scale_y_continuous(limits=c(9,25)) +
  annotate(geom="text", x=158, y=3.2, label="1", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=60, y=1.65, label="2", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=90, y=3.3, label="3", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=160, y=5.2, label="4", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=57.4, y=1.5, label="5", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=92.4, y=4.1, label="6", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=86.2, y=1.9, label="7", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=47.2, y=1.4, label="8", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=185, y=5.7, label="9", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=193, y=5.7, label="10", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=122, y=2.7, label="11", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=121, y=4.3, label="12", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=104, y=4.1, label="13", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=107, y=2.2, label="14", color='black', size =3) + #gg_color_hue(3)) +
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F))
#ggsave("plot2.jpeg", height =4, width=6)
p1


set.seed(1)
me2 <- conditional_effects(m8, effects = "TCSP", ndraws=200,
                           #prob = .95, 
                           spaghetti = T)
me2

p2 <- plot(me2, plot = FALSE)[[1]] +
  geom_point(data=d, aes(y=wound_width, x=TCSP, color=factor(spear)), show.legend = F) +
  ylab("Wound width (cm)") + xlab("TCSP") +
  theme(legend.position="none") +
  #guides(color= guide_legend(title = "Kinetic energy")) +
  #scale_y_continuous(limits=c(9,25)) +
  annotate(geom="text", x=81.8, y=5.2, label="1", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=42, y=1.65, label="2", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=56.5, y=3.5, label="3", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=78.7, y=5.2, label="4", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=40.8, y=2.6, label="5", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=57.1, y=4.1, label="6", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=52.4, y=1.9, label="7", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=34.9, y=1.4, label="8", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=99.8, y=6, label="9", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=91.8, y=5.7, label="10", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=69.2, y=2.7, label="11", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=75.6, y=4.3, label="12", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=55.2, y=4.1, label="13", color='black', size =3) + #gg_color_hue(3)) +
  annotate(geom="text", x=57.1, y=2.2, label="14", color='black', size =3) + #gg_color_hue(3)) +
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F))
#ggsave("plot2.jpeg", height =4, width=6)
p2

library(patchwork)
(p1 / p2) + plot_annotation(tag_levels ="A")
ggsave("plot_ww.jpeg", height = 7, width=6, dpi=600)


#pdepth and wound width graph
conditional_effects(m11, effects = "penetration", prob=.5)
set.seed(1)
me1 <- conditional_effects(m11, effects = "penetration", ndraws=200,
                           #prob = .95, 
                           spaghetti = T)
me1

gg_color_hue <- function(n) {hues = seq(15, 375, length = n + 1)
hcl(h = hues, l = 65, c = 100)[1:n]}

p1 <- plot(me1, plot = FALSE)[[1]] +
  geom_point(data=d, aes(x=penetration, y=wound_width, color=factor(spear)), show.legend = T) +
  xlab("Penetration depth (cm)") + ylab("Wound width (cm)") +
  #theme(legend.position="none") +
  #guides(color= guide_legend(title = "Kinetic energy")) +
  #scale_y_continuous(limits=c(9,25)) +
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F))
p1
ggsave("wound width penetration.jpeg", height =5, width=7)

p2 <- plot(me1, plot = FALSE)[[1]] +
  geom_point(data=d, aes(x=penetration, y=wound_width, color=factor(spear)), show.legend = T) +
  geom_smooth(data=d, aes(x=penetration, y= wound_width, color=spear), method=lm, se=F) +
  xlab("Penetration depth (cm)") + ylab("Wound width (cm)") +
  #theme(legend.position="none") +
  #guides(color= guide_legend(title = "Kinetic energy")) +
  #scale_y_continuous(limits=c(9,25)) +
  guides(fill=guide_legend(title="Point type", reverse=F)) +
  guides(color= guide_legend(title = "Point type", reverse=F))
p2
ggsave("wound width penetration2.jpeg", height =5, width=7, dpi=600)


