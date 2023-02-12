#' Quant_Methods_HW

#' ---
#' title: HW3
#' author: Briar Ownby-Connolly
#' date: ' `r paste("created on", Sys.Date())`'
#' output: html_document
#' ---
#' 


trees <- read.csv('https://raw.githubusercontent.com/dmcglinn/quant_methods/gh-pages/data/treedata_subset.csv')
trees
library(car)

sp_cov = with(trees, tapply(cover, list(plotID, spcode),
                            function(x) round(mean(x))))
sp_cov = ifelse(is.na(sp_cov), 0, sp_cov)
sp_cov = data.frame(plotID = row.names(sp_cov), sp_cov)

cols_to_select = c('elev', 'tci', 'streamdist', 'disturb', 'beers')
env = aggregate(trees[ , cols_to_select], by = list(trees$plotID),
                function(x) x[1])
names(env) [1] = 'plotID'

site_data = merge(sp_cov, env, by='plotID')
abies = site_data[ , c('ABIEFRA', cols_to_select)]
acer = site_data[ , c('ACERRUB', cols_to_select)]

names(abies)[1] = 'cover'
names(acer) [1] = 'cover'

#' ABIES 

pairs(~ cover + elev + tci + streamdist + beers, data = abies)
pairs(~ cover + elev + tci + streamdist + beers, data = abies, upper.panel = panel.smooth, lower.panel = panel.cor)


plot(~ elev + cover, data = abies)
plot(~ tci + cover, data = abies)

main_effect_elev <- lm(cover ~ elev, data = abies)

par(mfrow=c(1, 2))
plot(~ elev + cover, data = abies)
abline(main_effect_elev)

main_effect_tci <- lm(cover ~ tci, data = abies)

plot(~ tci + cover, data = abies)
par(mfrow=c(1, 2))
abline(main_effect_tci)

int_eff_ET <- lm(cover ~ elev + tci + elev:tci,
                 data = abies)
int_eff_ET

plot(int_eff_ET)

summary(main_effect_elev)
anova(main_effect_elev)

summary(main_effect_tci)
anova(main_effect_tci)

summary(int_eff_ET)
anova(int_eff_ET)

anova(main_effect_elev, main_effect_tci, int_eff_ET) 

Anova(update(int_eff_ET, . ~ . -1), type = 3)


abies_poi = glm(cover ~ tci + elev + streamdist + beers , data = abies,
                family = 'poisson')
abies_poi

pseudo_r2 = function(abies_poi) {
  1 - abies_poi$deviance / abies_poi$null.deviance
}


with(summary(abies_poi), 1 - deviance/null.deviance)


xrange = c(1500, 2000)
plot(main_effect_elev, data = abies)
lines(xrange,
      predict(main_effect_elev,
              newdata = data.frame(elev = xrange)))



#' ACER

pairs(~ cover + elev + tci + streamdist + beers, data = acer)
pairs(~ cover + elev + tci + streamdist + beers, data = acer, upper.panel = panel.smooth, lower.panel = panel.cor)


plot(~ elev + cover, data = acer)
plot(~ tci + cover, data = acer)

main_effect_elev2 <- lm(cover ~ elev, data = acer)

par(mfrow=c(1, 2))
plot(~ elev + cover, data = acer)
abline(main_effect_elev2)

main_effect_tci2 <- lm(cover ~ tci, data = acer)

plot(~ tci + cover, data = acer)
par(mfrow=c(1, 2))
abline(main_effect_tci2)

int_eff_ET2 <- lm(cover ~ elev + tci + elev:tci,
                  data = acer)
int_eff_ET2

plot(int_eff_ET2)

summary(main_effect_elev2)
anova(main_effect_elev2)

summary(main_effect_tci2)
anova(main_effect_tci2)

summary(int_eff_ET2)
anova(int_eff_ET2)

anova(main_effect_elev2, main_effect_tci2, int_eff_ET2)  

Anova(update(int_eff_ET2, . ~ . -1), type = 3)


acer_poi = glm(cover ~ tci + elev + streamdist + beers , data = acer,
               family = 'poisson')
acer_poi

pseudo_r2 = function(acer_poi) {
  1 - acer_poi$deviance / acer_poi$null.deviance
}


with(summary(acer_poi), 1 - deviance/null.deviance)


xrange = c(1500, 2000)
plot(main_effect_elev2, data = acer)
lines(xrange,
      predict(main_effect_elev2,
              newdata = data.frame(elev = xrange)))



#' 1a. How well does the exploratory model appear to explain cover?
In Abies, the exploratory model gives a decent overview of the explanatory variables in relation to cover, where elevation and tci seem to be 
interesting ones to explor further with models. Due to acer being a generalist speices, the exploratory models use in 
explaining cover just supports to notion of prevelance seen with this typs of speices. 

#' 2a. Which explanatory variables are the most important?
For the speices abies, the explanatory variables elevatin and tci seem to be most important. 
For comparison, elevation and tci are also important for acer.

#' 3a. Do model diagnostics indicate any problems with violations of OLS assumptions?
Yes, the diagonstics indicate problems with violations of the three OLS assumptions.

#' 4a. Are you able to explain variance in one species better than another, why might this be the case?
Variance can be explained better in abies as opposed to acer, this might be due to the different behaviors of
these species, with abies being a habitat specialist and acer being a habitat generalist.

#' 2. Compare your qualitative assessment of which variables were most important in each model. 
Does it appear that changing the error distribution changed the results much? In what ways?
  chaning the error distribution changed the results slightly. The pseudo code resulted in explaining significantly more variance for abies, 
while explained variance in acer was not as large. Adjusted r2 values also reflected this.


#' 3. Provide a plain English summary (i.e., no statistics) of what you have found and what conclusions we can take away from your analysis?
The interaction effect model seems to explain more of the variation within the species abies and acer for comparision. Elevation and tci were the explanatory 
variables looked at in reference to species cover, providing insight into the different behaviors of species generalist and specialist, where more variance was
found to be explianed with our specialist species as opposed to generalist species, through our models. 