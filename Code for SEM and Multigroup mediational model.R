setwd('~/Desktop')

## Libraries ##
library('psych')
library('lavaan')
library('semTools')
library('tidyverse')
library('dplyr')
library('mediation')

#############################################################################
#### Latent variable models (Structural regression models) ####
### Overall sample P-E fit model with P-E fit predicting psychological experiences of work and work outcomes.  ###
model.Overall.facet <- '
#Measurement model
PE_f =~ I.F_indicator + D.A_indicator + P.S_indicator

WLB =~ WLB1 + WLB2 + WLB3 + WLB4
EngagementFa1 =~ Engagement1 + Engagement2 + Engagement3 
EngagementFa2 =~ Engagement4 + Engagement5 + Engagement6  
EngagementFa3 =~ Engagement7 +  Engagement8 + Engagement9

BurnoutFa1 =~ Burnout1 + Burnout2 + Burnout3 + Burnout4 + Burnout6
BurnoutFa2 =~ Burnout8 + Burnout9 + Burnout14 + Burnout15 
BurnoutFa3 =~ Burnout5 +  Burnout7 + Burnout10 + Burnout11 + Burnout12 + Burnout16

Performance =~ Performance1 + Performance2 + Performance3 + Performance4 + Performance5
Turnover =~ Turnover1 + Turnover2 + Turnover3

Consci =~ ConsciPar1 + ConsciPar2 + ConsciPar3

#Structural model
WLB ~ PE_f
EngagementFa1 ~ PE_f 
EngagementFa2 ~ PE_f 
EngagementFa3 ~ PE_f 
BurnoutFa1 ~ PE_f 
BurnoutFa2 ~ PE_f 
BurnoutFa3 ~ PE_f 

Performance ~ WLB + EngagementFa1 + EngagementFa2 + EngagementFa3 + BurnoutFa1 + BurnoutFa2 + BurnoutFa3 + Consci  
Turnover ~ WLB + EngagementFa1 + EngagementFa2 + EngagementFa3 + BurnoutFa1 + BurnoutFa2 + BurnoutFa3 + Consci  
'
model.Overall.facet.fit <- sem(model.Overall.facet, data=df, missing="fiml")
summary(model.Overall.facet.fit, fit.measures=T, standardized=T) 

#############################################################################

#############################################################################
#### Multi-group model####
lv.1pe <- '
#Measurement model
PE_f =~ I.F_indicator + D.A_indicator + P.S_indicator

WLB =~ WLB1 + WLB2 + WLB3 + WLB4
EngagementFa1 =~ Engagement1 + Engagement2 + Engagement3 
EngagementFa2 =~ Engagement4 + Engagement5 + Engagement6  
EngagementFa3 =~ Engagement7 +  Engagement8 + Engagement9

BurnoutFa1 =~ Burnout1 + Burnout2 + Burnout3 + Burnout4 + Burnout6
BurnoutFa2 =~ Burnout8 + Burnout9 + Burnout14 + Burnout15 
BurnoutFa3 =~ Burnout5 +  Burnout7 + Burnout10 + Burnout11 + Burnout12 + Burnout16

Performance =~ Performance1 + Performance2 + Performance3 + Performance4 + Performance5
Turnover =~ Turnover1 + Turnover2 + Turnover3

Consci =~ ConsciPar1 + ConsciPar2 + ConsciPar3

#Structural model
WLB ~ PE_f
EngagementFa1 ~ PE_f 
EngagementFa2 ~ PE_f 
EngagementFa3 ~ PE_f 
BurnoutFa1 ~ PE_f 
BurnoutFa2 ~ PE_f 
BurnoutFa3 ~ PE_f 

Performance ~ WLB + EngagementFa1 + EngagementFa2 + EngagementFa3 + BurnoutFa1 + BurnoutFa2 + BurnoutFa3 + Gender + Part_time + Consci
Turnover ~ WLB + EngagementFa1 + EngagementFa2 + EngagementFa3 + BurnoutFa1 + BurnoutFa2 + BurnoutFa3 + Gender + Part_time + Consci
'
lv.1pe.fit <- sem(lv.1pe, data=df, missing="fiml")
summary(lv.1pe.fit, fit.measures=T, standardized=T) 

#Test that model for Remote vs. In-person: multigroup comparison
lv.1pe.fit.m <- sem(lv.1pe, data=df, missing="fiml", group="Context") #0 is Remote, 1 is In-person
summary(lv.1pe.fit.m, fit.measures=T, standardized=T)

#Constrain the regression coefficients and correlations in the model for Remote vs. In-person (simpler model) 
lv.1pe.fit.m.eq <- sem(lv.1pe, data=df, missing="fiml", group="Context", group.equal=c('regressions','lv.covariances')) #0 is Remote, 1 is In-person
summary(lv.1pe.fit.m.eq, fit.measures=T, standardized=T)

#Model comparison: does the more complex, freely estimated model across groups fit better?
anova(lv.1pe.fit.m, lv.1pe.fit.m.eq)

#Test a model for Remote vs. In-person: multigroup comparison with everything freely estimated for each group

lv.1pe.fit.m <- sem(lv.1pe, data=df, missing="fiml", group="Context") #0 is Remote, 1 is In-person
summary(lv.1pe.fit.m, fit.measures=T, standardized=T)

