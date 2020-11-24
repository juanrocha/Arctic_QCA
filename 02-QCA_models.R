library(tidyverse)
library(QCA)

load("data/cleaned_data.RData")

out <- df1$RES


## calibration
df1[, 2:5] <- df1[, 2:5] %>%
    map_df(calibrate, type = "fuzzy", thresholds= c(-0.5, 0, 0.5), ecdf=F)

df1 <- df1 %>% as.data.frame()

## Necessary conditions
necessity <- superSubset(
    data = df1, 
    outcome= out, 
    conditions= c('NAV', 'DIV', 'KNO', 'SFO'), 
    relation = "necessity", 
    incl.cut=0.75, cov.cut=0.5) 

# Parameters of fit for necessary conditions
pof(necessity$coms, df1, outcome = out, conditions= c('NAV', 'DIV', 'KNO', 'SFO'), neg.out = F) 
# parameters of fit for necessary conditions in the case of negated outcome
pof(necessity$coms, df1, outcome= out, conditions= c('NAV', 'DIV', 'KNO', 'SFO'), neg.out=T) 


## Sufficiency conditions
sufficiency <- superSubset(
    data = df1, 
    outcome= out, 
    conditions= c('NAV', 'DIV', 'KNO', 'SFO'), 
    relation = "sufficiency") 

sufficiency$coms
sufficiency$incl.cov

# Parameters of fit for sufficiency conditions
pof(sufficiency$coms, df1, outcome= out, conditions= c('NAV', 'DIV', 'KNO', 'SFO'), relation = 'suf', neg.out=F) 
pof(sufficiency$coms, df1, outcome= out, conditions= c('NAV', 'DIV', 'KNO', 'SFO'), relation = 'suf', neg.out=T) 


#### truth table  ####
tt <- truthTable(
    data = df1, outcome= "RES", 
    conditions= c('NAV', 'DIV', 'KNO', 'SFO'),  
    neg.out=FALSE, complete= TRUE, show.cases=FALSE, 
    sort.by= c('incl','n'), n.cut=1, decreasing=T,
    PRI=T)

tt

#### solutions ####
## boolean mnimization or parsimonious solution
ParSol <- eqmcc(tt, details=T, outcome= "RES", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), show.cases=T)

## complex solution
cs <- eqmcc(tt, details=T, show.cases=T, outcome= "RES", conditions= c('NAV', 'DIV', 'KNO', 'SFO'))

## intermediate solution
is <- eqmcc(tt, details=T, include='?', show.cases=T, direxp=c(1,1,1,1), outcome= out, conditions= c('NAV', 'DIV', 'KNO', 'SFO'))
# factorize(ParSol)

ParSol
cs
is
