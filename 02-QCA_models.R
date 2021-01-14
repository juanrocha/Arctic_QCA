library(tidyverse)
library(QCA)

load("data/cleaned_data.RData")

out <- df1$RES


## Correlograms:
GGally::ggpairs(df1, columns = 2:5)

# example with one variable
df1 %>% 
    mutate(K_c = calibrate (K, thresholds= "e = -2, c = 0.5, i = 2")) %>%
    ggplot(aes(K, K_c)) + geom_point(aes(color = as_factor(output))) 

# Make copies of the data frame
df0 <- df1 # unmodified copy
df2 <- df0 # df1 manual, df2 empirical distribution
df3 <- df0 # df3 totally fuzzy and relative "TFR"
df4 <- df0
## Calibration:
## Manual calibration with a S-shape function (logistic) with arbitrary thresholds.
## The thresholds selected are motivated by our coding where 0 is ambiguity, negative 
## values represent absence, and positive presence. 
df1[, 2:9] <- df0[, 2:9] %>%
    # this calibration takes into account thresholds of presence / absence, but
    # apply the same threshold to all variables.
    map_df(calibrate, type = "fuzzy", logistic = TRUE,
           thresholds= "e = -2, c = -0.5, i = 2", ecdf=F)

## Using empirical distributions
## truth table fails when 0.5 if a resulting value of the calibration
df2[, 2:9] <- df2[,2:9] %>%
    map_df(calibrate, logistic = FALSE, ecdf = TRUE,
           thresholds= "e = -3, c = -1, i = 3")

## Totally fuzzy and relative method.
df3[, 2:9] <- df3[,2:9] %>%
    map_df(calibrate, method = "TFR")

## crisp dataset (only 0,1)
df4[, 2:9] <- df4[,2:9] %>%
    map_df(calibrate,type = "crisp", threshold = 0.5)

df1 <- df1 %>% as.data.frame()

## Correlograms:
GGally::ggpairs(df1, columns = 2:5)

## J201215: The fuzzy set calibration with a logistic regression is the most promising approach.
## All others cluster most of cases (12) on the combination 0000 meaning lacking all attributes.


#### truth tables  ####
# Here I'm using the notation of CAP belongs, and cap for negation (~CAP), where cap is using capital letters

tt_RES <- truthTable(
    data = as.data.frame(df1), outcome= "RES", 
    conditions= c('N', 'D', 'K', 'S'),  
    neg.out= FALSE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.85)
tt_RES

tt_res <- truthTable(
    data = as.data.frame(df1), outcome= "RES", 
    conditions= c('N', 'D', 'K', 'S'),   
    neg.out= TRUE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.95)

tt_TRA <- truthTable(
    data = as.data.frame(df1), outcome= "TRA", 
    conditions= c('N', 'D', 'K', 'S'),  
    neg.out= FALSE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.95)

tt_tra <- truthTable(
    data = as.data.frame(df1), outcome= "TRA", 
    conditions= c('N', 'D', 'K', 'S'),  
    neg.out= TRUE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.95)

tt_FAI <- truthTable(
    data = as.data.frame(df1), outcome= "FAI", 
    conditions= c('N', 'D', 'K', 'S'),   
    neg.out= FALSE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.95)

tt_fai <- truthTable(
    data = as.data.frame(df1), outcome= "FAI", 
    conditions= c('N', 'D', 'K', 'S'),  
    neg.out= TRUE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.95)

tt_RES$minmat %>% apply(., 1, max)

tt_RES$minmat %>%
    as_tibble() %>%
    rownames_to_column("case") %>%
    pivot_longer(cols = 2:last_col(), 
                 names_to = "combination", values_to = "value") %>%
    ggplot(aes(value, group = combination)) +
    geom_density()


#### Necessary conditions ####
necessity <- superSubset(
    data = as.data.frame(df1), 
    outcome= "RES", 
    conditions= c('NAV', 'DIV', 'KNO', 'SFO'), 
    relation = "necessity", 
    incl.cut = 0.95) #

# Parameters of fit for necessary conditions
pof(necessity$coms, df1, outcome = "RES", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), neg.out = FALSE) 
# parameters of fit for necessary conditions in the case of negated outcome
pof(necessity$coms, df1, outcome= "RES", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), neg.out=TRUE) 


#### Sufficiency conditions ####

sufficiency <- superSubset(
    data = as.data.frame(df1), 
    outcome= "RES", 
    conditions= c('NAV', 'DIV', 'KNO', 'SFO'), 
    relation = "sufficiency", 
    incl.cut = 1) 

# sufficiency$coms
# sufficiency$incl.cov

# Parameters of fit for sufficiency conditions
pof(sufficiency$coms, df1, outcome= "RES", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), relation = 'suf', neg.out=FALSE) 
pof(sufficiency$coms, df1, outcome= "RES", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), relation = 'suf', neg.out=TRUE) 




#### solutions ####
## boolean mnimization or parsimonious solution
parsol <- minimize(
    tt_FAI, details = TRUE, 
    show.cases= TRUE)

## complex solution
cs <- minimize(tt_FAI, include = "?", details = TRUE)
    

## intermediate solution
is <- minimize(tt_RES, include = "?", details = TRUE, dir.exp = "1,1,1,1")
is <- minimize(tt_FAI, include = "?", details = TRUE, dir.exp = "0,0,0,0")    
    
# factorize(parsol)
# parsol$PIchart

parsol
cs
is
