library(tidyverse)
library(QCA)

source("01-read_data.R")


## second level
keys <- df_2tier %>% 
    mutate(x2 = as_factor(x2), x1 = as_factor(x1)) %>%
    group_by(x1, x2) %>%
    group_keys()

keys$code <- c(
    paste("SFO", 1:4, sep= "_"),
    paste("DIV", 1:3, sep= "_"),
    paste("NAV", 1:3, sep= "_"),
    paste("KNO", 1:3, sep= "_")
)

df_2tier <-  df_2tier %>%
    mutate(x2 = as_factor(x2), x1 = as_factor(x1)) %>%
    left_join(keys) %>%
    ungroup() %>%
    select(-x1, -x2) %>%
    pivot_wider(names_from = code, values_from = score) %>% 
    mutate(OUT = c(
        0.5, # Alaska transformation
        1,   # Bering strait shipping
        0.5, # Cape dorset
        1,   # Dempster Caribou
        1,   # Finmark fishing
        0,   # Finmark reindeer
        0,   # food security nunavut // changed from original suggested from Garry
        0,   # Greenland Disco bay
        0,   # Greenland mobility
        0,   # Greenland Paamiut cod
        0.5, # Greenalnd Sisimiut_cod
        1,   # Hamerfest coastal community (Naatamo?)
        0,   # Iceland herring collapse
        1,   # Kiruna
        0.5, # metal mining finland
        1,   # Moth outbreaks
        1,   # Naataamo
        0,   # NewFoundland cod
        0,   # Newfoundland seal hunt
        0,   # Newtok
        1,   # Pangnirtum_fisheries
        1,   # Savoonga_Alaska
        0,   # Shipping barrens
        0,   # Swedish reindeer
        0,   # Teriberka
        0.5,   # Ulukkaktok
        0,   # Uumannaq_Greenland
        0.5, # Whaling_Iceland
        0,   # Yakutia
        1   # Yamal
    )) %>%
    mutate(RES = as.numeric(OUT == 1), 
           TRA = as.numeric(OUT == 0.5), 
           FAI = as.numeric(OUT == 0)) 


#### Calibration ####

df_2tier[, 2:18] <- df_2tier[, 2:18] %>%
    map_df(calibrate, type = "fuzzy", logistic = TRUE,
           thresholds= "e = -2, c = -0.5, i = 2", ecdf=F)
## Correlograms:
GGally::ggpairs(df_2tier, columns = 2:18)

#### Truth table ####
tt_RES <- truthTable(
    data = as.data.frame(df_2tier), outcome= "RES", 
    conditions= keys$code,  
    neg.out= FALSE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.95)
tt_RES



#### Sufficiency conditions ####
sufficiency <- superSubset(
    data = as.data.frame(df_2tier), 
    outcome= "RES", 
    conditions= keys$code, 
    relation = "sufficiency", 
    incl.cut = 1) 

# sufficiency$coms
# sufficiency$incl.cov

# Parameters of fit for sufficiency conditions
pof(sufficiency$coms, df_2tier, outcome= "FAI", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), relation = 'suf', neg.out=F) 
pof(sufficiency$coms, df_2tier, outcome= "FAI", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), relation = 'suf', neg.out=T) 

#### Necessary conditions ####
necessity <- superSubset(
    data = as.data.frame(df_2tier), 
    outcome= "RES", 
    conditions= keys$code, 
    relation = "necessity", 
    incl.cut = 0.95) #

# Parameters of fit for necessary conditions
pof(necessity$coms, df_2tier, outcome = "RES", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), neg.out = F) 
# parameters of fit for necessary conditions in the case of negated outcome
pof(necessity$coms, df_2tier, outcome= "RES", conditions= c('NAV', 'DIV', 'KNO', 'SFO'), neg.out=T) 


#### solutions ####
## boolean mnimization or parsimonious solution
parsol <- minimize(
    tt_RES, details = TRUE, 
    show.cases= TRUE)

## complex solution
cs <- minimize(tt_RES, include = "?", details = TRUE)


## intermediate solution
# is <- minimize(tt_RES, include = "?", details = TRUE, dir.exp = "1,1,1,1")


factorize(parsol)

parsol$PIchart
cs
is

### J201219: While the algorithm finishes and minimises at least the parsimonious
### solution, the results are very hard to interpret. With 13 conditions, there are
### 2^13  = 8192 combinations of causal conditions, and the observed 30 cases only cover 
### 24 of the solution space (see dim(tt_RES$minmat)). Continue the analysis with the
### simpler version with 4 conditions.