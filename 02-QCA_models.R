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
    PRI=TRUE, incl.cut = 0.85)

tt_TRA <- truthTable(
    data = as.data.frame(df1), outcome= "TRA", 
    conditions= c('N', 'D', 'K', 'S'),  
    neg.out= FALSE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.85)

tt_tra <- truthTable(
    data = as.data.frame(df1), outcome= "TRA", 
    conditions= c('N', 'D', 'K', 'S'),  
    neg.out= TRUE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.85)

tt_FAI <- truthTable(
    data = as.data.frame(df1), outcome= "LoR", 
    conditions= c('N', 'D', 'K', 'S'),   
    neg.out= FALSE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.85)

tt_fai <- truthTable(
    data = as.data.frame(df1), outcome= "LoR", 
    conditions= c('N', 'D', 'K', 'S'),  
    neg.out= TRUE, complete= TRUE, show.cases=TRUE, dcc = TRUE,
    sort.by= c('incl','n'), n.cut=1, decreasing=TRUE,
    PRI=TRUE, incl.cut = 0.85)

tt_RES$minmat %>% apply(., 1, max)

tt_RES$minmat %>%
    as_tibble() %>%
    rownames_to_column("case") %>%
    pivot_longer(cols = 2:last_col(), 
                 names_to = "combination", values_to = "value") %>%
    ggplot(aes(value, group = combination)) +
    geom_density()

tt_list <- list(tt_RES, tt_res, tt_TRA, tt_tra, tt_FAI, tt_fai)
save(tt_list, file = "data/truth_tables.RData")

#### Necessary conditions ####
necessity <- purrr::map(
    .x = c("RES", "TRA", "LoR"), 
    function(x) {
        superSubset(
            data = as.data.frame(df1), 
            outcome= x, 
            conditions= c('N', 'D', 'K', 'S'), 
            relation = "necessity", 
            incl.cut = 0.95)}
    ) #
## Necesity for the negation of the output. But the result is not exactly the same as the 
## pof of the net.out = TRUE. which is in fact what I need.
non_necessity <- purrr::map(
    .x = c("RES", "TRA", "LoR"), 
    function(x) {
        superSubset(
            data = as.data.frame(df1), 
            outcome= paste0("~", x), 
            conditions= c('N', 'D', 'K', 'S'), 
            relation = "necessity", 
            incl.cut = 0.95)}
) #

# Parameters of fit for necessary conditions
# The function on the outcome = TRUE (neg.out = FALSE) results on the same table as 
# `necessity` above. What we need to know is if the scores are lower for the negation.
# But the way `pof` is written does not accept purrr verbs. So I need to do it manually.
pof(necessity[[1]]$coms, df1, outcome = "RES", conditions= c('N', 'D', 'K', 'S'), neg.out = FALSE) 

# parameters of fit for necessary conditions in the case of negated outcome

pof_necesity <- list()
outs <- c("RES", "TRA", "LoR")


pof_necesity[[1]] <- pof(necessity[[1]]$coms, df1, outcome= "RES", 
    conditions = c('N', 'D', 'K', 'S'), neg.out=TRUE) 
pof_necesity[[2]] <- pof(necessity[[1]]$coms, df1, outcome= "TRA", 
                         conditions= c('N', 'D', 'K', 'S'), neg.out=TRUE) 
pof_necesity[[3]] <- pof(necessity[[1]]$coms, df1, outcome= "LoR", 
                         conditions= c('N', 'D', 'K', 'S'), neg.out=TRUE) 
## doesn't work
# pof_necesity <- purrr::map2(
#     .x = outs,
#     .y = necessity,
#     .f = function(x,y){
#         pof(y$coms, df1, outcome = x, 
#             conditions= c('N', 'D', 'K', 'S'), neg.out=TRUE)
#     }
# )


necs_df <- purrr::map(necessity, function(x) x$incl.cov)


necs_df <- purrr::pmap(
    list(
        necs_df,
        c("Resilience", "Transformation", "Loss of resilience"),
        rep("True", 3)),
    .f = function(x,y,z) {
        x$variable <- y
        x$type <- z
        x <- x %>%
            rownames_to_column(var = "comb")
        return(x)
    }
) %>% bind_rows()  

necs_df2 <- purrr::map(pof_necesity, function(x) x$incl.cov)

necs_df2 <- purrr::pmap(
    list(
        necs_df2,
        c("Resilience", "Transformation", "Loss of resilience"),
        rep("False", 3)),
    .f = function(x,y,z) {
        x$variable <- y
        x$type <- z
        x <- x %>%
            rownames_to_column(var = "comb")
        return(x)
    }
)   %>% bind_rows() 

necs_df <- bind_rows(necs_df, necs_df2)



#### Sufficiency conditions ####

sufficiency <- purrr::map(
    .x = c("RES", "TRA", "LoR"), 
    function(x) {
        superSubset(
            data = as.data.frame(df1), 
            outcome= x, 
            conditions= c('N', 'D', 'K', 'S'), 
            relation = "sufficiency", 
            incl.cut = 0.95)}
) #

# sufficiency$coms
# sufficiency$incl.cov

# Parameters of fit for sufficiency conditions
# pof(sufficiency$coms, df1, outcome= "RES", conditions= c('N', 'D', 'K', 'S'),
#     relation = 'suf', neg.out=FALSE) 
# pof(sufficiency$coms, df1, outcome= "RES", conditions= c('N', 'D', 'K', 'S'), 
#     relation = 'suf', neg.out=TRUE) 
pof_suff <- list()
outs <- c("RES", "TRA", "LoR")

pof_suff[[1]] <- pof(sufficiency[[1]]$coms, df1, outcome= "RES", relation = 'suf',
                         conditions = c('N', 'D', 'K', 'S'), neg.out=TRUE) 
pof_suff[[2]] <- pof(sufficiency[[1]]$coms, df1, outcome= "TRA", relation = 'suf',
                         conditions= c('N', 'D', 'K', 'S'), neg.out=TRUE) 
pof_suff[[3]] <- pof(sufficiency[[1]]$coms, df1, outcome= "LoR", relation = 'suf',
                         conditions= c('N', 'D', 'K', 'S'), neg.out=TRUE) 

suff_df <- purrr::map(sufficiency, function(x) x$incl.cov)

suff_df <- purrr::pmap(
    list(
        suff_df,
        c("Resilience", "Transformation", "Loss of resilience"),
        rep("True", 3)),
    .f = function(x,y,z) {
        x$variable <- y
        x$type <- z
        x <- x %>%
            rownames_to_column(var = "comb")
        return(x)
    }
)   %>% bind_rows() 

suff_df2 <- purrr::map(pof_suff, function(x) x$incl.cov)
suff_df2 <- purrr::pmap(
    list(
        suff_df2,
        c("Resilience", "Transformation", "Loss of resilience"),
        rep("False", 3)),
    .f = function(x,y,z) {
        x$variable <- y
        x$type <- z
        x <- x %>%
            rownames_to_column(var = "comb")
        return(x)
    }
)   %>% bind_rows() 

suff_df <- bind_rows(suff_df, suff_df2)



#### solutions ####
## boolean mnimization or parsimonious solution
parsol <- list(tt_RES, tt_TRA, tt_FAI) %>%
    purrr::map(function(x){
        minimize(x, details = TRUE)
    })

parsol_df <- parsol %>%
    purrr::map(.f = function(x){x$IC$overall$incl.cov})

## J210413: On the new analysis with 39 cases there is two solutions for resilience. I breaks the code
## because the strucutre of the resulting list is different. A manual solution below

parsol_df[[1]] <- parsol[[1]]$IC$incl.cov

parsol_df %>%
    purrr::map2(.x = ., .y = c("Resilience", "Transformation", "Loss of resilience"),
                .f = function(x,y) {x$type <- y; return(x)}) %>%
    purrr::map(function(x) rownames_to_column(x, "comb")) %>% 
    bind_rows() %>%
    select(-cases) %>% 
    # rownames_to_column("comb") %>%
    pivot_longer(inclS:covU, names_to = "statistic", values_to = "value") %>% 
    ggplot(aes(value, comb)) +
    geom_point() +
    facet_grid(type ~ statistic, scales = "free") +
    theme_light()


## complex solution
cs <- minimize(tt_RES, include = "?", details = TRUE)
    

## intermediate solution
is <- minimize(tt_RES, include = "?", details = TRUE, dir.exp = "1,1,1,1")
is <- minimize(tt_FAI, include = "?", details = TRUE, dir.exp = "0,0,0,0")    
    
# factorize(parsol)
# parsol$PIchart
parsol
cs
is


save(suff_df, necs_df, parsol_df, sufficiency, necessity, parsol,
     file = "data/resultsQCA.RData")
