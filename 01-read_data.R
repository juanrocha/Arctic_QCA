
library(tidyverse)
library(QCA)

## read data:

dat <- read_csv2(file = "data/QCA-AnalysisData-carla.csv") %>%
    janitor::clean_names()


# # Remember that the 1 means presence, 0 absence and 0.5 is maximum ambiguity.
# third <- subset(data, data$x2 != '')[,-c(1,2)] # this is the raw data
# 
# # the second tier is the median of the relevant variables on the rawdata
# second <- subset(data,data$x1 != '')[,-c(1,3)] # this is the median()of all chunks from the third tier. I calculated them by had in Excel (in 2016)
# 
# first <- subset(data, data$x!= '')[,-c(2,3)]

dat <- dat %>% filter(!is.na(x3)) %>%
    pivot_longer(4:last_col(), names_to = "case", values_to = "value") 

dat$value[dat$value == 0] <- -1
dat$value[is.na(dat$value)] <- 0

## third level
dat %>% select(-x1, -x2) 

## second level
dat %>% group_by(case, x2, x1) %>%
    summarize(score = sum(value))

## first level
df1 <- dat %>% group_by(case, x1) %>%
    summarize(score = sum(value))

## prep data for QCA:
df1 <- df1 %>%
    ungroup() %>% 
    pivot_wider(names_from = x1, values_from = score) %>%
    rename(KNO = 2, SFO = 3, NAV = 4, DIV = 5) %>%
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
        ))

## Make outcome variables mutually exclussive
df1 <- df1 %>%
    mutate(RES = as.numeric(OUT == 1), 
           TRA = as.numeric(OUT == 0.5), 
           FAI = as.numeric(OUT == 0)) 


save(df1, file = "data/cleaned_data.RData")
