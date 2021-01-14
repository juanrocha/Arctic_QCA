
library(tidyverse)
library(QCA)

## read data:
## 30 cases version
dat <- read_csv2(file = "data/QCA-AnalysisData-carla.csv") %>%
    janitor::clean_names()


## current version in drive with 34 cases
dat <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1qX_Eg1wdExa0SyV2sqR-8DfHoD_6KfEbrGqxkkmO-ZA/edit#gid=1714451352") %>%
    rename(x1 = 1, x2 = 2, x3 = 3)

coords <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1qX_Eg1wdExa0SyV2sqR-8DfHoD_6KfEbrGqxkkmO-ZA/edit#gid=1573239536",
    sheet = 2)

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
    rename(K = 2, S = 3, N = 4, D = 5) %>%
    left_join(
        coords %>% select(short_name, output),
        by = c("case" = "short_name")
    ) %>% ## Make outcome variables mutually exclussive
    mutate(RES = as.numeric(output == 1), 
           TRA = as.numeric(output == 0.5), 
           LoR = as.numeric(output == 0)) 


save(df1, file = "data/cleaned_data.RData")
save(coords, file = "data/coords.RData")
save(dat, file = "data/cleaned_df.RData")
## second level:
## second level
df_2tier <- dat %>% group_by(case, x2, x1) %>%
    summarize(score = sum(value)) 

#### old code ####
#### 
#### %>% 
# mutate(OUT = c(
#     0.5, # Alaska transformation
#     1,   # Bering strait shipping
#     0.5, # Cape dorset
#     1,   # Dempster Caribou
#     1,   # Finmark fishing
#     0,   # Finmark reindeer
#     0,   # food security nunavut // changed from original suggested from Garry
#     0,   # Greenland Disco bay
#     0,   # Greenland mobility
#     0,   # Greenland Paamiut cod
#     0.5, # Greenalnd Sisimiut_cod
#     1,   # Hamerfest coastal community (Naatamo?)
#     0,   # Iceland herring collapse
#     1,   # Ikpiarkjuk (R)
#     1,   # Kiruna
#     0.5, # metal mining finland
#     1,   # Moth outbreaks
#     1,   # Naataamo
#     0,   # NewFoundland cod
#     0,   # Newfoundland seal hunt
#     0,   # Newtok
#     1,   # Pangnirtum_fisheries
#     1,   # Savoonga_Alaska
#     0,   # Shipping barrens
#     0,   # Swedish reindeer
#     0,   # Teriberka
#     0.5,   # Ulukkaktok
#     0,   # Uumannaq_Greenland
#     0.5, # Whaling_Iceland
#     0.5, # Upernavik (T)
#     0,   # Yakutia
#     0,   # Yakutia(LoR)
#     1   # Yamal
# ))