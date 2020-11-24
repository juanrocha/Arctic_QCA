library(tidyverse)


## third level
dat %>% select(-x1, -x2) %>%
    ggplot(aes(x3, case)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis_c() +
    theme_light() +
    theme(axis.text.x = element_blank())


## second level
dat %>% group_by(case, x2, x1) %>%
    summarize(score = sum(value))  %>%
    ggplot(aes(x2, case)) +
    geom_tile(aes(fill = score)) +
    scale_fill_viridis_c() +
    theme_light() +
    theme(axis.text.x = element_text(angle=90))

## first level
dat %>% group_by(case, x1) %>%
    summarize(score = sum(value))%>%
    ggplot(aes(x1, case)) +
    geom_tile(aes(fill = score)) +
    scale_fill_viridis_c() +
    theme_light() +
    theme(axis.text.x = element_text(angle=90))
