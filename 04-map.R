library(tidyverse)
library(maps)
library(mapproj)
library(ggrepel)


coords <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1qX_Eg1wdExa0SyV2sqR-8DfHoD_6KfEbrGqxkkmO-ZA/edit#gid=1573239536",
    sheet = 2)

coords <- coords %>%
    mutate(type = case_when(
        output == 1 ~ "Resilience", 
        output == 0.5 ~ "Transformation",
        output == 0 ~ "Loss of resilience"
        ), 
        type = as_factor(type))

world <- map_data("world")
arctic_map <- ggplot(world, aes(x = long, y = lat)) +
    geom_path(aes(group = group), size = 0.1, color = "grey50") +
    coord_map(projection = "orthographic", orientation = c(90,0,0)) +
    ylim(45,90) +
    geom_point(
        data = coords,
        aes(x = lon, y = lat, color = type, size = pop_size),
        alpha = 0.5
    ) + 
    geom_text_repel(data = coords, aes(lon, lat, label = `Case name`), size = 2) +
    scale_color_manual(
        name = "Case", values = c("#3E97F7", "#8829F3", "#EB4891"),
        guide = guide_legend(title.position = "top")) + # or nice orange "#FF7F00"
    scale_size("Population") +
    theme_void(base_size = 10) +
    theme(legend.position = c(0.2, 0.1), legend.key.size = unit(0.5, "cm"), 
          legend.direction = "vertical", legend.box = "horizontal")

arctic_map
