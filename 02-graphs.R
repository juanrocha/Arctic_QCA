library(tidyverse)
library(maps)
library(mapproj)
library(ggrepel)
library(gplots)
library(patchwork)
library(gridGraphics)
library(pheatmap)
library(png)
library(QCA)
# setwd("~/Documents/Projects/MARAT/Arctic_QCA")

## Produced with 01-read_data.R
load('data/coords.Rdata')
load('data/cleaned_data.RData')
load('data/cleaned_df.RData')
load('data/resultsQCA.RData')
load("data/truth_tables.RData")


#### Fig 1: Map ####
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
    geom_text_repel(
        data = coords %>%
            mutate(`Case name` = str_remove_all(`Case name`, " \\[R\\]| \\[T\\]| \\[L\\]")) ,
        aes(lon, lat, label = `Case name`),
        stat = "identity", position = "identity", size = 1.75,
        min.segment.length = 0.5, max.overlaps = 15, box.padding = 0,
        segment.size = 0.35) +
    scale_color_manual(
        name = "Case", values = c("#377EB8", "#984EA3", "#FF7F00"),
        guide = guide_legend(title.position = "top")) + # or nice orange "#FF7F00"
    scale_size("Population", breaks = c(300, 2000, 18000), range = c(0.25,2)) +
    theme_void(base_size = 6) +
    # labs(tag = "A") +
    theme(legend.position = c(0.2, 0.1), legend.key.size = unit(0.25, "cm"),
          legend.direction = "vertical", legend.box = "horizontal",
          plot.tag.position = c(0.01,0.95), legend.title = element_text(size = 5),
          legend.text = element_text(size = 5))
          #panel.background = element_rect(fill = "#c6e2ff"),
          #legend.box.background = element_rect(fill = "white"))


##
#quartz(width = 3.5, height = 3.5)
arctic_map

ggsave(filename = "fig1_arctic_map.png", path = "paper/figures/",
       plot = arctic_map, width = 4, height = 4, device = "png",
       dpi = 600)

#### Fig 2: descriptive stats ####

## time series
tseries <- coords %>%
  separate(time_period, c("start", "end"), sep="-") %>% 
  select(short_name, start, end) %>%
  mutate(short_name = str_remove(short_name, "_L|_T|_R")) %>%
  pivot_longer(start:end, names_to = "when", values_to = "year") %>%
  filter(!is.na(year)) %>% 
  mutate(year = as.numeric(year)) 

pd <- tseries %>% 
  ggplot(aes(year, short_name)) +
    geom_point(color = "midnightblue", size = 0.7) +
    geom_line(aes(group = short_name), color = "midnightblue", size = 0.1) + 
    geom_text(
      data = tseries %>% filter(when == "start"),
      aes(year, short_name, label = short_name),
      hjust = 1, size = 1.25, nudge_x = -5, color = "midnightblue") +
  labs(tag = "D", x = "Year") + xlim(1550, NA) +
    theme_classic(base_size = 6) + 
    theme(axis.text.y = element_blank(), axis.ticks.length.y = unit(0,"mm"), 
          axis.line.y = element_blank(), axis.title.y = element_blank())
  
pa <- coords %>%
  mutate(country = as_factor(country),
         country = fct_relevel(
           .f = country, 
           levels =table(coords$country) %>% sort() %>% names() )) %>% 
  ggplot(aes(country)) + 
  geom_bar(aes(fill = type), alpha = .8) +
  scale_y_continuous(breaks = c(0,5,10)) +
  labs(y = "Cases", x = "Country", tag = "A") +
  coord_flip() +
  scale_fill_manual(
    name = "Case type", values = c("#377EB8", "#984EA3", "#FF7F00"),
    guide = guide_legend(title.position = "top")) +
  theme_light(base_size = 6) +
  theme(legend.position = c(0.7, 0.2), legend.key.size = unit(0.25, "cm"))

pb <- coords %>%
  mutate(Sector = str_split(Sector, pattern = "; ")) %>%
  select(Sector) %>%
  unnest(cols = c(Sector)) %>%
  mutate(Sector = str_replace(Sector, 
    pattern = "Caribou management and road infraestructure",
    replacement = "Caribou management\n and road infraestructure")) %>% 
  group_by(Sector) %>% tally() %>%
  ggplot(aes(reorder(Sector, n), n)) +
  geom_col(fill= "grey24", alpha = 0.8) +
  labs(y = "Cases", x = "Sectors", tag = "B") +
  coord_flip() +
  theme_light(base_size = 6)

pc <- coords %>%
  select(Climate:`Other socio-economic`) %>%
  pivot_longer(1:last_col(), names_to = "driver", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(driver) %>%
  summarize(n = sum(value)) %>% 
  ggplot(aes(reorder(driver, n), n)) +
  geom_col(fill= "grey24", alpha = 0.8) +
  labs(y = "Cases", x = "Drivers", tag = "C") +
  coord_flip() +
  theme_light(base_size = 6)

#arctic_map | 

(pa + pb + pc) / pd

ggsave(
  filename = "fig2_stats.png", path = "paper/figures/",
  plot = last_plot(), width = 7, height = 3.5, dpi = 300,
  device = "png"
)




#### Fig 3: dendrogram instead of table ####
library(ggraph)
library(igraph)
edge_list <- dat %>%
    select(from = x1, to = x2) %>% mutate(hjust = 1, ref = "Berkes et al") %>%
    bind_rows(
        dat %>% select(from = x2, to = x3) %>%
            mutate(hjust = 0, ref = "Current")) %>%
    unique() %>%
    add_row(from = "Adaptive capacity",
            to = c("Navigating change & uncertainty",
                   "Nurturing diversity for reorganzation & renewal",
                   "Combing different types of knowledge\n for learning",
                   "Creating opportunities for self-organization"),
            hjust = 1, ref = "Berkes et al") %>%
    mutate(
        from = str_replace_all(string = from,
            pattern = "Combing different types of knowledge for learning",
            replacement = "Combing different types of knowledge\n for learning"
        )
    )

g <- igraph::graph_from_edgelist(as.matrix(edge_list[,1:2]))
E(g)$ref <- edge_list$ref
vertex_attr(g, "hjust", index = V(g)) <- c(rep(0.5, 17), rep(0, (88-17)), 1)
# vertical adjustment didn't work, it does not follow the ggplot standard
#V(g)$vjust <- c(rep(1,4), rep(0, 13), rep(0.5, (89-17)))

## Looks cluttered, no idea how to make it readable with such a long text for lower
## level categories. Will have to be a boring table.
dend <- ggraph(g, "dendrogram") +
    geom_edge_diagonal(aes(colour = ref ), edge_width = 0.1) +
    geom_node_text(
        aes(label = name, hjust = hjust),
        size = 1.5) +
    coord_flip() +
    ylim(3.3,-2) +
    scale_edge_colour_manual(values = c("#FF7F00", "#377EB8")) +
    theme_void(base_size = 8) +
    theme(legend.position = c(0.1, 0.1), legend.title = element_blank(),
          legend.key.size = unit(0.25,"cm"), legend.text = element_text(size = 6))

dend

ggsave(
    filename = "vars_dendrogram.png", path = "paper/figures",
    plot = dend, width = 7, height = 4, dpi = 300,
    device = "png"
)

#### Fig 4: calibration ####
cal <- df1 %>% 
  select(case, K, S, N, D) %>%
  pivot_longer(K:D, names_to = "feature", values_to = "score") %>%
  group_by(feature) %>%
  mutate(calibrated = calibrate(score, thresholds= "e = -2, c = 0.5, i = 2")) %>%
  separate(case, into = c("case", "output"), sep = "_") %>%
  mutate(output = case_when(
    output == "R" ~ "Resilience",
    output == "T" ~ "Transformation",
    output == "L" ~ "Loss of resilience"
  ), output = as_factor(output)) %>% 
  ggplot(aes(score, calibrated)) +
  geom_point(aes(color = output), size = 0.8, alpha = 0.8) +
  facet_wrap(~feature) +
  labs(x = "Raw score", y = "Calibrated score") +
  scale_color_manual(
    name = "Case", values = c("#FF7F00", "#984EA3", "#377EB8")) +
  theme_light(base_size = 6) +
  theme(legend.position = "top")

cal

ggsave(filename = "calibration.png", path = "paper/figures/",
       plot = cal, width = 3, height = 3.75, dpi = 300,
       device = "png")

#### heatmaps ####

## first level
df_1tier <- dat %>%
  group_by(case, x1) %>%
  summarize(score = sum(value))

mat <- df_1tier %>%
  pivot_wider(
    names_from = case,
    values_from = score) %>%
  select(-x1) %>%
  as.matrix()
rownames(mat) <- c("K", "S", "N", "D") #df_1tier$x1 %>% unique()

sidebar <- rep(0,nrow(coords))
sidebar[colnames(mat) %>% str_detect("_R")] <- "#377EB8"
sidebar[colnames(mat) %>% str_detect("_T")] <- "#984EA3"
sidebar[colnames(mat) %>% str_detect("_L")] <- "#FF7F00"

colnames(mat) <- str_remove(colnames(mat), "_L|_T|_R")

quartz(width = 3, height = 3.75, pointsize = 5) # for heatmap


heatmap.2(
  x = t(mat),
  Rowv=T,
  Colv=T,
  trace='none',
  denscol = "black",
  density.info = "histogram",
  #densadj = 1,
  symkey = TRUE,
  # main='First tier',
  # ylab='Cases',
  xlab ='Adaptive capacity features',
  keysize = 1.25, key = TRUE,
  key.title = "Score", key.xlab = NA, key.ylab = NA,
  #key.xtickfun = 0, key.ytickfun = 0,
  key.par=list(
    cex.main= 0.8, cex = 1, cex.axis = 0.8,
    cex.lab = 1, lwd = 0.1, mar=c(2,2,4,1), las = 1,
    lab = c(2,2,2), ps = 7),
  margins=c(5, 15), cexRow = 1.5, cexCol = 2, srtCol = 0,
  col = colorspace::diverge_hcl(20, palette = "Blue-Red 2", rev = TRUE),
  RowSideColors = sidebar
  # ColSideColors = col_color
)

legend(
  x = 0.67, y = 1.01, pt.cex = 1.2, xjust = 0, #y.intersp = 0.2,
  fill=c("#377EB8","#984EA3","#FF7F00"),
  legend=c("Resilient", 'Transformation', 'Resilience loss'),
  bty='n', cex= 1.2, border = FALSE)

# quartz.save(
#   file = "paper/figures/heatmap.png", type = "png",
#   width = 3, height = 3.75, pointsize = 5, dpi = 300)


df_colors  <- coords %>%
  mutate(short_name = str_remove(short_name, "_L|_T|_R"),
         short_name = as.factor(short_name)) %>%
  select(short_name, type) %>%
  column_to_rownames(., var = "short_name")

rownames(df_colors) <- colnames(mat)

## Alternative not used.
# pheatmap(
#     mat = t(mat),
#     color = colorspace::diverge_hcl(20, palette = "Blue-Red 2", rev = TRUE),
#     show_colnames = TRUE,
#     show_rownames = TRUE,
#     border_color = NA,
#     clustering_distance_rows = "correlation",
#     # cluster_rows = TRUE,
#     # cluster_cols = TRUE,
#     # clustering_method = "centroid",
#     legend = FALSE,
#     annotation_row = df_colors,
#     annotation_colors = list(
#         type = c(Resilience = "#377EB8",
#                  Transformation = "#984EA3",
#                  `Loss of resilience` = "#FF7F00")
#     ), angle_col = 0,
#     fontsize = 5
#     #filename = "paper/figures/heatmap.png", width = 3, height = 3, dpi = 500
# )


quartz(width = 7, height = 4, pointsize = 5) # combined
hm <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
  labs(tag = "A") +
  theme_void(base_size = 6) + theme(plot.margin = margin(0,0,0,0)) + 
  annotation_custom(
    grob = grid::rasterGrob( image = readPNG(
      "paper/figures/heatmap.png"), interpolate = TRUE),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) 

## to make them align I need to reimport as png unfortunately
cal2 <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
  labs(tag = "B") +
  theme_void(base_size = 6) + theme(plot.margin = margin(0,0,0,0)) + 
  annotation_custom(
    grob = grid::rasterGrob( image = readPNG(
      "paper/figures/calibration.png"), interpolate = TRUE),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) 

hm + cal2 + plot_layout(widths = c(3, 3))

ggsave(
    filename = "fig3_calibration_heatmap.png", path = "paper/figures/",
    plot = last_plot(), width = 6, height = 3.5, dpi = 300,
    device = "png"
)



## truth tables
extract_truthtable <- function(x){
  x <- x$tt %>%
    as_tibble() %>%
    select(-cases) %>%
    mutate(OUT = str_replace(string = OUT, pattern = "\\?", replacement = "NA") %>% as.numeric(),
           incl = str_replace(incl, "-", "NA") %>% as.numeric(),
           PRI = str_replace(PRI, "-", "NA") %>% as.numeric(),
           id = row_number()) %>%
    mutate(N = ifelse(N == 1, "N", "n"),
           D = ifelse(D == 1, "D", "d"),
           K = ifelse(K == 1, "K", "k"),
           S = ifelse(S == 1, "S", "s")) %>%
    mutate(comb = paste0(N,D,K,S), .keep = "unused") 
  
  return(x)
}


extract_truthtable(tt_list[[2]])

tt1 <- tt_list %>% 
  purrr::map(., extract_truthtable)
  
## barplots: checking back again, the number of cases per combination is the same across all truth tables
## so I only need one graph. For visualization, it's probably better to have NA removed, or?
g1 <- tt1 %>%
  purrr::map(., function(x) {
    p <- ggplot(x, aes(n, comb)) + 
      geom_col(fill = "orange", color = "orange", alpha = 0.8) +
      scale_x_continuous(position = "top", breaks = c(0,5,10)) +
      labs(y = "", x = "Number of cases") +
      theme_classic(base_size = 8) +
      theme(axis.text.y = element_blank())
    return(p)
  })

g1 <- tt1[[1]] %>% #filter(n>0) %>%
  ggplot( aes(reorder(comb, n), n)) + 
  geom_col(fill = "orange", color = "orange", alpha = 0.8) +
  scale_y_continuous(breaks = c(0,5,10)) +
  labs(x = "Logical combinations", y = "Number of cases", tag = "A") +
  coord_flip() +
  theme_light(base_size = 8)
g1

tt2 <- pmap(list(x = c("True", "False", "True", "False", "True", "False"), 
            y = c(rep("Resilience",2), rep("Transformation", 2), rep ("Loss of resilience", 2)),
            z = tt1),
       .f = function(x,y,z){
         z$response <- x
         z$type <- y
         return(z)
       }) %>%
  bind_rows()

g2 <- tt2 %>% select(-n, -id) %>% 
  pivot_longer(OUT:PRI, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>% 
  mutate(variable = as_factor(variable), 
         response = as_factor(response),
         type = as_factor(type)) %>%
  ggplot(aes(variable, comb)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2(midpoint = 0.5, na.value = "gray84") +
  labs(x = "Statistic", y = "Logical combinations", tag = "B") +
  facet_grid(response~type) +
  theme_light(base_size = 8) +
  theme(legend.position = "bottom", legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.5, "cm"), legend.text = element_text(size = 5))


g1 + g2 + plot_layout(widths = c(2.5, 3), heights = 3)


ggsave(
  filename = "fig4_truth_tables.png", path = "paper/figures/",
  plot = last_plot(), width = 6, height = 3, dpi = 300,
  device = "png"
)

#### Necessary and Sufficient conditions ####

p1 <- suff_df %>%
  pivot_longer(inclS:covS, names_to = "statistic", values_to = "value") %>% 
  mutate(comb = str_replace_all(comb, pattern = "~D", "d"),
         comb = str_replace_all(comb, pattern = "~K", "k"),
         comb = str_replace_all(comb, pattern = "~S", "s"),
         comb = str_replace_all(comb, pattern = "~N", "n")) %>%
  ggplot(aes(value, comb)) +
  geom_point(aes(color = type), size = 0.75, alpha = 1) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_color_manual("Type", values = c("#ef8a62", "#67a9cf")) +
  labs(x = "Value", y = "Sufficient conditions", tag = "A") +
  facet_grid( as_factor(variable) ~ statistic, scales = "free_y") +
  theme_light(base_size = 6)+ theme(legend.position = "top")


p2 <- necs_df %>%
  pivot_longer(inclN:covN, names_to = "statistic", values_to = "value") %>% 
  mutate(comb = str_replace_all(comb, pattern = "~D", "d"),
         comb = str_replace_all(comb, pattern = "~K", "k"),
         comb = str_replace_all(comb, pattern = "~S", "s"),
         comb = str_replace_all(comb, pattern = "~N", "n")) %>%
  ggplot(aes(value, comb)) +
  geom_point(aes(color = type), size = 0.75, alpha = 1) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_color_manual("Type", values = c("#ef8a62", "#67a9cf")) +
  labs(x = "Value", y = "Necessary conditions", tag = "B") +
  facet_grid(as_factor(variable) ~ statistic, scales = "free_y") +
  theme_light(base_size = 6) + theme(legend.position = "top")

p1 + p2 + plot_layout(widths = c(3,3), heights = 3)

ggsave(
  filename = "fig5_necessary_and_sufficient.png", path = "paper/figures/",
  plot = last_plot(), width = 6, height = 3, dpi = 300,
  device = "png"
)

#### Parsimonious solutions ####

parsol_df %>%
  purrr::map2(.x = ., .y = c("Resilience", "Transformation", "Loss of resilience"),
              .f = function(x,y) {x$type <- y; return(x)}) %>%
  purrr::map(.x =., .f = function(x) rownames_to_column(x, "comb")) %>% 
  bind_rows() %>%
  select(-cases) %>% 
  mutate(comb = str_replace_all(comb, pattern = "~D", "d"),
         comb = str_replace_all(comb, pattern = "~K", "k"),
         comb = str_replace_all(comb, pattern = "~S", "s"),
         comb = str_replace_all(comb, pattern = "~N", "n")) %>%
  # rownames_to_column("comb") %>%
  pivot_longer(inclS:covU, names_to = "statistic", values_to = "value") %>% 
  ggplot(aes(value, comb)) +
  geom_point() +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  facet_grid(as_factor(type ) ~statistic , scales = "free_y") +
  labs(y = "Solution terms", x = "Value") +
  theme_light(base_size = 7)

ggsave(
  filename = "fig6_solutions.png", path = "paper/figures/",
  plot = last_plot(), width = 4, height = 3, dpi = 300,
  device = "png"
)








 #### SM figs ####

## Correlograms:
sm1 <- df1 %>% 
  # mutate(outcome = case_when(
  #   output == 0 ~ "Resilience loss",
  #   output == 0.5 ~ "Transformation",
  #   output == 1 ~ "Resilience"
  # )) %>% 
  GGally::ggpairs(
    ., columns = c(2:5),
    lower = list(continuous = GGally::wrap("points", alpha = 0.75, size = 1)),
    upper = list(continuous = GGally::wrap(GGally::ggally_cor, display_grid = FALSE, size = 2)),
    diag = list(continuous = GGally::wrap("densityDiag", alpha = 0.25))
    ) +
  theme_light(base_size = 6)

sm1
ggsave(
  filename = "sm1_correlogram.png", path = "paper/figures/",
  plot = last_plot(), width = 4, height = 3, dpi = 300,
  device = "png"
)

#### sm2: heatmap 2nd-tier ####

df_2tier <- df_2tier %>% 
  select(-x1) %>% 
  pivot_wider(names_from = x2, values_from = score)

case_name <- df_2tier$case
  
mat2 <- as.matrix(df_2tier[,-1])
rownames(mat2) <- case_name

sidebar <- rep(0,nrow(coords))
sidebar[rownames(mat2) %>% str_detect("_R")] <- "#377EB8"
sidebar[rownames(mat2) %>% str_detect("_T")] <- "#984EA3"
sidebar[rownames(mat2) %>% str_detect("_L")] <- "#FF7F00"

rownames(mat2) <- str_remove(rownames(mat2), "_L|_T|_R")

quartz(width = 5.5, height = 4, pointsize = 6) # combined

heatmap.2(
  x = t(mat2),
  Rowv=T,
  Colv=T,
  trace='none',
  denscol = "black",
  density.info = "histogram",
  #densadj = 1,
  symkey = TRUE,
  # main='First tier',
  xlab='Cases',
  ylab ='Adaptive capacity features',
  keysize = 1.25, key = TRUE,
  key.title = "Score", key.xlab = NA, key.ylab = NA,
  #key.xtickfun = 0, key.ytickfun = 0,
  key.par=list(
    cex.main= 0.8, cex = 1, cex.axis = 0.8,
    cex.lab = 1, lwd = 0.1, mar=c(2,2,4,1), las = 1,
    lab = c(2,2,2), ps = 7),
  margins=c(15, 30), cexRow = 1.25, cexCol =1.15, srtCol = 90,
  col = colorspace::diverge_hcl(20, palette = "Blue-Red 2", rev = TRUE),
  ColSideColors = sidebar
  # ColSideColors = col_color
)

legend(
  x = 0.67, y = 1.01, pt.cex = 1.2, xjust = 0, #y.intersp = 0.2,
  fill=c("#377EB8","#984EA3","#FF7F00"),
  legend=c("Resilient", 'Transformation', 'Resilience loss'),
  bty='n', cex= 1.2, border = FALSE)

# quartz.save(
#   file = "paper/figures/heatmap_2tier.png", type = "png",
#   width = 5.5, height = 4, pointsize = 6, dpi = 300)








#### older graphs ####
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
    ggplot(aes(case, x2)) +
    geom_tile(aes(fill = score)) +
    scale_fill_gradient2(low = "red", mid = "grey75", high = "blue", midpoint = 0) +
    theme_light(base_size = 8) +
    theme(axis.text.x = element_text(angle=90, hjust = 1), 
          legend.position = "top")

## first level
dat %>% group_by(case, x1) %>%
    summarize(score = sum(value))%>%
    ggplot(aes(x1, case)) +
    geom_tile(aes(fill = score)) +
    scale_fill_viridis_c() +
    theme_light() +
    theme(axis.text.x = element_text(angle=90))


heatmap.2(
  x = dat %>% 
    select(-x1, -x2) %>% 
    pivot_wider(names_from = x3, values_from = value) %>% 
    select(-case) %>% 
    as.matrix() %>% 
    t(),
  Rowv=T,
  Colv=T,
  trace='none',
  denscol = "black",
  density.info = "histogram",
  #densadj = 1,
  symkey = TRUE,
  # main='First tier',
  # ylab='Cases',
  xlab ='Adaptive capacity features',
  keysize = 1.25, key = TRUE,
  key.title = "Score", key.xlab = NA, key.ylab = NA,
  #key.xtickfun = 0, key.ytickfun = 0,
  key.par=list(
    cex.main= 0.8, cex = 1, cex.axis = 0.8,
    cex.lab = 1, lwd = 0.1, mar=c(2,2,4,1), las = 1,
    lab = c(2,2,2), ps = 7),
  margins=c(5, 15), cexRow = 1, cexCol = 1, srtCol = 0,
  col = colorspace::diverge_hcl(20, palette = "Blue-Red 2", rev = TRUE),
  ColSideColors = sidebar
)
