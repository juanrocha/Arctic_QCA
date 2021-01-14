library(tidyverse)
library(maps)
library(mapproj)
library(ggrepel)
library(gplots)
library(patchwork)
library(gridGraphics)
library(pheatmap)
library(png)

setwd("~/Documents/Projects/MARAT/Arctic_QCA")

## Produced with 01-read_data.R
load('data/coords.Rdata')
load('data/cleaned_data.RData')
load('data/cleaned_df.RData')

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
    theme_void(base_size = 7) +
    labs(tag = "A") +
    theme(legend.position = c(0.2, 0.1), legend.key.size = unit(0.25, "cm"),
          legend.direction = "vertical", legend.box = "horizontal",
          plot.tag.position = c(0.01,0.95), legend.title = element_text(size = 5),
          legend.text = element_text(size = 5))

quartz(width = 3.5, height = 3.5)
arctic_map

ggsave(filename = "arctic_map.png", path = "paper/figures/",
       plot = arctic_map, width = 3.5, height = 3.5, device = "png",
       dpi = 600)

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

quartz(width = 3, height = 3.5, pointsize = 5) # for heatmap


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

quartz.save(
    file = "paper/figures/heatmap.png", type = "png",
    width = 3, height = 3.5, pointsize = 5, dpi = 300)


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


quartz(width = 7, height = 3.5, pointsize = 5) # combined
hm <- ggplot(data = data_frame(x = 0, y= 0), aes(x,y)) + geom_blank() +
    labs(tag = "B") +
    theme_void(base_size = 7) +
    annotation_custom(
        grob = grid::rasterGrob( image = readPNG(
            "paper/figures/heatmap.png"), interpolate = TRUE),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
    )

arctic_map + hm + plot_layout(widths = c(3.5, 3.5))



ggsave(
    filename = "fig1_map_vars.png", path = "paper/figures/",
    plot = last_plot(), width = 7, height = 3.5, dpi = 300,
    device = "png"
)



#### Fig 2: dendrogram instead of table ####
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
        size = 1) +
    coord_flip() +
    ylim(3.3,-2) +
    scale_edge_colour_manual(values = c("#FF7F00", "#377EB8")) +
    theme_void(base_size = 6) +
    theme(legend.position = c(0.1, 0.1), legend.title = element_blank(),
          legend.key.size = unit(0.25,"cm"), legend.text = element_text(size = 4))

dend

ggsave(
    filename = "vars_dendrogram.png", path = "figures/",
    plot = dend, width = 4.5, height = 3, dpi = 500,
    device = "png"
)

#### Fig 3: calibration ####
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
       plot = last_plot(), width = 3, height = 3, dpi = 300,
       device = "png")

## truth table

tt_RES$tt %>% as_tibble() %>%
  select(-cases) %>%
  mutate(OUT = str_replace(string = OUT, pattern = "\\?", replacement = "NA") %>% as.numeric(),
         incl = str_replace(incl, "-", "NA") %>% as.numeric(),
         PRI = str_replace(PRI, "-", "NA") %>% as.numeric()) %>%
  
  pivot_longer(1:last_col(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(variable))










#### SM figs ####

## Correlograms:
GGally::ggpairs(df1, columns = 2:5)



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
