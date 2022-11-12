library(ggplot2)
library(dplyr)
library(png)
library(grid)

proteza <- data.frame(
  pion = c('a', 'b', 'c', 'd', 'e'),
  poziom = c(0, 0, 0, 0, 1),
  info = c('14', '', '', '', '50%'),
  color = c('ffffff','ffffff','ffffff','ffffff', '00c2cb')
)

proteza
img <- readPNG('bahrain.png')
g <- rasterGrob(img, interpolate=TRUE)

bahrain <- ggplot(proteza, aes(y = pion, x = poziom, fill = '3e79e7')) +
  geom_col() +
  geom_text(aes(label = info),
            hjust = 1, size = 10,
            fontface = 'bold') +
  theme_classic() +
  theme(axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.y= element_blank(),
        axis.ticks.y= element_blank(),
        legend.position = 'none') +
  scale_y_discrete(limits=rev) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

bahrain  
  

