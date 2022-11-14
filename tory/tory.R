library(ggplot2)
library(dplyr)
library(png)
library(grid)

hamilton = c('#ffffff', '#00c2cb')
vettel = c('#ffffff', '#004b20')
verstappen = c('#ffffff', '#004aad')
schumacher = c('#ffffff', '#e20404')

proteza <- data.frame(
  pion = c('a', 'b', 'c', 'd', 'e'),
  poziom = c(1, 1, 1, 1, 1),
  info = c('14', '', '', '', '50%'),
  isvisible = c(rep('no',4),'yes')
)
img <- readPNG('tory/bahrain.png')
g <- rasterGrob(img, interpolate=TRUE)
bahrain <- ggplot(proteza, aes(y = pion, x = poziom, fill = isvisible)) +
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
  scale_fill_manual(values = hamilton) +
  scale_y_discrete(limits=rev) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=1.3, ymax=5)
bahrain
ggsave('tory/bahrain.png', bahrain, bg='transparent')

proteza <- data.frame(
  pion = c('a', 'b', 'c', 'd', 'e'),
  poziom = c(1, 1, 1, 1, 1),
  info = c('14', '', '', '', '50%'),
  isvisible = c(rep('no',4),'yes')
)
img <- readPNG('tory/australia.png')
g <- rasterGrob(img, interpolate=TRUE)
australia <- ggplot(proteza, aes(y = pion, x = poziom, fill = isvisible)) +
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
  scale_fill_manual(values = vettel) +
  scale_y_discrete(limits=rev) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
australia
ggsave('tory/australia.png', australia, bg='transparent')
