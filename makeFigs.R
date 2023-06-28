library(igraph)
library(tidyverse)
library(ggnetwork)
library(scales)
library(patchwork)

# Define styles
personshape <- 16 # circle
ICshape <- 15 # square
shapepal <- c(personshape, ICshape)
shapesize <- 10
colorpal <- hue_pal()(3)[c(1,3)] %>% rev

# Define incidence matrix
B <- matrix(c(1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1),4,6,byrow=FALSE)
colnames(B) <- 1:ncol(B)
rownames(B) <- 1:nrow(B)

# Create bipartite graph object
g <- graph_from_incidence_matrix(B)
V(g)$type <- !V(g)$type

# Plot
labels <- c("X[1]", "Y[1]", "X[2]", "Y[2]", "1", "2", "3", "4", "5", "6")
p1 <- g %>%
  ggnetwork(layout = layout_as_bipartite(g)) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(aes(color = type, shape = type), size = shapesize) +
  geom_nodetext(label = labels, parse = TRUE) +
  scale_color_manual(values = colorpal) +
  scale_shape_manual(values = shapepal) +
  scale_x_continuous(expand = c(0.1, 0)) +
  scale_y_continuous(expand = c(0.1, 0)) +
  theme_blank() +
  theme(legend.position = "none")

labels <- c("1", "2", "3", "4", "5", "6")
SIproj <- bipartite_projection(g, which = FALSE)
p2 <- SIproj %>%
  ggnetwork(layout = layout_in_circle(SIproj)) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(shape = personshape, color = colorpal[1], size = shapesize) +
  geom_nodetext(aes(label = name)) +
  geom_edgetext(aes(label = weight)) +
  scale_x_continuous(expand = c(0.1, 0)) +
  scale_y_continuous(expand = c(0.1, 0)) +
  theme_blank() +
  theme(legend.position = "none")

# Add labels to each plot and combine
P1 <- p1 + p2 +  plot_annotation(tag_levels = "A")
ggsave(plot = P1, filename = "fig1.pdf", width = 5, height = 2.45, units = "in")
knitr::plot_crop("fig1.pdf")

makeGraph <- function(B, shapesize) {
  colnames(B) <- 1:ncol(B)
  rownames(B) <- 1:nrow(B)
  g <- graph_from_incidence_matrix(B)
  V(g)$type <- !V(g)$type
  labels <- c("X[1]", "Y[1]", "X[2]", "Y[2]", "1", "2", "3", "4", "5", "6")
  p <- g %>%
    ggnetwork(layout = layout_as_bipartite(g)) %>%
    mutate(name = case_when(
      type == TRUE & name == 1 ~ "X[1]",
      type == TRUE & name == 2 ~ "X[2]",
      type == TRUE & name == 3 ~ "Y[1]",
      type == TRUE & name == 4 ~ "Y[2]",
      TRUE ~ name)) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(color = "grey50") +
    geom_nodes(aes(color = type, shape = type), size = shapesize) +
    geom_nodetext(aes(label = name), parse = TRUE) +
    scale_color_manual(values = colorpal) +
    scale_shape_manual(values = shapepal) +
    scale_x_continuous(expand = c(0.1, 0)) +
    scale_y_continuous(expand = c(0.1, 0)) +
    theme_blank() +
    theme(legend.position = "none")
}

# Do the three groups
B1 <- matrix(c(1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1),4,6,byrow=FALSE)
p1 <- makeGraph(B1, 6)
B2 <- matrix(c(1,0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,0,1,0,1,0,1,0,1),4,6,byrow=FALSE)
p2 <- makeGraph(B2, 6)
B3 <- matrix(c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1),4,6,byrow=FALSE)
p3 <- makeGraph(B3, 6)
P2 <- p1 + p2 + p3 + plot_annotation(tag_levels = "A")
ggsave(plot = P2, filename = "fig2.pdf", width = 5, height = 2, units = "in")
knitr::plot_crop("fig2.pdf")