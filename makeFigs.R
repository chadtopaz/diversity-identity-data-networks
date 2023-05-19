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
colorpal <- hue_pal()(2) %>% rev

# Define incidence matrix
B <- matrix(c(1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1),4,6,byrow=FALSE)
colnames(B) <- 1:ncol(B)
rownames(B) <- 1:nrow(B)

# Create bipartite graph object
g <- graph_from_incidence_matrix(B)
V(g)$type <- !V(g)$type

# Plot
labels <- c("R[1]", "G[1]", "R[2]", "G[2]", "1", "2", "3", "4", "5", "6")
p1 <- g %>%
  ggnetwork(layout = layout_as_bipartite(g)) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(aes(color = type, shape = type), size = shapesize) +
  geom_nodetext(label = labels, parse = TRUE) +
  scale_color_manual(values = colorpal) +
  scale_shape_manual(values = shapepal) +
  theme_blank() +
  theme(legend.position = "none")

labels <- c("1", "2", "3", "4", "5", "6")
SIproj <- bipartite_projection(g, which = FALSE)
p2 <- SIproj %>%
  ggnetwork(layout = layout_in_circle(SIproj)) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50") +
  geom_nodes(shape = personshape, color = colorpal[1], size = shapesize) +
  geom_nodetext(label = labels, parse = TRUE) +
  geom_edgetext(aes(label = weight)) +
  theme_blank() +
  theme(legend.position = "none")

# Add labels to each plot and combine
p3 <- p1 + p2 +  plot_annotation(tag_levels = "A")
ggsave(plot = p3, filename = "fig1.pdf", width = 6.5, height = 3.3, units = "in")