# Load libraries
library(tidyverse)
library(rvest)
library(varhandle)
library(rlist)
library(data.table)
library(ggrepel)

# Load film data, clean up, see how complete it is
film <- read.csv("filmFinal.csv")
film <- film %>%
  dplyr::select(year,rank,movie,tconst,theaters,gross,releasedate,name,nconst,category,gender,ethnicity)
film$gender <- as.factor(film$gender)
film$ethnicity[film$ethnicity=="other"] <- NA
film$ethnicity[film$ethnicity=="middleeastern"] <- "middleeast"
film$ethnicity <- as.factor(film$ethnicity)
film$ethnicity <- droplevels(film$ethnicity)

# Get complete cases
completes <- film %>%
  dplyr::select(gender,ethnicity) %>%
  complete.cases
film <- film %>%
  filter(!tconst %in% unique(film[!completes,"tconst"]))

# Choose axes of identity
identityvars <- c("gender","ethnicity")

# Make helper functions
calculateMetrics <- function(thistconst){
  # Choose sample of people
  # Reduce data to selected identity variables
  data <- subset(film,tconst==thistconst,select=c(identityvars))
  # Calculate metrics
  N <- nrow(data) # Total number people
  C <- ncol(data) # Total number of identity categories
  CI <- prod(sapply(subset(data,select=identityvars),nlevels)) # Total number intersectional identities
  I <- lapply(data[,1:(ncol(data))],function(x) to.dummy(x,"X"))
  b <- t(list.cbind(I))
  P <- t(b) %*% b
  S <- sum(P[upper.tri(P,diag=FALSE)])/(choose(N,2)*C)
  ID <- 1-sum(prop.table(table(data$gender,data$ethnicity))^2)
  ID <- ID*CI/(CI-1) # Standardizes so ID is on interval [0,1]
  #props <- as.numeric(prop.table(data$weight))
  #ID <- -sum(props*log2(props))/log2(CI)
  data.frame(tconst=thistconst,S=S,ID=ID)
}

# Score the movies
diversity <- rbindlist(lapply(unique(film$tconst),calculateMetrics))


# Add to main dataset, reduce
diversity <- merge(diversity,film,by="tconst",all.x=TRUE,all.y=FALSE)
diversity <- diversity %>%
  dplyr::select(tconst,movie,S,ID) %>%
  unique()

# Plot
new <- diversity %>% dplyr::select(S,ID) %>% data.matrix() %>% jitter(amount=0.05)
diversity$Splot <- new[,1]
diversity$IDplot <- new[,2]
diversity$Splot <- pmax(diversity$Splot,0)
diversity$Splot <- pmin(diversity$Splot,1)
diversity$IDplot <- pmax(diversity$IDplot,0)
diversity$IDplot <- pmin(diversity$IDplot,1)

p <- diversity %>%
  ggplot(aes(x=ID,y=S)) +
  geom_jitter(size = 0.5) +
  geom_text_repel(aes(label = movie), min.segment.length = 0.04, force = 30, force_pull = 1, segment.color = 'grey50', segment.alpha = 0.3, segment.size = 0.3, size = 2.3, show.legend = FALSE, max.overlaps = 20) + 
  xlab("Intersecting Diversity ID") +
  ylab("Shared Identity S") +
  coord_fixed()
ggsave(plot = p, filename = "fig3.pdf", width = 5, units = "in")
knitr::plot_crop("fig3.pdf")