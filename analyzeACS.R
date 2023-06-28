# Load libraries
library(tidyverse)
library(varhandle)
library(rlist)
library(rvest)
library(MASS)
library(data.table)
library(pbmcapply)
library(scales)
library(ggrepel)
library(ggpubr)
library(grid)
library(patchwork)

# Load data
load("acsdata.Rdata")

# Choose axes of identity
identityvars <- c("sex","age","marital","race","disability")

# Make helper functions
calculateMetrics <- function(thisstate,weightcol){
  # Choose sample of people
  # Reduce ACS data to selected identity variables
  data <- subset(acs,state==thisstate,select=c(identityvars,weightcol))
  # Collapse data
  data <- data %>% group_by_at(vars(one_of(identityvars))) %>% summarise(weight=sum(.data[[weightcol]]),.groups="drop")
  # Save only complete cases
  completes <- complete.cases(data)
  discarded <- sum(data[!completes,]$weight)/sum(data$weight)
  data <- data[completes,]
  # Calculate metrics
  N <- sum(data$weight) # Total number people
  C <- ncol(data) - 1 # Total number of identity categories
  CI <- prod(sapply(subset(data,select=identityvars),nlevels)) # Total number intersectional identities
  I <- lapply(data[,1:(ncol(data)-1)],function(x) to.dummy(x,"X"))
  b <- t(list.cbind(I))
  P <- t(b) %*% b
  weightmatrix <- outer(data$weight,data$weight,'*')
  P <- P * weightmatrix
  S <- (sum(P[upper.tri(P,diag=TRUE)])-sum(data$weight))/(choose(N,2)*C)
  ID <- (1-sum(prop.table(data$weight)^2))
  ID <- ID*CI/(CI-1) # Standardizes so ID is on interval [0,1]
  data.frame(state=thisstate,S=S,ID=ID)
}
checkstate <- function(basestate,data,whichvar){
  tmp <- subset(data,state==basestate)
  Sbase <- tmp$S
  IDbase <- tmp$ID
  rankbase <- tmp[[whichvar]]
  comps <- subset(data,S > Sbase & ID > IDbase)
  check <- TRUE
  good <- 0
  bad <- 0
  pct <- NA
  if (nrow(comps) > 0) {
    good <- sum(comps[[whichvar]] < rankbase)
    tot <- nrow(comps)
    bad <- tot - good
    pct <- good/tot
    check <- pct == 1
  }
  final <- data.frame(good=good,bad=bad,pct=pct,check=check)
  return(final)
}
score <- function(data){
  tmp <- rbindlist(lapply(data$state,checkstate,data,"gdprank"))
  return(sum(tmp$good)/sum(tmp$good+tmp$bad))
}
shuffle <- function(data) {
  data$gdprank <- sample(data$gdprank)
  return(data)
}

# Get per capita GDP rank of states
url <- "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_GDP_per_capita"
tabs <- read_html(url) %>% html_table()
gdp <- as.data.frame(tabs[[1]])
gdp <- gdp[3:53,c(1,8)]
names(gdp) <- c("state","gdp")
gdp <- gdp %>%
  mutate(gdp = str_replace_all(gdp,"\\,|\\$","")) %>%
  mutate(gdp = as.numeric(gdp)) %>%
  arrange(desc(gdp)) %>%
  mutate(state = str_replace_all(state, "\\*|","")) %>%
  mutate(state = str_squish(state)) %>%
  dplyr::select(-gdp) %>%
  mutate(gdprank = row_number())
names(gdp) <- c("state","gdprank")
gdp$state <- tolower(gdp$state)

# Create main data set
diversity <- lapply(levels(acs$state),calculateMetrics,"weight")
diversity <- rbindlist(diversity)

# Merge in rankings
diversity <- left_join(diversity,gdp)
diversity$gdprank <- as.numeric(diversity$gdprank)

# Do Heather-shuffling experiment to see what we should expect by chance
sims <- 1:10000
scores <- pbmclapply(sims, function(x) score(shuffle(diversity)), mc.cores=detectCores(logical=TRUE)-1)
scores <- data.frame(score=unlist(scores))

# Do experiment where we use alternative weights
scorealt <- function(weightcol){
  diversity <- lapply(levels(acs$state),calculateMetrics,weightcol)
  diversity <- rbindlist(diversity)
  diversity <- merge(diversity,gdp)
  return(score(diversity))
}
altscores <- pbmclapply(paste0("altweight",1:80),scorealt, mc.cores=detectCores(logical=TRUE)-1)
altscores <- data.frame(score=unlist(altscores))

# Calculate some quantiles
myecdf <- ecdf(scores$score)
true <- score(diversity)
trueq <- myecdf(true)
lowvar <- min(altscores$score)
lowvarq <- myecdf(lowvar)
avg <- mean(altscores$score)
avgq <- myecdf(avg)

# Set up reference state and example
refstate <- "alabama"
refrecord <- diversity %>%
  filter(state == refstate)
example <- diversity %>%
  filter(S >= refrecord$S & ID >= refrecord$ID) %>%
  mutate(group =case_when(
    state == refstate ~ "ref",
    TRUE ~ "comp")) %>%
  mutate(group = factor(group)) %>%
  mutate(label = paste0(str_to_title(state),"\nGDPPC # ",gdprank))

# Plot main data
xmin <- refrecord$ID - 0.45*diff(range(example$ID))
xmax <- max(example$ID) + 0.45*diff(range(example$ID))
ymin <- refrecord$S - 0.25*diff(range(example$S))
ymax <- max(example$S) + 0.25*diff(range(example$S))
p1 <- diversity %>%
  mutate(S = ifelse(state=="north carolina", 0.47, S)) %>%
  ggplot(aes(x=ID,y=S)) +
  geom_point(size = 1) +
  geom_text_repel(aes(label = str_to_title(state)),
                  min.segment.length = 0.04,
                  force = 1000,
                  force_pull = 0.03,
                  box.padding = 0.35,
                  point.padding = 0.5,
                  segment.color = "grey50",
                  segment.alpha = 0.3,
                  segment.size = 0.3,
                  size = 2,
                  max.iter = 30000,
                  show.legend = FALSE,
                  max.overlaps = 8) +
  xlab("Int. Diversity ID") +
  ylab("Shared Identity S") +
  xlim(0.96, 0.995) +
  ylim(0.425, 0.575) +
  annotate("rect", xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = "blue", alpha = 0.4)

# Plot example
xrange <- max(example$ID) - min(example$ID)
yrange <- max(example$S) - min(example$S)
prop <- 0.3
xmin <- min(example$ID) - prop*xrange
xmax <- max(example$ID) + prop*xrange
ymin <- min(example$S) - prop*yrange
ymax <- max(example$S) + prop*yrange
p2 <- ggplot(example,aes(x=ID,y=S, color = group, alpha = 0.7)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(xmin, xmax), breaks = NULL, minor_breaks = NULL) +
  scale_y_continuous(limits = c(ymin, ymax), breaks = NULL, minor_breaks = NULL) +
  scale_color_discrete(type = c("blue","blue")) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "none") +
  geom_label_repel(force = 8000, force_pull = 30, aes(label = label),box.padding=0.35,segment.color='grey50',size=1.8,show.legend=FALSE, min.segment.length = 0, max.time = 10, max.iter = 100000) +
  xlab("Int. Diversity ID") +
  ylab("Shared Identity S")


# Plot results of shuffling
p3 <- ggplot(scores,aes(x=score)) +
  geom_density(alpha=0.5,fill="green",color=NA) +
  geom_density(data=altscores,alpha=0.2,fill="red",color=NA,bw="bcv") +
  geom_vline(xintercept=round(true,2),color="black", linetype = "dashed") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  xlab("GDP-Demographics Hypothesis Accuracy") +
  ylab("Density") +
  annotate("label", label = "Distribution for true ranks under\nalternative ACS sample weights", x = 0.71, y = 12, hjust = 0.5, vjust = 0.5, size = 1.7) +
  annotate("label", label = "Distribution arising from\nrandom shuffles of GDP rank", x = 0.5, y = 1, hjust = 0.5, vjust = 0.5, size = 1.77)

# Combine plots
finalplot <- (p1 + p2) / p3 +
  plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(1.25,2), heights = c(2,1))
ggsave(finalplot,file="fig4.pdf", width= 5, height = 5, units="in")
knitr::plot_crop("fig4.pdf")


######
tmp <- diversity %>% dplyr::filter(state %in% c("alabama", "alaska", "oklahoma", "south carolina", "illinois", "virginia", "delaware", "district of columbia"))
states <- tmp$states
teststate <- function(x)

