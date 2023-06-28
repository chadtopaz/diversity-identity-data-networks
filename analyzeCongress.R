# Load libraries
library(tidyverse)

# Function to calculate scores for a given sample
scoreCommittee <- function(ids, B, people) {
  N <- length(ids)
  b <- B[,ids]
  P <- t(b) %*% b
  identityvars <- c("gender","ethnicity","lgbtq","age")
  C <- length(identityvars) # Total number of identity categories
  tallies <- people %>%
    .[ids,] %>%
    group_by(across(all_of(identityvars))) %>% 
    summarise(count = n()) %>%
    ungroup %>%
    complete(gender, ethnicity, lgbtq, age) %>% 
    mutate(count = ifelse(is.na(count), 0, count)) %>%
    mutate(count = count/sum(count))
  CI <- nrow(tallies) # Total number intersectional identities
  ID <- (1-sum((tallies$count)^2))*(CI)/(CI-1)
  S <- sum(P[upper.tri(P,diag=FALSE)])/(choose(N,2)*C)
  return(c(ID,S))
}

scoreChamber <- function(chamber) {
  
  # Get data
  peoplefile <- paste0(switch(chamber,"senate"="senators","house"="reps"),".csv")
  people <- read.csv(peoplefile)
  committeesfile <- paste0(switch(chamber,"senate"="senate","house"="house"),"Committees.csv")
  committees <- read.csv(committeesfile)
  paramsfile <- paste0(switch(chamber,"senate"="senate","house"="house"),"Params.csv")
  scores <- read.csv(paramsfile) %>%
    select(-majority, -minority)
  
  # Make factors
  people$party <- as.factor(people$party)
  people$gender <- factor(people$gender, levels = c("man", "woman"))
  people$ethnicity <- factor(people$ethnicity, levels = c("asian", "black", "latinx", "middleeastern", "nativeamerican", "pacificislander", "white"))
  people$lgbtq <- factor(people$lgbtq, levels = c("no", "yes"))
  people$age <- factor(people$age, levels = c("30", "40", "50", "60", "70", "80"))
  
  # Convert demographic characteristics into binary ones and construct incidence matrix
  gender <- to.dummy(people$gender,"gender")
  ethnicity <- to.dummy(people$ethnicity,"ethnicity")
  lgbtq <- to.dummy(people$lgbtq,"lgbtq")
  age <- to.dummy(people$age,"age")
  B <- t(cbind(gender,ethnicity,lgbtq,age))
  
  # Score the actual committee
  scores$ID <- NA
  scores$S <- NA
  for (i in 1:nrow(scores)) {
    members <- unlist(subset(committees, committee == scores[i,"committeename"], select="id"))
    scores[i,c("ID","S")] <- scoreCommittee(members, B, people)
  }
  return(scores)
}

# Get scores
senatescores <- scoreChamber("senate") %>%
  mutate(chamber = "senate")
housescores <- scoreChamber("house") %>%
  mutate(chamber = "house")
scores <- rbind(senatescores, housescores)

# Plot
scores %>%
  ggplot(aes(x = ID, y = S, color = chamber, shape = chamber)) +
  geom_point() +
  geom_text_repel(aes(label = committeename),
                  min.segment.length = 0.04,
                  force = 30,
                  force_pull = 1,
                  box.padding = 0.35,
                  point.padding = 0.5,
                  segment.color = "grey50",
                  segment.alpha = 0.3,
                  segment.size = 0.3,
                  size = 2.3,
                  show.legend = FALSE,
                  max.overlaps = 20) +
  xlab("Intersectional Diversity ID") +
  ylab("Shared Identity S")
ggsave(paste0("~/Desktop/",chamber,".png"),p,device="png",width=8,height=8/1.618,units="in")