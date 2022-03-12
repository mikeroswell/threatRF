# null model for rank correlations
library(tidyverse)
threat<-rbeta(200, 2, 2.8)
secure <- rbeta(100, 2.8, 2)
from_mod<-data.frame(prob = c(threat, secure), simple = c(rep("threat", 200), rep("secure", 100)))                     

#visualize
from_mod %>% 
  ggplot(aes(prob, fill = simple)) + 
  geom_density(alpha = 0.6) + 
  theme_classic()

# is this the same as the AUC?
wilcox.test(threat, secure, )$statistic/20000

# make up the ranks now
ranks <- c(sample(1:3, 200, replace =T), sample (4:5, 100, replace =T))

dat <-cbind(ranks, from_mod)
cor(dat[,1], dat[,2], method = "spearman")
