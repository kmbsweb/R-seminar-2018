library(MASS)
library(dplyr)
library(ggplot2)
library(ggthemes)

#8-a
#http://www-bcf.usc.edu/~gareth/ISL/index.html
college <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")

#8-b
rownames(college) <- college[,1]
college <- college[,-1]
fix(college)

#8-c
#factor型でないと集計できない
summary(college)

round(cor(college[,2:10]),3)

ggplot(college) +
  geom_boxplot(mapping = aes(x=Private, y=Outstate))

college <- college %>%
  mutate(Elite = ifelse(Top10perc >= 50, "Yes", "No"))

summary(as.factor(college$Elite))

ggplot(college) +
  geom_boxplot(mapping = aes(x=Elite, y=Outstate))

ggplot(college, mapping = aes(x=PhD)) +
  geom_histogram(binwidth = 1)

ggplot(college, mapping = aes(x = PhD, fill = Private)) +
  geom_histogram(binwidth = 3) +
  scale_fill_gdocs() +
  theme_bw(base_family = "HiraKakuProN-W3") 

#10
data("Boston")
?Boston

ggplot(Boston, mapping = aes(x=age)) +
  geom_histogram(binwidth = 4)

round(cor(Boston$crim,Boston[,2:14]),3)
