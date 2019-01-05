# analysis-functions.R script for metacritic analysis
# created 12/12/17

library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggthemes)
library("car")

# Make sure your R session is setup at the root of your repo
# before running this script
# ie.

# setwd("insertpathtoyourrepo")

a1 <- Sys.time()

source("src/data-clean.R")

a2 <- Sys.time()
print("sourcing data-clean.R complete")
print(a2-a1)


# METACRITIC
# boxplots
ggplot(data=df.metacritic,aes(1,metascore)) + stat_boxplot(geom="errorbar") + geom_boxplot() + 
  stat_boxplot(geom="errorbar",aes(x=3,userscore)) + geom_boxplot(aes(x=3,userscore)) +  
  scale_x_continuous(breaks=c(1,3),labels=c("metascore", "userscore")) +
  geom_text(data = df.metacritic %>% summarise(five=list(fivenum(metascore))) 
            %>% tidyr::unnest(),
            aes(x=1,y=five,label=five),color="grey33",
            nudge_y=.7, nudge_x=.5) +
  geom_text(data = df.metacritic %>% summarise(five=list(fivenum(userscore))) 
            %>% tidyr::unnest(),
            aes(x=3,y=five,label=five),color="grey33",
            nudge_y=.7, nudge_x=0.5) +
  labs(x="",y="",title="Distribution of metascores and userscores") +
  theme_economist() +
  theme(plot.title = element_text(hjust=0.5))


# frequency histograms
median.text = as.character(median(na.omit(df.metacritic$metascore)))
mean.text = as.character(round(mean(df.metacritic$metascore),2))
skewness.text = as.character(skewness(na.omit(df.metacritic$metascore)))

h1 <- ggplot(data=df.metacritic,aes(metascore)) + geom_histogram(breaks=seq(0, 100, by=1)) +
  labs(title="Frequency count of metascores & userscores") +
  annotate("text", x=15,y=750,label= expression(tilde(italic(x)))) +
  annotate("text", x=18,y=750,label=paste("=",median.text)) + 
  annotate("text", x=15,y=650,label= expression(mu)) +
  annotate("text", x=19,y=650,label=paste("=",mean.text)) +
  annotate("text", x=14,y=400,label=paste("skewness =",skewness.text), size=3, fontface="italic") +
  #geom_text(mapping=)
  ylim(0,1200) + 
  theme_economist() +
  theme(plot.title = element_text(hjust=0.5),
        panel.grid.major = element_line(size=0.5))

median.text = as.character(median(na.omit(df.metacritic$userscore)))
mean.text = as.character(round(mean(na.omit(df.metacritic$userscore)),2))
skewness.text = as.character(skewness(na.omit(df.metacritic$userscore)))

h2 <- ggplot(data=df.metacritic,aes(userscore)) + geom_histogram(breaks=seq(0, 100, by=1)) +
  ylim(0,1200) +
  annotate("text", x=15,y=750,label= expression(tilde(italic(x)))) +
  annotate("text", x=18,y=750,label=paste("=",median.text)) + 
  annotate("text", x=15,y=650,label= expression(mu)) +
  annotate("text", x=19,y=650,label=paste("=",mean.text)) +
  annotate("text", x=15,y=400,label=paste("skewness =",skewness.text), size=3, fontface="italic") +
  
  theme_economist() +
  theme(panel.grid.major = element_line(size=0.5))

ggarrange(h1,h2, ncol = 1, nrow = 2, align = "v")

ks.test(df.metacritic$metascore,df.metacritic$userscore)
metascores <- sample(df.metacritic$metascore,5000,replace=FALSE)
userscores <- sample(df.metacritic$userscore,5000,replace=FALSE)
shapiro.test(metascores)
ad.test(metascores)
shapiro.test(userscores)
ad.test(userscores)



#qqnorm(df.metacritic$metascore)
#qqnorm(df.metacritic$userscore)
qqPlot(df.metacritic$metascore)
qqPlot(df.metacritic$userscore)

summary(df.metacritic$metascore)
summary(df.metacritic$userscore)


