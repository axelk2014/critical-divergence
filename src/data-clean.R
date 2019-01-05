# data-clean.R script created for metacritic analysis


# Dataset from vgchartz.com In Million Unit source: https://data.world/julienf/video-games-global-sales-in-volume-1983-2017/workspace/file?filename=vgsalesGlobale.csv
# used to add genre and publisher to metacritic dataframe
df.globalsales <- read.csv("data/vgsalesGlobale.csv")  
df.globalsales$Year <- as.numeric(as.character(df.globalsales$Year))
df.globalsales$Name <- as.character(df.globalsales$Name)

# METACRITIC data
df.metacritic <- read.csv("data/result.csv", header=TRUE)
df.metacritic$userscore <- as.numeric(as.character(df.metacritic$userscore))

# transform userscore to same scale as metascore
df.metacritic$userscore <- df.metacritic$userscore*10

# format dates
df.metacritic$date.formatted <- as.Date(as.character(df.metacritic$date), "%b %d, %Y")

df.metacritic <- merge(df.metacritic,df.globalsales[,c(2,5:6)], all.x=TRUE,by.x="name",by.y="Name")
write.csv(df.metacritic, file="references/metacritic-results.csv", row.names = FALSE)

# create aggregated dataset with average metascore
df.avg.metascore <- as.data.frame(df.metacritic %>% group_by(name) %>% summarise(mean=round(mean(metascore),2))) 
df.avg.metascore <- merge(df.avg.metascore,df.globalsales[,c(2,5:6)], all.x=TRUE,by.x="name",by.y="Name")


df.mc.bp <- melt(df.metacritic,id=c("name")) 
