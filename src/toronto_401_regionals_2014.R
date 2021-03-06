library("rjson")
library("ggplot2")

json_file <- "data/401-140628/NRTM.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

append_value <- function (L, id, value) {
    if (id %in% names(L)) {
        L[[id]] <- c(L[[id]],value)
    } else {
        L[[id]] <- c(value)
    }
    return(L)
}

#Transmute the data layout
player_list <- list(id=c(),name=c(),rank=c(),corp=c(), runner=c())
for (i in json_data$players) {
    player_list <- append_value(player_list, "id", i$id)
    player_list <- append_value(player_list, "name", i$name)
    player_list <- append_value(player_list, "rank", i$rank)
    player_list <- append_value(player_list, "corp", i$corpIdentity)
    player_list <- append_value(player_list, "runner", i$runnerIdentity)
}
player_df <- data.frame(player_list$id,player_list$name,player_list$rank,player_list$corp,player_list$runner)
names(player_df) <- c("id","name","rank","corp","runner")

#Add to our data frame the number of wins each player got
add_points <- function(data, player) {
    cp <- data[data$id==player$id,"corp.prestige"]
    rp <- data[data$id==player$id,"runner.prestige"]
    data[data$id==player$id,"corp.prestige"] <- cp + player$corpScore
    data[data$id==player$id,"runner.prestige"] <- rp + player$runnerScore
    return(data)
}

nplayers <- length(json_data$players)
player_df$corp.prestige <- rep(0,nplayers)
player_df$runner.prestige <- rep(0,nplayers)
for (r in json_data$rounds) {
    for (g in r) {
        if(!g$eliminationGame) {
            player_df <- add_points(player_df, g$player1)
            player_df <- add_points(player_df, g$player2) 
        }
    } 
}

#Gather Tables
#Corp
countcp <- tapply(rep(1,length(player_df$corp)), player_df$corp, sum)
meancp <- tapply(player_df$corp.prestige, player_df$corp, mean)

#Runner
countrp <- tapply(rep(1,length(player_df$runner)), player_df$runner, sum)
meanrp <- tapply(player_df$runner.prestige, player_df$runner, mean)

allcounts <- c(countcp,countrp)
mean.prestige <- c(meancp, meanrp)
identity_df <- data.frame(allcounts, mean.prestige)
names(identity_df) <- c("Number.of.Players", "Average.Prestige")
identity_df$Identity <- row.names(identity_df)
faction_colours <- c('Because We Built It'='darkgreen',
                     'Building a Better World'='darkgreen',
                     'Cerebral Imaging'='purple',
                     'Engineering the Future'='purple',
                     'GRNDL'='darkgreen',
                     'Harmony Medtech'='red',
                     'Making News'='yellow',
                     'NEXT Design'='purple',
                     'Nisei Division'='red',
                     'Personal Evolution'='red',
                     'Replicating Perfection'='red',
                     'Stronger Together'='purple',
                     'Tennin Institute'='red',
                     'The World is Yours*'='yellow',
                     'Andromeda'='blue',
                     'Chaos Theory'='green',
                     'Exile'='green',
                     'Gabriel Santiago'='blue',
                     'Iain Stirling'='blue',
                     'Kate "Mac" McCaffrey'='green',
                     'Ken "Express" Tenma'='blue',
                     'Noise'='orange',
                     'Reina Roja'='orange',
                     'Rielle "Kit" Peddler'='green',
                     'Silhouette'='blue',
                     'Whizzard'='orange')

#Some information about colour pairings
n <- nrow(player_df)

corp.assign <- function(corp){
    ifelse(corp == "Making News" | corp == "The World is Yours*", "NBN",
            ifelse(corp == "Replicating Perfection" | corp == "Personal Evolution" | 
                       corp == "Tennin Institute" | corp == "Harmony Medtech" | corp == "Nisei Division", "Jinteki",
                 ifelse(corp == "Engineering the Future" | corp == "NEXT Design" | corp == "Cerebral Imaging", "Haas-Bioroid",
                         ifelse(corp == "GRNDL" | corp == "Building a Better World" | corp == "Because We Built It", "Weyland", "Unknown"))))
}

for(i in 1:n){
    cfaction <- corp.assign(player_df$corp[i])
    player_df$corpf[i]<- cfaction
}

run.assign <- function(runner){
    ifelse(runner == 'Andromeda' | runner == 'Gabriel Santiago' | runner == 'Silhouette' | 
               runner == 'Ken "Express" Tenma' | runner == 'Iain Stirling', 'Criminal',
           ifelse(runner == 'Chaos Theory' | runner == 'Kate "Mac" McCaffrey' | 
                      runner == 'Rielle "Kit" Peddler' | runner == 'Exile', 'Shaper',
                  ifelse(runner == 'Whizzard' | runner == 'Reina Roja' | runner == 'Noise', 'Anarch', 'Unknown')))
}

for(i in 1:n){
    rfaction <- run.assign(player_df$runner[i])
    player_df$runf[i]<- rfaction
}

player_df$corpf <- as.factor(player_df$corpf)
player_df$runf <- as.factor(player_df$runf)

#Plotting the specifics for Regionals 401 Games 2014
#Jittering some of the ids manually
identity_df['NEXT Design',]$Average.Prestige <- 2.15
identity_df['Building a Better World',]$Average.Prestige <- 1.85
identity_df['Silhouette',]$Average.Prestige <- 6.15
identity_df['Cerebral Imaging',]$Average.Prestige <- 5.85
identity_df['Iain Stirling',]$Average.Prestige <- 2.86667
identity_df['Ken "Express" Tenma',]$Average.Prestige <- 2.46667
identity_df['Noise',]$Average.Prestige <- 3.35
identity_df['Reina Roja',]$Average.Prestige <- 3.05
identity_df['Replicating Perfection',]$Average.Prestige <- 7.4
identity_df['Andromeda',]$Average.Prestige <- 7.025

pdf("image.pdf", width=24, height=10)
p2 <- ggplot(identity_df, aes(as.numeric(Number.of.Players), Average.Prestige, label=rownames(identity_df)))
p2 <- p2 + scale_x_continuous(breaks=seq(1, 16, 1)) + scale_y_continuous(breaks=seq(0, 10, 1))
p2 <- p2 + geom_text(aes(colour=Identity, vjust=1.5)) + scale_colour_manual(guide=FALSE, values=faction_colours) 
p2 <- p2 + geom_point(aes(colour = Identity))
p2 <- p2 + labs(title = "Toronto 401 Games Regionals 2014\nNumber of Players and Average Number of Prestige per Faction", 
                x="Number of Players", y = "Average Prestige")
p2 <- p2 + theme_bw()
plot(p2)
dev.off()

# Mosaic Plot to look at interaction between faction choices:
mosaicplot(~ player_df$corpf + player_df$runf, shade = TRUE, xlab = "Corporation Faction", ylab = "Runner Faction", main = "Faction Choice Combinations at\n the 401 Regionals June 2014")

# Some interesting trends, but nothing substantial (or significant).  More Anarch players brought HB/Weyland, and fewer Jinteki/NBN. Criminals and Shapers mainly concentrated on Jinteki/NBN.
