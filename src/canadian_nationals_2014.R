library("rjson")
library("ggplot2")



build_player_data_frame <- function(json_file) {
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
  
  add_wins <- function(data, g) {
    cp1 <- g$player1$corpScore
    rp1 <- g$player1$runnerScore
    cp2 <- g$player2$corpScore
    rp2 <- g$player2$runnerScore
    
    if (cp1 == 2) {
      data[data$id==g$player1$id,"corp.win"] <- data[data$id==g$player1$id,"corp.win"] + 1
    } 
    if (rp1 == 2) {
      data[data$id==g$player1$id,"runner.win"] <- data[data$id==g$player1$id,"runner.win"] + 1
    }
    if (cp2 == 2) {
      data[data$id==g$player2$id,"corp.win"] <- data[data$id==g$player2$id,"corp.win"] + 1
    }
    if (rp2 == 2) {
      data[data$id==g$player2$id,"runner.win"] <- data[data$id==g$player2$id,"runner.win"] + 1
    }

    return(data)
  }
  
  
  add_flatlines <- function(data, g) {
    cp1 <- g$player1$corpScore
    rp1 <- g$player1$runnerScore
    cp2 <- g$player2$corpScore
    rp2 <- g$player2$runnerScore
    g1w <- g$game1WonBy
    g2w <- g$game2WonBy
    p1_corp <- data[data$id==g$player1$id,"corp"]
    p2_corp <- data[data$id==g$player2$id,"corp"]
    if ((g1w == "ap") && (g2w == "ap")) {
      return(data)
    } else if ((cp1 == 2) && (cp2 != 2)) {
      #Corp 1 won by flatline
      data[data$id==g$player1$id,"corp.flatline"] <- data[data$id==g$player1$id,"corp.flatline"] + 1
      data[data$id==g$player2$id,"runner.flatline"] <- data[data$id==g$player2$id,"runner.flatline"] + 1
    } else if ((cp1 != 2) && (cp2 == 2)) {
      #Corp 2 won by flatline
      data[data$id==g$player2$id,"corp.flatline"] <- data[data$id==g$player2$id,"corp.flatline"] + 1
      data[data$id==g$player1$id,"runner.flatline"] <- data[data$id==g$player1$id,"runner.flatline"] + 1
    } else if ((g1w == "flatline") && (g2w == "flatline")) {
      #Both corp's won by flatline
      data[data$id==g$player2$id,"corp.flatline"] <- data[data$id==g$player2$id,"corp.flatline"] + 1
      data[data$id==g$player1$id,"runner.flatline"] <- data[data$id==g$player1$id,"runner.flatline"] + 1
      data[data$id==g$player1$id,"corp.flatline"] <- data[data$id==g$player1$id,"corp.flatline"] + 1
      data[data$id==g$player2$id,"runner.flatline"] <- data[data$id==g$player2$id,"runner.flatline"] + 1
      #Add to both tallies
    } else if ((cp1 == 2) && (cp2 == 2)) {
      #Special case where the information is missing, have to print it out and decide by hand
      #what is more likely.
      #Namely we know 2 corps won, but only one won by flatline, and it's not listed which
      #I've looked at them and you can't really figure much out.  
      #I'd have to go ask everyone to give accurate numbers.
      #New theory, I think game 1 always has player1 running, at least for the first 5 or so I
      #looked at.
      if (g1w == "flatline") {
        #Corp 2 won by flatline (??) This may not be correct.
        data[data$id==g$player2$id,"corp.flatline"] <- data[data$id==g$player2$id,"corp.flatline"] + 1
        data[data$id==g$player1$id,"runner.flatline"] <- data[data$id==g$player1$id,"runner.flatline"] + 1
      } else if (g2w == "flatline") {
        #Corp 1 won by flatline (??) This may not be correct.
        data[data$id==g$player1$id,"corp.flatline"] <- data[data$id==g$player1$id,"corp.flatline"] + 1
        data[data$id==g$player2$id,"runner.flatline"] <- data[data$id==g$player2$id,"runner.flatline"] + 1
      }
      
      
      #print(paste("Both corps won"))
      #print(paste("  game 1 by", g1w))
      #print(paste("  game 2 by", g2w))
      #if ((g1w == "flatline") || (g2w == "flatline")) {
      #  print(paste("  player1 Name:", data[data$id==g$player1$id,"name"]))
      #  print(paste("  player1 Corp:", p1_corp))
      #  print(paste("  player2 Name:", data[data$id==g$player2$id,"name"]))
      #  print(paste("  player2 Corp:", p2_corp))
      #  print(paste("  game1: "))
      #}
    }
    return(data)
  }
  
  nplayers <- length(json_data$players)
  player_df$corp.prestige <- rep(0,nplayers)
  player_df$corp.flatline <- rep(0,nplayers)
  player_df$corp.win <- rep(0,nplayers)
  player_df$runner.prestige <- rep(0,nplayers)
  player_df$runner.flatline <- rep(0,nplayers)
  player_df$runner.win <- rep(0,nplayers)
  for (r in json_data$rounds) {
    for (g in r) {
      if(!g$eliminationGame) {
        player_df <- add_points(player_df, g$player1)
        player_df <- add_points(player_df, g$player2)
        player_df <- add_wins(player_df, g)
        player_df <- add_flatlines(player_df, g)
      }
    } 
  } 
  # Hey Matt, something broke in here with the changes I made and I wasn't using the 
  #faction, so I've commented it out for now.
#   corp.assign <- function(corp){
#     ifelse(corp == "Near-Earth Hub" | corp == "Making News" | corp == "The World is Yours*", "NBN",
#            ifelse(corp == "Replicating Perfection" | corp == "Personal Evolution" | 
#                     corp == "Tennin Institute" | corp == "Harmony Medtech" | corp == "Nisei Division", "Jinteki",
#                   ifelse(corp == "Engineering the Future" | corp == "NEXT Design" | corp == "Cerebral Imaging", "Haas-Bioroid",
#                          ifelse(corp == "GRNDL" | corp == "Building a Better World" | corp == "Because We Built It", "Weyland", "Unknown"))))
#   }
#   
#   for(i in 1:n){
#     cfaction <- corp.assign(player_df$corp[i])
#     player_df$corpf[i]<- cfaction
#   }
#   
#   run.assign <- function(runner){
#     ifelse(runner == 'Andromeda' | runner == 'Gabriel Santiago' | runner == 'Silhouette' | 
#              runner == 'Ken "Express" Tenma' | runner == 'Iain Stirling', 'Criminal',
#            ifelse(runner == 'Chaos Theory' | runner == 'Kate "Mac" McCaffrey' | 
#                     runner == 'Rielle "Kit" Peddler' | runner == 'Nasir Meidan' | runner == 'Exile', 'Shaper',
#                   ifelse(runner == 'Whizzard' | runner == 'Reina Roja' | runner == 'Noise', 'Anarch', 'Unknown')))
#   }
#   
#   for(i in 1:n){
#     rfaction <- run.assign(player_df$runner[i])
#     player_df$runf[i]<- rfaction
#   }
#   
#   player_df$corpf <- as.factor(player_df$corpf)
#   player_df$runf <- as.factor(player_df$runf)
  
  return(player_df)
}

player_df <- build_player_data_frame("data/Nationals-140816/NRTM.json")

#Gather Tables
#Corp
countcp <- tapply(rep(1,length(player_df$corp)), player_df$corp, sum)
countcft <- tapply(player_df$corp.flatline, player_df$corp, sum)
meancp <- tapply(player_df$corp.prestige, player_df$corp, mean)

#Runner
countrp <- tapply(rep(1,length(player_df$runner)), player_df$runner, sum)
countrft <- tapply(player_df$runner.flatline, player_df$runner, sum)
meanrp <- tapply(player_df$runner.prestige, player_df$runner, mean)

allcounts <- c(countcp,countrp)
mean.prestige <- c(meancp, meanrp)
count.flatlines <- c(countcft, countrft)
identity_df <- data.frame(allcounts, mean.prestige, count.flatlines)
names(identity_df) <- c("Number.of.Players", "Average.Prestige", "Number.of.Flatlines")
identity_df$Identity <- row.names(identity_df)
faction_colours <- c('Because We Built It'='darkgreen',
                     'Building a Better World'='darkgreen',
                     'Cerebral Imaging'='purple',
                     'Engineering the Future'='purple',
                     'GRNDL'='darkgreen',
                     'Harmony Medtech'='red',
                     'Making News'='yellow3',
                     'Near-Earth Hub'='yellow3',
                     'NEXT Design'='purple',
                     'Nisei Division'='red',
                     'Personal Evolution'='red',
                     'Replicating Perfection'='red',
                     'Stronger Together'='purple',
                     'Tennin Institute'='red',
                     'The World is Yours*'='yellow3',
                     'Andromeda'='blue',
                     'Chaos Theory'='green',
                     'Exile'='green',
                     'Gabriel Santiago'='blue',
                     'Iain Stirling'='blue',
                     'Kate "Mac" McCaffrey'='green',
                     'Ken "Express" Tenma'='blue',
                     'Nasir Meidan'='green',
                     'Noise'='orange',
                     'Reina Roja'='orange',
                     'Rielle "Kit" Peddler'='green',
                     'Silhouette'='blue',
                     'Whizzard'='orange')

#Some information about colour pairings
n <- nrow(player_df)
#Plotting the specifics for Regionals 401 Games 2014
#Jittering some of the ids manually
#Both at 4 points
identity_df['NEXT Design',]$Average.Prestige <- 4.15
identity_df['Rielle "Kit" Peddler',]$Average.Prestige <- 3.85
#Both at 6 points
identity_df['The World is Yours*',]$Average.Prestige <- 6.15
identity_df['Cerebral Imaging',]$Average.Prestige <- 5.85

#pdf("image.pdf", width=24, height=10)
png("plots/Nationals-140816/identities_vs_presitge_all_players.png", width=24, height=10, units="in", res=72)
p2 <- ggplot(identity_df, aes(as.numeric(Number.of.Players), Average.Prestige, label=rownames(identity_df)))
p2 <- p2 + scale_x_continuous(breaks=seq(1, 18, 1)) + scale_y_continuous(breaks=seq(0, 12, 1))
p2 <- p2 + geom_text(aes(colour=Identity, vjust=1.5)) + scale_colour_manual(guide=FALSE, values=faction_colours) 
p2 <- p2 + geom_point(aes(colour = Identity))
p2 <- p2 + labs(title = "Canadian Netrunner Nationals 2014\nAug 16-17 2014, 60 Players with Cards up to Upstalk", 
                x="Number of Players", y = "Average Prestige")
p2 <- p2 + theme_bw()
plot(p2)
dev.off()

# Mosaic Plot to look at interaction between faction choices:
#mosaicplot(~ player_df$corpf + player_df$runf, shade = TRUE, xlab = "Corporation Faction", ylab = "Runner Faction", main = "Faction Choice Combinations at\n the 401 Regionals June 2014")

# Some interesting trends, but nothing substantial (or significant).  More Anarch players brought HB/Weyland, and fewer Jinteki/NBN. Criminals and Shapers mainly concentrated on Jinteki/NBN.


tournament_results <- player_df
tournament_ids <- identity_df

corp_wins <- sum(tournament_results$corp.win)
corp_flatlines <- sum(tournament_results$corp.flatline)
runner_wins <- sum(tournament_results$runner.win)
swiss_rounds <- 6
total_games <- n*swiss_rounds

corp_win_pct <- sprintf("%2.0f%%",100*corp_wins/total_games)
runner_win_pct <- sprintf("%2.0f%%",100*runner_wins/total_games)

print(paste("Total Possible Games:",total_games))
print(paste("Corporation AP Wins:", corp_wins-corp_flatlines))
print(paste("Corporation Flatlines:", corp_flatlines))
print(paste("Runner Wins:", runner_wins))
print("  Any missing games are either ties or byes.")
print(paste("Corporation Wins", corp_win_pct))
print(paste("Runner Wins", runner_win_pct))



#We played a top-16, so lets pull out the stats of the top 16 players
player_df <- head(player_df,16)

#Gather Tables
#Corp
countcp <- tapply(rep(1,length(player_df$corp)), player_df$corp, sum)
countcft <- tapply(player_df$corp.flatline, player_df$corp, sum)
meancp <- tapply(player_df$corp.prestige, player_df$corp, mean)

#Runner
countrp <- tapply(rep(1,length(player_df$runner)), player_df$runner, sum)
countrft <- tapply(player_df$runner.flatline, player_df$runner, sum)
meanrp <- tapply(player_df$runner.prestige, player_df$runner, mean)

allcounts <- c(countcp,countrp)
mean.prestige <- c(meancp, meanrp)
count.flatlines <- c(countcft, countrft)
identity_df <- data.frame(allcounts, mean.prestige, count.flatlines)
names(identity_df) <- c("Number.of.Players", "Average.Prestige", "Number.of.Flatlines")
identity_df$Identity <- row.names(identity_df)

#Next 4 ids are at 8
identity_df['Gabriel Santiago',]$Average.Prestige <- 8.30
identity_df['Whizzard',]$Average.Prestige <- 8.1
identity_df['Harmony Medtech',]$Average.Prestige <- 7.9
identity_df['Making News',]$Average.Prestige <- 7.70
#Both at 6 points
identity_df['Rielle "Kit" Peddler',]$Average.Prestige <- 6.15
identity_df['Personal Evolution',]$Average.Prestige <- 5.85
#identity_df['Exile',]$Average.Prestige <- 2.15
#identity_df['Nasir Meidan',]$Average.Prestige <- 1.85
#identity_df['The World is Yours*',]$Average.Prestige <- 6.15
#identity_df['Cerebral Imaging',]$Average.Prestige <- 5.85

n <- nrow(player_df)
png("plots/Nationals-140816/identities_vs_presitge_top16.png", width=24, height=10, units="in", res=72)
p2 <- ggplot(identity_df, aes(as.numeric(Number.of.Players), Average.Prestige, label=rownames(identity_df)))
p2 <- p2 + scale_x_continuous(breaks=seq(0, 8, 1)) + scale_y_continuous(breaks=seq(0, 12, 1))
p2 <- p2 + geom_text(aes(colour=Identity, vjust=1.5)) + scale_colour_manual(guide=FALSE, values=faction_colours) 
p2 <- p2 + geom_point(aes(colour = Identity))
p2 <- p2 + labs(title = "Top 16 - Canadian Netrunner Nationals 2014\nAug 16-17 2014, 60 Players with Cards up to Upstalk", 
                x="Number of Players", y = "Average Prestige")
p2 <- p2 + theme_bw()
plot(p2)
dev.off()
top16_results <- player_df
top16_ids <- identity_df