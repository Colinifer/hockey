library(jsonlite)
library(stringr)
library(rvest)
library(readxl)
#the id of games
g_num = vector(mode="numeric",length=0)
date <- seq(as.Date("2020-08-05"), by = "day", length.out = 1)#adjust the date 1
for (i in 1:length(date)){
  web1<-paste("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",date[i],"&endDate=",date[i],"&hydrate=team(leaders(categories=[points,goals,assists],gameTypes=[P])),linescore,broadcasts(all),tickets,game(content(media(epg),highlights(scoreboard)),seriesSummary),radioBroadcasts,metadata,seriesSummary(series),decisions,scoringplays&leaderCategories=points,goals,assists&site=en_nhl&teamId=&gameType=&timecode=",sep = "")  
  games<-read_json(web1, simplifyVector = TRUE)
  g_num = c(g_num,games$dates$games[[1]]$gamePk)
}

#data for each season
for (n in 1:length(g_num)){
  NHL <- matrix(0,40,116)
  NHL_template <- read_excel("NHL Template.xlsx", sheet = "NHL_boxscore_template", range = "A2:DL2", col_names = FALSE)
  # colnames(NHL) <-
  #   c(
  #     "game_summary_url",
  #     "season",
  #     "game_type",
  #     "g_num",
  #     "date",
  #     "away_team",
  #     "away_goals",
  #     "home_team",
  #     "home_goals",
  #     "away_shots",
  #     "away_power_play_goals",
  #     "away_power_play_opportunities",
  #     "away_pim",
  #     "home_shots",
  #     "home_power_play_goals",
  #     "home_power_play_opportunities",
  #     "home_pim",
  #     "away_shots_on_goal_per_1",
  #     "away_shots_on_goal_per_2",
  #     "away_shots_on_goal_per_3",
  #     "away_shots_on_goal_so",
  #     "away_shots_on_goal",
  #     "home_shots_on_goal_per_1",
  #     "home_shots_on_goal_per_2",
  #     "home_shots_on_goal_per_3",
  #     "home_shots_on_goal_so",
  #     "home_shots_on_goal",
  #     "away_power_play_percentage",
  #     "away_hits",
  #     "away_faceoff_percentage",
  #     "away_giveaways",
  #     "away_takeaways",
  #     "away_blocked",
  #     "away_pim",
  #     "home_power_play_percentage",
  #     "home_hits",
  #     "home_faceoff_percentage",
  #     "home_giveaways",
  #     "home_takeaways",
  #     "home_blocked",
  #     "home_pim",
  #     "official_1",
  #     "official_2",
  #     "official_3",
  #     "official_4",
  #     "away_coach",
  #     "home_coach"
  #     
  #   )
  colnames(NHL) <- NHL_template
  web2<- paste("https://statsapi.web.nhl.com/api/v1/game/",g_num[n],"/feed/live?site=en_nhl",sep = "")
  players<-read_json(web2, simplifyVector = TRUE)
  
  NHL[,1] = paste('http://www.nhl.com/scores/htmlreports/20192020/GS', substr(g_num[n],5,10), ".HTM",sep = '')#adjust the date 2
  NHL[,2] = players$gameData$game$season
  NHL[,3] = players$gameData$game$type
  NHL[,4] = substr(players$gamePk[1],5,10)
  NHL[,5] = substr(players$gameData$datetime$dateTime,1,10)
  NHL[,6] = players$gameData$teams$away$name
  NHL[,7] = players$liveData$linescore$teams$away$goals
  NHL[,8] = players$gameData$teams$home$name
  NHL[,9] = players$liveData$linescore$teams$home$goals
  NHL[,10] = players$liveData$linescore$currentPeriodOrdinal
  NHL[,13] = players$liveData$linescore$teams$away$shotsOnGoal
  NHL[,14] = players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$powerPlayGoals
  NHL[,15] = players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$powerPlayOpportunities 
  NHL[,16] = players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$pim
  NHL[,17] = players$liveData$linescore$teams$home$shotsOnGoal
  NHL[,18] = players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$powerPlayGoals
  NHL[,19] = players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$powerPlayOpportunities 
  NHL[,20] = players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$pim
  NHL[,22] = players$liveData$linescore$periods$away$shotsOnGoal[1]
  NHL[,23] = players$liveData$linescore$periods$away$shotsOnGoal[2]
  NHL[,24] = players$liveData$linescore$periods$away$shotsOnGoal[3]
  NHL[,25] = if(players$liveData$linescore$hasShootout == TRUE){players$liveData$linescore$periods$away$shotsOnGoal[4]}else{0} 
  NHL[,26] = players$liveData$linescore$teams$away$shotsOnGoal
  NHL[,27]  = players$liveData$linescore$periods$home$shotsOnGoal[1]
  NHL[,28] = players$liveData$linescore$periods$home$shotsOnGoal[2]
  NHL[,29] = players$liveData$linescore$periods$home$shotsOnGoal[3]
  NHL[,30] = if(players$liveData$linescore$hasShootout == TRUE){players$liveData$linescore$periods$home$shotsOnGoal[4]}else{0} 
  NHL[,31] = players$liveData$linescore$teams$home$shotsOnGoal
  NHL[,32] = paste(players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$powerPlayGoals,"/",players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$powerPlayOpportunities)
  NHL[,33] = players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$hits
  NHL[,34] = if(is.null(players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$faceOffWinPercentage)){NA}else{players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$faceOffWinPercentage}
  NHL[,35] = players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$giveaways
  NHL[,36] = players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$takeaways
  NHL[,37] = players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$blocked
  NHL[,38] = players$liveData$boxscore$teams$away$teamStats$teamSkaterStats$pim
  NHL[,39] = paste(players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$powerPlayGoals,"/",players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$powerPlayOpportunities)
  NHL[,40] = players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$hits
  NHL[,41] = if(is.null(players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$faceOffWinPercentage)){NA}else{players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$faceOffWinPercentage}
  NHL[,42] = players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$giveaways
  NHL[,43] = players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$takeaways
  NHL[,44] = players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$blocked
  NHL[,45] = players$liveData$boxscore$teams$home$teamStats$teamSkaterStats$pim
  NHL[,46] = if(is.null(players$liveData$boxscore$officials$official$fullName[1])){NA}else{players$liveData$boxscore$officials$official$fullName[1]}
  NHL[,47] = if(is.null(players$liveData$boxscore$officials$official$fullName[2])){NA}else{players$liveData$boxscore$officials$official$fullName[2]}
  NHL[,48] = if(is.null(players$liveData$boxscore$officials$official$fullName[3])){NA}else{players$liveData$boxscore$officials$official$fullName[3]}
  NHL[,49] = if(is.null(players$liveData$boxscore$officials$official$fullName[4])){NA}else{players$liveData$boxscore$officials$official$fullName[4]}
  team<-c("away","home")
  for (j in 1:2){
    NHL[,49+j] = if (is.null(players$liveData$boxscore$teams[[team[j]]]$coaches$person$fullName)){NA}else{players$liveData$boxscore$teams$away$coaches$person$fullName} 
  }
  
  k = 0
  for (j in 1:2){
    #scratches
    sc_n <- players$liveData$boxscore$teams[[team[j]]]$scratches
    sc_no <- if(length(sc_n)>9){paste("ID", sc_n[1:9], sep = "")}else{paste("ID", sc_n, sep = "")}
    if (length(sc_n) != 0){
      for (i in 1:length(sc_no)){#
        NHL[,51+18*(j-1)+i] = if(is.null(players$gameData$players[[sc_no[i]]]$fullName)){NA}else{players$gameData$players[[sc_no[i]]]$fullName}
        NHL[,60+18*(j-1)+i] = paste("http://www.nhl.com/ice/player.htm?id=",players$gameData$players[[sc_no[i]]]$id,sep = "")
      }
    }
    
    #skaters
    sk_n <- players$liveData$boxscore$teams[[team[j]]]$skaters
    sk_num <- length(sk_n) - length(sc_n)
    sk_no <- paste("ID", sk_n[1:sk_num], sep = "")
    if (sk_num != 0){
      for (i in 1:length(sk_no)){
        NHL[i+(length(sk_no)+2)*(j-1),87+j] = 1
        NHL[i+(length(sk_no)+2)*(j-1),90] = 1
        NHL[i+(length(sk_no)+2)*(j-1),92] = paste("http://www.nhl.com/ice/player.htm?id=",players$gameData$players[[sk_no[i]]]$id,sep = "")
        NHL[i+(length(sk_no)+2)*(j-1),93] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$jerseyNumber)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$jerseyNumber}
        NHL[i+(length(sk_no)+2)*(j-1),94] = if(is.null(players$gameData$players[[sk_no[i]]]$fullName)){NA}else{players$gameData$players[[sk_no[i]]]$fullName}
        NHL[i+(length(sk_no)+2)*(j-1),95] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$position$code)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$position$code}
        NHL[i+(length(sk_no)+2)*(j-1),96] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$goals)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$goals}
        NHL[i+(length(sk_no)+2)*(j-1),97] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$assists)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$assists}
        NHL[i+(length(sk_no)+2)*(j-1),98] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$powerPlayGoals)){0}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$powerPlayGoals}+if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$powerPlayAssists)){0}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$powerPlayAssists}
        NHL[i+(length(sk_no)+2)*(j-1),99] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$plusMinus)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$plusMinus}
        NHL[i+(length(sk_no)+2)*(j-1),100] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$penaltyMinutes)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$penaltyMinutes}
        NHL[i+(length(sk_no)+2)*(j-1),101] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$shots)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$shots}
        NHL[i+(length(sk_no)+2)*(j-1),102] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$hits)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$hits}
        NHL[i+(length(sk_no)+2)*(j-1),103] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$blocked)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$blocked}
        NHL[i+(length(sk_no)+2)*(j-1),104] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$giveaways)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$giveaways}
        NHL[i+(length(sk_no)+2)*(j-1),105] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$takeaways)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$takeaways}
        NHL[i+(length(sk_no)+2)*(j-1),106] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$faceOffWins)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$faceOffWins}
        NHL[i+(length(sk_no)+2)*(j-1),107] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$powerPlayTimeOnIce)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$powerPlayTimeOnIce}
        NHL[i+(length(sk_no)+2)*(j-1),108] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$shortHandedTimeOnIce)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$shortHandedTimeOnIce}
        NHL[i+(length(sk_no)+2)*(j-1),109] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$timeOnIce)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[sk_no[i]]]$stats$skaterStats$timeOnIce}
      }
    }
    k = k + (length(sk_no))
    #goalies
    g_n <- players$liveData$boxscore$teams[[team[j]]]$goalies
    g_no <- paste("ID", g_n, sep = "")
    NHL[,10+j] = if(is.null(players$gameData$players[[g_no[1]]]$fullName)){NA}else{players$gameData$players[[g_no[1]]]$fullName}
    if (length(g_n) != 0){
      for (i in 1:length(g_no)){
        NHL[k+i,87+j] = 1
        NHL[k+i,91] = 1
        NHL[k+i,92] = paste("http://www.nhl.com/ice/player.htm?id=",players$gameData$players[[g_no[i]]]$id,sep = "")
        NHL[k+i,93] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$jerseyNumber)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$jerseyNumber}
        NHL[k+i,94] = if(is.null(players$gameData$players[[g_no[i]]]$fullName)){NA}else{players$gameData$players[[g_no[i]]]$fullName}
        NHL[k+i,95] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$position$code)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$position$code}
        NHL[k+i,110] = paste(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$evenSaves,"-",players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$evenShotsAgainst) 
        NHL[k+i,111] = paste(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$powerPlaySaves,"-",players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$powerPlayShotsAgainst) 
        NHL[k+i,112] = paste(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$shortHandedSaves,"-",players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$shortHandedShotsAgainst) 
        NHL[k+i,113] = paste(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$saves,"-",players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$shots) 
        NHL[k+i,114] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$shots) || players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$shots==0){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$savePercentage*0.01}
        NHL[k+i,115] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$pim)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$pim}
        NHL[k+i,116] = if(is.null(players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$timeOnIce)){NA}else{players$liveData$boxscore$teams[[team[j]]]$players[[g_no[i]]]$stats$goalieStats$timeOnIce}
      }
    }  
    k = k+2
  }
  if (length(sc_n)+length(sk_n)+length(g_n)!=0){
    web3<-paste("http://www.nhl.com/scores/htmlreports/20192020/GS",substr(g_num[n],5,10),".HTM",sep = "")#adjust the date 3
    web3<-read_html(web3)
    summary <- web3 %>% html_nodes("table") 
    summary_td<-summary %>% html_nodes("td")
    attendance<-summary_td[15]%>%html_text()
    NHL[,21] = if(length(str_extract_all(attendance, "[0-9]+,[0-9]+")[[1]])){str_extract_all(attendance, "[0-9]+,[0-9]+")[[1]]}else{NA}
    
  }
  
  write.csv(NHL,file=paste(g_num[n],".csv",sep = ""))#adjust the date 4
}