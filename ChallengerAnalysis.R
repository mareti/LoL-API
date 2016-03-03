library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)

# Make sure you initialize API_KEY
source("init.R")

getChallengerList <- function() {
    BASE <- "https://na.api.pvp.net/api/lol/na"
    call <- "/v2.5/league/challenger"
    type <- "?type=RANKED_SOLO_5x5"
    
    requestURL <- paste(BASE, call, type, "&", API_KEY, sep="")
    responseBody <- fromJSON(requestURL)
    
    desiredFileName <- paste("challenger snapshots/" 
                             , toString(today())
                             , " NA.json"
                             , sep=""
                             )
    
    if (!file.exists(desiredFileName)) {
        write(toJSON(responseBody, pretty=TRUE)
              , file=desiredFileName
        )
        print(paste("File", desiredFileName, "has been created"))
    }
    else {
        print(paste("File", desiredFileName, "already exists"))
    }
    
    tbl_df(responseBody$entries)
}

getChallengerMatchFile <- function(match) {
    BASE <- "https://na.api.pvp.net/api/lol/na"
    callType <- "/v2.2/match"
    matchId <- paste("/", match, sep="")
    optionalParams <- "&includeTimeline=TRUE"
    desiredFileName <- paste("challenger matches/"
                             , toString(match)
                             , ".json"
                             , sep=""
    )
    
    requestURL <- paste(BASE, callType, matchId, "?", API_KEY, optionalParams
                        , sep="")
    responseBody <- fromJSON(requestURL)
    
    if (!file.exists(desiredFileName)) {
        write(toJSON(responseBody, pretty=TRUE)
              , file=desiredFileName
        )
        print(paste("File", desiredFileName, "has been created"))
    }
    else {
        print(paste("File", desiredFileName, "already exists"))
    }  
}

getChallengerMatches <- function() {
    BASE <- "https://na.api.pvp.net/api/lol/na"
    call <- "/v1.3/game/by-summoner"

    df <- getChallengerList()
    
    for (i in 1:nrow(df)) {
        x <- slice(df, i)
        summonerId <- toString(x$playerOrTeamId)
        
        requestURL <- paste(BASE, call, "/", summonerId, "/recent", "?", API_KEY
                            , sep="")
        responseBody <- fromJSON(requestURL)
        Sys.sleep(1) #sleep for one second
        
        recentChallengerMatches <- responseBody$games %>%
                select(-fellowPlayers, -stats)
        
        for (j in 1:nrow(recentChallengerMatches)) {
            y <- slice(recentChallengerMatches, j)
            matchId <- toString(y$gameId)
            getChallengerMatchFile(matchId)
            Sys.sleep(1) #sleep for one second
        }
    }
}