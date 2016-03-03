library(dplyr)
library(tidyr)
library(jsonlite)

# Program flow
# Get [my] summoner Id
# Get match history
#   get match types (to filter out ARAM)
#   get champions played
#   get match summaries
#   get match details (with timelines? or will the delta summary suffice?)

# Dev notes:
#   API Key: 9bca8310-2406-4720-afb4-9d8f7740e36d
#   10 requests every 10 seconds
#   500 requests every 10 minutes

getChampionInfo <- function() {    
    BASE <- paste("http://ddragon.leagueoflegends.com/cdn"
            , "/6.3.1/data/en_US/champion.json"
            , sep=""
    )
    
    requestURL <- BASE
    responseBody <- fromJSON(requestURL)
    
    champions <- data.frame(stringsAsFactors=FALSE)
    stats <- data.frame(stringsAsFactors=FALSE)
    info <- data.frame(stringsAsFactors=FALSE)
    
    for (i in 1:length(responseBody$data)) {
        key <- responseBody$data[[i]]$key
        name <- responseBody$data[[i]]$name
        title <- responseBody$data[[i]]$title
        info <- responseBody$data[[i]]$info
        tag0 <- responseBody$data[[i]]$tags[1]
        tag1 <- responseBody$data[[i]]$tags[2]
        stats <- responseBody$data[[i]]$stats
        
        df <- data.frame(key, name, title, info, tag0, tag1, stats
                         , stringsAsFactors=FALSE)
        champions <- rbind(champions, df)
    }
    
    tbl_df(champions)
}

getSummonerId <- function() {
    # This function gets the summoner id given a name to search for
    
    BASE <- "https://na.api.pvp.net/api/lol/na"
    callType <- "/v1.4/summoner/by-name"
    summonerName <- "/Ediot"
#     summonerName <- paste("/", name, sep="")
    
    requestURL <- paste(BASE, callType, summonerName, "?", API_KEY, sep="")
    responseBody <- fromJSON(requestURL)
    
    df <- data.frame(responseBody)
    tbl_df(df)
}

getRankedMatchHistory <- function() {
    # This function returns a data frame containing a list of matches
    
    BASE <- "https://na.api.pvp.net/api/lol/na"
    callType <- "/v2.2/matchlist/by-summoner"
    # summonerId <- paste("/", summId, sep="")
    summonerId <- paste("/", 23375925, sep="")
    
    requestURL <- paste(BASE, callType, summonerId, "?", API_KEY, sep="")
    responseBody <- fromJSON(requestURL)
    
    df <- data.frame(responseBody)
    tbl_df(df)
}

getRecentMatchHistory <- function() {
    # attempting to get recent match history
    
    BASE <- "https://na.api.pvp.net/api/lol/na"
    callType <- "/v1.3/game/by-summoner"
    # summonerId <- paste("/", summId, sep="")
    summonerId <- paste("/", 23375925, sep="")
    
    requestURL <- paste(BASE, callType, summonerId, "/recent", "?", API_KEY
                        , sep="")
    responseBody <- fromJSON(requestURL)
    
    names(responseBody$games)
    
    df <- tbl_df(
        responseBody$games %>%
            select(-fellowPlayers, -stats)
    )
    df
}

getMatchDetails <- function(match) {
    BASE <- "https://na.api.pvp.net/api/lol/na"
    callType <- "/v2.2/match"
    matchId <- paste("/", match, sep="")
    # matchId <- paste("/", 2103540893, sep="")
    optionalParams <- "&includeTimeline=TRUE"
    
    requestURL <- paste(BASE, callType, matchId, "?", API_KEY, optionalParams
                        , sep="")
    responseBody <- fromJSON(requestURL)
    
    teams <- tbl_df(responseBody$teams)

    participants <- responseBody$participants %>%
        select(teamId, spell1Id, spell2Id, championId, participantId)
    
    participants_stats <- responseBody$participants$stats
    
    participant_summary <- data.frame(participants, participants_stats
                    , stringsAsFactors = FALSE)
    
    tbl_df(participant_summary)
    
    #participants_timeline
}

getMatchFile <- function(match) {
    BASE <- "https://na.api.pvp.net/api/lol/na"
    callType <- "/v2.2/match"
    matchId <- paste("/", match, sep="")
    optionalParams <- "&includeTimeline=TRUE"
    desiredFileName <- paste("matches/"
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

getMatchJSON <- function() {
    games <- select(getRecentMatchHistory(), gameId)
    
    for (i in 1:nrow(games)){
        x <- slice(games, i)
        print(x$gameId)
        
        getMatchFile(x$gameId)
        Sys.sleep(1) #sleep for one second
    }
}

main <- function() {
    df <- getChampionInfo()
    
}
