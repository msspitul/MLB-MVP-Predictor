################################################################################
### Matthew Spitulnik ##########################################################
### Applied Machine Learning ###################################################
### MLB MVP Predictor ##########################################################
### Project Summary: Used data mining techniques in R to collect advanced stats 
# from tens of thousands of individual MLB player’s seasons, then created 
# machine learning algorithms for predicting if an MLB player would receive an 
# MVP vote. ####################################################################
################################################################################

################################################################################
###Install and load required packages###########################################
################################################################################

###Install Required Packages
#install.packages('tidyverse')
#install.packages('XML')
#install.packages('RCurl')
#install.packages('httr')
#install.packages('stringr')
#install.packages('dplyr')
#install.packages('rpart')
#install.packages('caret')
#install.packages('tree')
#install.packages('naivebayes')
#install.packages('e1071')
#install.packages('rattle')
#install.packages('rpart.plot')

###Load required packages
library(tidyverse)
library(XML)
library(RCurl)
library(httr)
library(stringr)
library(dplyr)
library(rpart)
library(caret)
library(tree)
library(naivebayes)
library(e1071)
library(rattle)
library(rpart.plot)

################################################################################
###Import the data sets that will be used throughout the code###################
################################################################################

###This creates a "default_directory" variable, where the directory path to the 
# data files folder containing all of the required data sets is saved so that it 
# does not need to be constantly re-entered. Remember to use forward slashes 
# instead of back slashes in the directory path. For example, if the datafiles 
# folder is saved in "C:\home\project\datafiles", then "C:/home/project" 
# would be inserted between the quotes below.
#default_directory <- "<DEFAULT DIRECTORY PATH HERE>"

###The various data sets that will be used throughout the code are imported now.

#import pitcher, player, and batter list
PitchStats <- read_csv(str_glue("{default_directory}/datafiles/Pitching.csv"))
BatStats <- read_csv(str_glue("{default_directory}/datafiles/Batting.csv"))
Players <- read_csv(str_glue("{default_directory}/datafiles/People.csv"))

#import the MVP info so the For loop used to collect the info from 
# baseballreference.com doesn't have to be run again
ALMVPTotal <- read_csv(str_glue("{default_directory}/datafiles/ALMVPTotal.csv"))
NLMVPTotal <- read_csv(str_glue("{default_directory}/datafiles/NLMVPTotal.csv"))
MLBMVPTotal <- read_csv(str_glue("{default_directory}/datafiles/MLBMVPTotal.csv"))

#import WAR list to avoid having to run the For loop used to collect the info 
# from baseballreference.com
DFPBWARTotal <- read_csv(str_glue("{default_directory}/datafiles/DFPBWARTotal.csv"))

#import manualMVPCheck which was used as part of cleanup
ManualMVPCheck <- read_csv(str_glue("{default_directory}/datafiles/ManualMVPCheck.csv"))

#list of only players that won MVP, no MVP vote getters included
MVPOnly <- read_csv(str_glue("{default_directory}/datafiles/MVPOnly.csv"))

#import data used for the 2022 test
DF2022_1 <- read_csv(str_glue("{default_directory}/datafiles/StandBat2022.csv"))
DF2022_2 <- read_csv(str_glue("{default_directory}/datafiles/ValBatting2022.csv"))
DF2022_3 <- read_csv(str_glue("{default_directory}/datafiles/StandPitch2022.csv"))
DF2022_4 <- read_csv(str_glue("{default_directory}/datafiles/ValPitching2022.csv"))

################################################################################
###Collect and Clean Required Data##############################################
################################################################################

###function to download all of the MVP stats from  baseballreference.com into 
# individual data frames (DF), then combine them into one overall DF for each league
#IMPORTANT NOTE# This code took a while to run, so once it finished I 
# exported the CSV files so that they can just be imported going forward. 
# There is code to import them at the top of the page already, files are 
# NLMVPTotal, ALMVPTotal, and MLBMVPTotal.
# for (i in 1911:2021){
#   url <- str_glue('https://www.baseball-reference.com/awards/awards_{i}.shtml#AL_MVP_voting_link')
#   urldata <- GET(url)
#   CurrentMVPList <- readHTMLTable(rawToChar(urldata$content),
#                                   stringsAsFactors = FALSE)
#   CurrentMVPList$AL_MVP_voting$Year <- i
#   CurrentMVPList$NL_MVP_voting$Year <- i
#   assign(paste0('ALMVP',i), CurrentMVPList$AL_MVP_voting)
#   assign(paste0('NLMVP',i), CurrentMVPList$NL_MVP_voting)
#   if (!exists("ALMVPTotal") && !exists("NLMVPTotal")){
#     ALMVPTotal <- CurrentMVPList$AL_MVP_voting
#     NLMVPTotal <- CurrentMVPList$NL_MVP_voting
#   }
#   else {
#     if (typeof(CurrentMVPList[["AL_MVP_voting"]][["Rank"]])=="NULL"){
#       rm(list=(paste0('ALMVP',i)))
#     }
#     else{
#       ALMVPTotal <- rbind(ALMVPTotal, CurrentMVPList$AL_MVP_voting)
#       CurrentMVPList$AL_MVP_voting$League="AL"
#     }
#     if (typeof(CurrentMVPList[["NL_MVP_voting"]][["Rank"]])=="NULL"){
#       rm(list=(paste0('NLMVP',i)))
#     }
#     else{
#       NLMVPTotal <- rbind(NLMVPTotal, CurrentMVPList$NL_MVP_voting)
#     }}
# }
# #export the MVPlists so that the loop to collect MVP info does not have to be run again
# write.csv(ALMVPTotal,str_glue("{default_directory}/datafiles/ALMVPTotal.csv"), row.names = FALSE)
# write.csv(NLMVPTotal,str_glue("{default_directory}/datafiles/NLMVPTotal.csv"), row.names = FALSE)

#Check for duplicate entries (player has MVP caliber season but switches teams during it)
ALMVPTotalDupCheck <- ALMVPTotal[,c('Name','Year')]
ALMVPTotalDupCheck[duplicated(ALMVPTotalDupCheck),]#0 duplicates
NLMVPTotalDupCheck <- NLMVPTotal[,c('Name','Year')]
NLMVPTotalDupCheck[duplicated(NLMVPTotalDupCheck),]#0 duplicates

#Combine AL and NL
MLBMVPTotal <- rbind(ALMVPTotal, NLMVPTotal)
str(MLBMVPTotal)
MLBMVPTotal[MLBMVPTotal==""]<-0
MLBMVPTotal[MLBMVPTotal==""]
str(MLBMVPTotal)

#export the MVPlist so that the loop to collect MVP info does not have to be run again
#write.csv(MLBMVPTotal,str_glue("{default_directory}/datafiles/MLBMVPTotal.csv"), row.names = FALSE)

#Convert Columns to appropriate data types
MLBMVPTotalNum <- MLBMVPTotal
MLBMVPTotalNum$Share <- sub("%","",MLBMVPTotalNum$Share)
#When importing the CSV MVP files, the following numeric conversion may not be needed
#MLBMVPTotalNum[,4:32] <- as.numeric(unlist(MLBMVPTotalNum[,4:32]))
MLBMVPTotalNum[,6] <- as.numeric(unlist(MLBMVPTotalNum[,6]))
MLBMVPTotalNum$Share <- MLBMVPTotalNum$Share/100
MLBMVPTotalNum[is.na(MLBMVPTotalNum)]=0
str(MLBMVPTotalNum)

#aggregate players rows for when they moved to a new team during season
BatStatRed <- BatStats[,-(c(3:5))]
#BatStatRed[is.na(BatStatRed)]=0
AggBatStatRed <- aggregate(.~playerID+yearID, data=BatStatRed,sum, na.action = na.pass) #110495 down to 102198
BatStatsF <- AggBatStatRed

PitchStatsRed <- PitchStats[,-c(3:5)]
#PitchStatsRed[is.na(PitchStatsRed)]=0
AggPitchStatRed <- aggregate(.~playerID+yearID, data=PitchStatsRed,sum, na.action = na.pass) #49430 down to 26725
PitchStatsF <- AggPitchStatRed

BatStatCountCheck <- aggregate(G~playerID, data=BatStats, sum) #20166
PitchStatCountCheck <-aggregate(G~playerID, data=PitchStats, sum) #10199

#Check to see how many rows should be expected once pitcher and batter stats are combined
BatStatsFDiffCheck <- BatStatsF[,c('playerID', 'yearID')]
PitchStatsFDiffCheck <- PitchStatsF[,c('playerID', 'yearID')]
#BatStatsF: 102198
#PitchStatsF:45632

#Many pitchers have also batted, but most batters have not pitched. Take a look
# now at how that affects the different DFs.
DiffInRows19<-anti_join(BatStatsFDiffCheck,PitchStatsFDiffCheck) #56566
DiffInRows20<-anti_join(PitchStatsFDiffCheck,BatStatsFDiffCheck) #0
DiffInRows21<-semi_join(BatStatsFDiffCheck,PitchStatsFDiffCheck)# 45632
DiffInRows22<-semi_join(PitchStatsFDiffCheck,BatStatsFDiffCheck) #45632
#Every pitcher year is in the batter DF, 56566 batter years not in pitcher DF, 
# when we merg the two, there should be same number of rows as the batter DF

#Merge Pitch and Bat Stats
PitchBat <- merge(BatStatsF, PitchStatsF, c('playerID','yearID'), all=TRUE)
str(PitchBat) #102198 rows!

#aggregate on playerID to compare number of players to Player sheet
AggPitchBatCountCheck <- aggregate(G.x~playerID, data=PitchBat, sum) #20166
AggPitchBatCountCheck[duplicated(AggPitchBatCountCheck)] #0 duplicates

#check differences in unique players in Players DF (20370) and PitchBat
DiffInRows23<-anti_join(AggPitchBatCountCheck,Players) #0
DiffInRows24<-anti_join(Players,AggPitchBatCountCheck) #204
DiffInRows25<-semi_join(AggPitchBatCountCheck,Players)# 20166
DiffInRows26<-semi_join(Players,AggPitchBatCountCheck)# 20166
#The 204 extra players in the player list never played games, some look like 
# managers, but don't need them either way.

#No NL MVPs 1915 to 1923, 1930, league ID already removed from PitchBat, 
# user another one to figure out what should be removed to get rid of these years
PitchBatPlayerRemoval <- merge(BatStats, PitchStats, 
                               c('playerID','yearID','stint','teamID', 'lgID'), all=TRUE)

count(PitchBatPlayerRemoval[((PitchBatPlayerRemoval$yearID==1915:1923 | PitchBatPlayerRemoval$yearID==1930) & 
                               PitchBatPlayerRemoval$lgID=="NL"),])#507 players

PitchBatPR1 <- PitchBatPlayerRemoval[((PitchBatPlayerRemoval$yearID==1915:1923 | PitchBatPlayerRemoval$yearID==1930) & 
                                        PitchBatPlayerRemoval$lgID=="NL"),]#507

PitchBatPlayerRemoval <- 
  PitchBatPlayerRemoval[!((PitchBatPlayerRemoval$yearID==1915:1923 | PitchBatPlayerRemoval$yearID==1930) &
                            PitchBatPlayerRemoval$lgID=="NL"),] #109988

#No AL MVPs 1915 to 1921, 29:30, get rid of these
count(PitchBatPlayerRemoval[((PitchBatPlayerRemoval$yearID==1915:1921 | PitchBatPlayerRemoval$yearID==1929:1930) &
                               PitchBatPlayerRemoval$lgID=="AL"),])#523

PitchBatPR2 <- PitchBatPlayerRemoval[((PitchBatPlayerRemoval$yearID==1915:1921 | PitchBatPlayerRemoval$yearID==1929:1930) &
                                        PitchBatPlayerRemoval$lgID=="AL"),] #523

PitchBatPlayerRemoval <- 
  PitchBatPlayerRemoval[!((PitchBatPlayerRemoval$yearID==1915:1921 | PitchBatPlayerRemoval$yearID==1929:1930) &
                            PitchBatPlayerRemoval$lgID=="AL"),] #109465

#102198 in PitchBat to Start
DiffInRowsPR1_1 <-anti_join(PitchBat,PitchBatPR1) #101761
DiffInRowsPR1_2 <-anti_join(PitchBatPR1,PitchBat) #70
DiffInRowsPR1_3 <-semi_join(PitchBat,PitchBatPR1)# 437
DiffInRowsPR1_4 <-semi_join(PitchBatPR1,PitchBat)# 437
PitchBat2<-DiffInRowsPR1_1
str(PitchBat2)

#101761 in PitchBat2 to Start
DiffInRowsPR2_1 <-anti_join(PitchBat2,PitchBatPR2) #101290
DiffInRowsPR2_2 <-anti_join(PitchBatPR2,PitchBat2) #52
DiffInRowsPR2_3 <-semi_join(PitchBat2,PitchBatPR2)# 471
DiffInRowsPR2_4 <-semi_join(PitchBatPR2,PitchBat2)# 471
PitchBat3 <- DiffInRowsPR2_1
str(PitchBat3)

#101290 to start. No MVPs before 1911, get rid of those
PitchBat4 <- (PitchBat3[PitchBat3$yearID>=1911,])
str(PitchBat4) #89763

#get rid of NAs in data
PitchBat4[is.na(PitchBat4)]=0

#want to get total innings pitched, IPouts = innings pitched *3, so reverse that process
PitchStats$InnPitch <- PitchStats$IPouts/3
aggPitchStatsTeam<- aggregate(InnPitch~yearID, data=PitchStats, sum)
aggPitchStatsTeam<-aggPitchStatsTeam[aggPitchStatsTeam$yearID>=1911,]

#now total AB
aggBatStatsTeam<- aggregate(AB~yearID, data=BatStats, sum)
aggBatStatsTeam<-aggBatStatsTeam[aggBatStatsTeam$yearID>=1911,]

#now get all the AB and IP numbers for MVPs as percentages
MLBMVPTotalNum$ABperc <- 100*(MLBMVPTotalNum$AB/aggBatStatsTeam$AB[match(MLBMVPTotalNum$Year,aggBatStatsTeam$yearID)])
MLBMVPTotalNum$IPperc <- 100*(MLBMVPTotalNum$IP/aggPitchStatsTeam$InnPitch[match(MLBMVPTotalNum$Year,aggPitchStatsTeam$yearID)])

#create a cutoff point for who should get caught based on how much they played that year
MLBMVPTotalNum$MVPCutOff <- (MLBMVPTotalNum$ABperc+MLBMVPTotalNum$IPperc)/2
min(MLBMVPTotalNum$MVPCutOff)#0.04545401

#get the IP and AB percent for whole player set now
PitchBat4$InnPitch <- PitchBat4$IPouts/3
PitchBat4$IPperc <- 100*(PitchBat4$InnPitch/aggPitchStatsTeam$InnPitch[match(PitchBat4$yearID,aggPitchStatsTeam$yearID)])
PitchBat4$ABperc <- 100*(PitchBat4$AB/aggBatStatsTeam$AB[match(PitchBat4$yearID,aggBatStatsTeam$yearID)])

#set full player list cutoff
PitchBat4$MVPCutOff <- (PitchBat4$ABperc+PitchBat4$IPperc)/2
count(PitchBat4[PitchBat4$MVPCutOff>=0.04545401,]) #58308

#make data of players below MVP CutOff range to get an idea of whose potentially being removed
PitchBatNotMVPQualify <- PitchBat4[PitchBat4$MVPCutOff<0.04545401,]

#remove players below MVP CutOff range
MVPQualify <- PitchBat4[PitchBat4$MVPCutOff>=0.04545401,]
str(MVPQualify) #58308

#check for duplicates one more time
MVPQualify[duplicated(MVPQualify)] #no duplicates

#aggregate MVPQualify to use next for collecting stats I need to add to the data
AGGMVPQP <- aggregate(G.x~playerID, data=MVPQualify, sum)#11005 players

#Reduce Player list to match # of players in aggregated MVP list (need to use baseball reference ID from the player list)
PlayersRed<-Players
str(PlayersRed)#20370 to start
DiffInRowsPM_1 <-anti_join(PlayersRed,AGGMVPQP) #9365
DiffInRowsPM_2 <-anti_join(AGGMVPQP,PlayersRed) #0
DiffInRowsPM_3 <-semi_join(PlayersRed,AGGMVPQP)# 11005
DiffInRowsPM_4 <-semi_join(AGGMVPQP,PlayersRed)# 11005
PlayersRed <- DiffInRowsPM_3

###getting WAR metric from baseballreference.com
#IMPORTANT NOTE# This code took about 8 hours to run, so once it finished I 
# exported the CSV files so that they can just be imported going forward. 
# There is code to import them at the top of the page already, file name is DFPBWARTotal 
# for (i in 1:nrow(PlayersRed)){
#   #extract first letter of player ID to use in links
#   PlayerIDFirstLetter <- substring(PlayersRed$bbrefID[i],1,1)
#   #Get the pitching WAR
#   url <- str_glue('https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fplayers%2F{PlayerIDFirstLetter}%2F{PlayersRed$bbrefID[i]}.shtml&div=div_pitching_value')
#   urldata <- GET(url)
#   CurrentPlayerPV <- readHTMLTable(rawToChar(urldata$content),stringsAsFactors = FALSE)
#   yearID <- CurrentPlayerPV[["pitching_value"]][["Year"]]
#   PWAR<-CurrentPlayerPV[["pitching_value"]][["WAR"]]
#   DFPWAR <- data.frame(yearID,PWAR)
#   DFPWAR<-na.omit(DFPWAR[!DFPWAR=="",])
#   #get batting WAR
#   url <- str_glue('https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fplayers%2F{PlayerIDFirstLetter}%2F{PlayersRed$bbrefID[i]}.shtml&div=div_batting_value')
#   urldata <- GET(url)
#   CurrentPlayerBV <- readHTMLTable(rawToChar(urldata$content),stringsAsFactors = FALSE)
#   yearID <- CurrentPlayerBV[["batting_value"]][["Year"]]
#   BWAR<-CurrentPlayerBV[["batting_value"]][["WAR"]]
#   DFBWAR <- data.frame(yearID,BWAR)
#   DFBWAR<-na.omit(DFBWAR[!DFBWAR=="",])
#   #create work around if player has never pitched or never batted so one of the DFWARs is empty
#   if (length(CurrentPlayerPV)==0 | length(CurrentPlayerBV)==0){
#     if (length(CurrentPlayerPV)==0){
#       PWAR=NA
#       yearID <- CurrentPlayerBV[["batting_value"]][["Year"]]
#       DFPWAR <- data.frame(yearID,PWAR)
#       DFPWAR<-na.omit(DFPWAR[!DFPWAR=="",])
#     }
#     else{
#       BWAR=NA
#       yearID <- CurrentPlayerPV[["pitching_value"]][["Year"]]
#       DFBWAR <- data.frame(yearID,BWAR)
#       DFBWAR<-na.omit(DFBWAR[!DFBWAR=="",])
#     }}
#   #create the merged and combined DFs
#   DFPBWAR <- merge(DFBWAR, DFPWAR, 'yearID', all=TRUE)
#   DFPBWAR$playerID <- PlayersRed$bbrefID[i]
#   #add to overall player WARs
#   if (!exists("DFPBWARTotal")) {
#     DFPBWARTotal<-DFPBWAR}
#   else{
#     DFPBWARTotal <- rbind(DFPBWARTotal, DFPBWAR)
#   }}
#export the WARList so that the loop to collect WAR does not have to be run again
#write.csv(DFPBWARTotal,str_glue("{default_directory}/datafiles/DFPBWARTotal.csv"), row.names = FALSE)
str(DFPBWARTotal)

#Check to see if it worked, should be 11005 unique playerIDs when done, and there is!
AggDFPBWARTotal <- aggregate(as.numeric(BWAR)~playerID, data=DFPBWARTotal, sum, na.action = na.pass)

#some players that had seasons before and after 1911 had WAR records downloaded for before 1911, so removing those
DFPBWARTotalRed <- DFPBWARTotal
DFPBWARTotalRed<-DFPBWARTotalRed[DFPBWARTotalRed$yearID>=1911,]
AggDFPBWARTotalRed <- aggregate(as.numeric(BWAR)~playerID, data=DFPBWARTotalRed, sum, na.action = na.pass) #still 11005 unique players

#remove na
DFPBWARTotalRed[is.na(DFPBWARTotalRed)]=0
DFPBWARTotalRed[,2] <- as.numeric(DFPBWARTotalRed$BWAR)#depending on how the data is imported, may already be numeric
DFPBWARTotalRed[,3] <- as.numeric(DFPBWARTotalRed$PWAR)#depending on how the data is imported, may already be numeric

#players that switched teams mid season have multiple fields, need to aggregate those
AggDFPBWARTotalRed <- aggregate(.~playerID +yearID, data=DFPBWARTotalRed, mean, na.action = na.pass)

#create total WAR, so PWAR +BWAR
AggDFPBWARTotalRed$WAR <- AggDFPBWARTotalRed$BWAR+AggDFPBWARTotalRed$PWAR

#bbrefID was used to collect the WAR, so get playerID added to the WAR table then merge it onto the table with players
names(AggDFPBWARTotalRed)[1] <- 'bbrefID'
AggDFPBWARTotalRed$playerID<-PlayersRed$playerID[match(AggDFPBWARTotalRed$bbrefID,PlayersRed$bbrefID)]
MVPQWAR <- MVPQualify
MVPQWAR <- merge(MVPQWAR, AggDFPBWARTotalRed[,c('playerID','yearID','WAR')], by= c('playerID','yearID'), all.x = TRUE)

#do some cleanup of columns in MVPQWAR, rename pitching categories with a "_A" for hits against, runs against, etc.
MVPQWAR$G.y <-NULL
MVPQWAR<-rename(MVPQWAR, G=G.x, R=R.x, H=H.x, HR=HR.x, BB=BB.x, SO=SO.x,IBB=IBB.x,
                HBP=HBP.x,SH=SH.x,SF=SF.x,GIDP=GIDP.x, H_A=H.y,HR_A=HR.y, BB_A=BB.y,
                SO_A=SO.y,IBB_A=IBB.y,R_A=R.y,SH_A=SH.y,SF_A=SF.y,GIDP_A=GIDP.y,HBP_A=HBP.y, X2B="2B", X3B="3B") 
str(MVPQWAR)

#now manually add in the remaining advance stats, BA,OBP, SLG, OPS,WHIP
MVPQWARStats<- MVPQWAR

#BA (Batting Average : Number of Hits/Number of At Bats)
MVPQWARStats$BA <- MVPQWARStats$H / MVPQWARStats$AB

#OBP (On-Base Percentage: Hits + Walks + Hit by Pitch) / (At Bats + Walks + Hit by Pitch + Sacrifice Flies)
MVPQWARStats$OBP <- (MVPQWARStats$H + MVPQWARStats$BB + MVPQWARStats$HBP)/
  (MVPQWARStats$AB + MVPQWARStats$BB + MVPQWARStats$HBP + MVPQWARStats$SF)

#Singles (to use for SLG%)
MVPQWARStats$X1B <- MVPQWARStats$H - MVPQWARStats$X2B - MVPQWARStats$X3B - MVPQWARStats$HR

#SLG (Slugging Percentage: Total Number of Bases / Total Number of At Bats)
MVPQWARStats$SLG <- ((1 * MVPQWARStats$X1B) + (2 * MVPQWARStats$X2B) + (3 * MVPQWARStats$X3B) + (4 * MVPQWARStats$HR) ) / MVPQWARStats$AB

#OPS (On-Base Plus Slugging: OBP + SLG)
MVPQWARStats$OPS <- MVPQWARStats$OBP + MVPQWARStats$SLG

#WHIP (walks plus hits per inning pitched: (Walks + Hits) / Innings Pitched )
MVPQWARStats$WHIP <- (MVPQWARStats$BB_A+MVPQWARStats$H_A)/MVPQWARStats$InnPitch
str(MVPQWARStats)
MVPQWARStats[is.na(MVPQWARStats)]=0

#During one of the aggregation processes, ERA and BAOpp were summed, need to be mean, correcting that
PitchBatERABAO <- merge(BatStatRed, PitchStatsRed, c('playerID','yearID'), all=TRUE)
aggPitchBatERA  <- aggregate(ERA~playerID+yearID, data=PitchBatERABAO, mean)
aggPitchBatBAO  <- aggregate(BAOpp~playerID+yearID, data=PitchBatERABAO, mean)
MVPQWARStatsF <- MVPQWARStats
MVPQWARStatsF <- merge(MVPQWARStatsF[,-33], aggPitchBatERA[,c('playerID','yearID','ERA')], by= c('playerID','yearID'), all.x = TRUE)
MVPQWARStatsF <- merge(MVPQWARStatsF[,-32], aggPitchBatBAO[,c('playerID','yearID','BAOpp')], by= c('playerID','yearID'), all.x = TRUE)

#set new variables that will be used during the remaining data merge processes
MLBMVPTotalPID <- MLBMVPTotalNum#5491
MVPQWARSModel <- MVPQWARStatsF
str(MVPQWARSModel)

###In order to run the MVP analysis, the list of players in the MVP table needed
# to somehow be linked to the overall combined list of pitchers and batters. 
# The MVP table had a player’s first and last name, but no playerID, while the 
# batter/pitcher list had the playerID’s, but did not have the player’s name. 
# The overall Players table had player’s first and last names, as well as the 
# playerIDs, but the full names used did not always match up perfectly with the 
# full name listed in the MVP table. This all meant multiple steps would be 
# needed to obtain all the of the correct playerIDs for the associated names in 
# the MVP table so that the MVP table could be properly merged with the 
# batter/pitcher list.

#Create guestimate playerID variable.
MLBMVPTotalPID <- MLBMVPTotalPID[,-c(1,3:6,24,33:35)]
MLBMVPTotalPID <-rename(MLBMVPTotalPID,H_A=H...28,HR_A=HR...29, BB_A=BB...30, SO_A=SO,yearID=Year)
MLBMVPTotalPID[c('FirstName','LastName')] <- str_split_fixed(MLBMVPTotalPID$Name, ' ', 2)
MLBMVPTotalPID$playerID <- paste0(substr(MLBMVPTotalPID$LastName,1,5),substr(MLBMVPTotalPID$FirstName,1,2),"01")
MLBMVPTotalPID$playerID <- tolower(MLBMVPTotalPID$playerID)
MLBMVPTotalPID$playerID <- sub(" ", "", MLBMVPTotalPID$playerID)
MLBMVPTotalPID$FirstName <- NULL
MLBMVPTotalPID$LastName <- NULL
MLBMVPTotalPID$MVPCan<-"yes"
MLBMVPTotalPID$playerID[4] <-	"jacksjo01"

#remove unneeded variables from pitcher/batter info
MVPQWARSModel <- MVPQWARSModel[,-c(7,8,12,14:19,23,24,26,28,32:41,43:45,49,54)]
names(MVPQWARSModel)[19] <- "IP"

###The below steps were taken to clean up the ~600 players whose playerIDs could 
# not be matched up by guessing the IDs. If there are any issues, I have provided 
# the final outcome files, which can be called to here:
#MLBMVPTotalPID2 <- read_csv(str_glue("{default_directory}/datafiles/MLBMVPTotalPID2.csv"))
#MVPQWARSModel2 <- read_csv(str_glue("{default_directory}/datafiles/MVPQWARSModel2.csv"))

MVPQWARSModel2 <-MVPQWARSModel
MLBMVPTotalPID2 <- MLBMVPTotalPID

RedMVP <- MLBMVPTotalPID2[,c(1,26:28)] #name,year,player, Can
RedTotP <- MVPQWARSModel2[,c(1:2)]#,player, year

#compare which rows in MVP table are not in pitchbat data
DiffInRowsMVP_1 <-anti_join(RedTotP,RedMVP) #
DiffInRowsMVP_2 <-anti_join(RedMVP,RedTotP) #627
DiffInRowsMVP_3 <-semi_join(RedTotP,RedMVP)#
DiffInRowsMVP_4 <-semi_join(RedMVP,RedTotP)# 

#export out the rows that are not matching up, manually update what can be updated
#write.csv(DiffInRowsMVP_2,str_glue("{default_directory}/datafiles/DiffInRowsMVP_2.csv"), row.names = FALSE)
#import manualMVPCheck which was used as part of cleanup, file was imported at top of code
#ManualMVPCheck <- read_csv(str_glue("{default_directory}/datafiles/ManualMVPCheck.csv"))

#copy updates to RedMVP
for (i in 1:nrow(RedMVP)){
  for(h in 1:nrow(ManualMVPCheck)){
    if(RedMVP$Name[i]==ManualMVPCheck$Name[h]){
      if(RedMVP$yearID[i]==ManualMVPCheck$yearID[h]){
        RedMVP$playerID[i]<-ManualMVPCheck$playerID[h] } } }}

#check what is still left
DiffInRowsMVP_1 <-anti_join(RedTotP,RedMVP) #
DiffInRowsMVP_2 <-anti_join(RedMVP,RedTotP) #170
DiffInRowsMVP_3 <-semi_join(RedTotP,RedMVP)#
DiffInRowsMVP_4 <-semi_join(RedMVP,RedTotP)# 

#copy info remaining difference to Test02 for testing
Test02<- DiffInRowsMVP_2

#if any bbrefIDs were used instead of playerIDs, and if so, copy over playerID
for (i in 1:nrow(Players)){
  for (h in 1:nrow(Test02)){
    if (is.na(Players$bbrefID[i])){
      next}
    else{
      if (Players$bbrefID[i]==Test02$playerID[h]){
        Test02$playerID[h]=Players$playerID[i]
      }}}}

#check what is left now
DiffInRowsMVP_5 <-anti_join(RedTotP,Test02)
DiffInRowsMVP_6 <-anti_join(Test02,RedTotP) #148
DiffInRowsMVP_7 <-semi_join(RedTotP,Test02)
DiffInRowsMVP_8 <-semi_join(Test02,RedTotP)

#update test data to RedMVP
for (i in 1:nrow(RedMVP)){
  for(h in 1:nrow(Test02)){
    if(RedMVP$Name[i]==Test02$Name[h]){
      if(RedMVP$yearID[i]==Test02$yearID[h]){
        RedMVP$playerID[i]<-Test02$playerID[h] } } }}

#confirm again whats left
DiffInRowsMVP_1 <-anti_join(RedTotP,RedMVP) 
DiffInRowsMVP_2 <-anti_join(RedMVP,RedTotP) #148
DiffInRowsMVP_3 <-semi_join(RedTotP,RedMVP)
DiffInRowsMVP_4 <-semi_join(RedMVP,RedTotP)

#copy difference to test again
Test02<- DiffInRowsMVP_2

#change remaining playerIDs to "02" in case they were already used
Test02$playerID<-sub("01", "02", DiffInRowsMVP_2$playerID)

#copy test to RedMVP
for (i in 1:nrow(RedMVP)){
  for(h in 1:nrow(Test02)){
    if(RedMVP$Name[i]==Test02$Name[h]){
      if(RedMVP$yearID[i]==Test02$yearID[h]){
        RedMVP$playerID[i]<-Test02$playerID[h] } } }}

#check again
DiffInRowsMVP_1 <-anti_join(RedTotP,RedMVP)
DiffInRowsMVP_2 <-anti_join(RedMVP,RedTotP) #9 left
DiffInRowsMVP_3 <-semi_join(RedTotP,RedMVP)
DiffInRowsMVP_4 <-semi_join(RedMVP,RedTotP)

#diff to test
Test02<- DiffInRowsMVP_2

#manually update remaining
Test02$playerID[4:9] <- 'robinja02' #fix Jackie Robinsons playerID
Test02$playerID[3] <- 'demarfr02' #Fix Frank Demaree's playerID
Test02$playerID[2] <- 'marshmi01' #Fix Mike Marshall's playerID

Test02$playerID[1] <- 'cronijo01' #Fix Mike Marshall's playerID
# Joe Cronin was the very baseline MVPCutoff number used to get rid of the 
# players with too low numbers to be part of the analysis. Somehow between the 
# MVP list and the players training list he was move to the not MVP qualify 
# group, so moving him back.

PitchBatNotMVPQualifyJoeC <- PitchBatNotMVPQualify
PitchBatNotMVPQualifyJoeC<-
  rename(PitchBatNotMVPQualifyJoeC, G=G.x, R=R.x, H=H.x, HR=HR.x, BB=BB.x, 
         SO=SO.x,IBB=IBB.x,HBP=HBP.x,SH=SH.x,SF=SF.x,GIDP=GIDP.x, H_A=H.y,
         HR_A=HR.y, BB_A=BB.y,SO_A=SO.y,IBB_A=IBB.y,R_A=R.y,SH_A=SH.y,SF_A=SF.y,
         GIDP_A=GIDP.y,HBP_A=HBP.y, X2B="2B", X3B="3B")
names(PitchBatNotMVPQualifyJoeC)[45] <-"IP"
JoeCroFix <- PitchBatNotMVPQualifyJoeC[PitchBatNotMVPQualifyJoeC$playerID==
                                         "cronijo01"&PitchBatNotMVPQualifyJoeC$yearID=="1943",]
JoeCroFix[,49:53] <- MLBMVPTotalPID2[624,c(11:14,18)]
JoeCroFix[,54:56] <- MLBMVPTotalPID2[624,c(1,2,28)]
JoeCroFix <- JoeCroFix[names(MVPQWARSModel2)]
MVPQWARSModel2 <- rbind(MVPQWARSModel2, JoeCroFix)

#check again, will still be 1 until original data set is updated showing Joe Cronin copied over
DiffInRowsMVP_5 <-anti_join(RedTotP,Test02)
DiffInRowsMVP_6 <-anti_join(Test02,RedTotP) #1 left
DiffInRowsMVP_7 <-semi_join(RedTotP,Test02)
DiffInRowsMVP_8 <-semi_join(Test02,RedTotP)

#test to RedMVP
for (i in 1:nrow(RedMVP)){
  for(h in 1:nrow(Test02)){
    if(RedMVP$Name[i]==Test02$Name[h]){
      if(RedMVP$yearID[i]==Test02$yearID[h]){
        RedMVP$playerID[i]<-Test02$playerID[h] } } }}

#update the model used for comparing to show Joe Cronin back in the data
RedTotP <- MVPQWARSModel2[,c(1:2)]#,player, year

#check again, should be zero now
DiffInRowsMVP_1 <-anti_join(RedTotP,RedMVP) 
DiffInRowsMVP_2 <-anti_join(RedMVP,RedTotP) #0
DiffInRowsMVP_3 <-semi_join(RedTotP,RedMVP)
DiffInRowsMVP_4 <-semi_join(RedMVP,RedTotP)

#update original MVP data with changes to RedMVP
for (i in 1:nrow(MLBMVPTotalPID2)){
  for(h in 1:nrow(RedMVP)){
    if(MLBMVPTotalPID2$Name[i]==RedMVP$Name[h]){
      if(MLBMVPTotalPID2$yearID[i]==RedMVP$yearID[h]){
        MLBMVPTotalPID2$playerID[i]<-RedMVP$playerID[h]}}} }

#now test final changes
RedMVP <- MLBMVPTotalPID2[,c(1,26:28)] 
RedTotP <- MVPQWARSModel2[,c(1:2)]

DiffInRowsMVP_1 <-anti_join(RedTotP,RedMVP)
DiffInRowsMVP_2 <-anti_join(RedMVP,RedTotP) #0
DiffInRowsMVP_3 <-semi_join(RedTotP,RedMVP)
DiffInRowsMVP_4 <-semi_join(RedMVP,RedTotP)

#export out the files incase of issues replicating clean up
#write.csv(MLBMVPTotalPID2,str_glue("{default_directory}/datafiles/MLBMVPTotalPID2.csv"), row.names = FALSE)
#write.csv(MVPQWARSModel2,str_glue("{default_directory}/datafiles/MVPQWARSModel2.csv"), row.names = FALSE)


################################################################################
####Begin Modelling Process#####################################################
################################################################################
MLBMVPTotalPIDF <- MLBMVPTotalPID2
MVPQWARSModelF <- MVPQWARSModel2

#first create data for modeling using just the 204 MVP award winners
MVPMergeModel <- merge(MVPQWARSModelF, MVPOnly, by=c('playerID',"yearID"),all.x=TRUE)#58309

#clean up the data
PlayerIDCheck<- MVPMergeModel[is.na(MVPMergeModel$awardID),] #58105
MVPMergeModel$awardID[is.na(MVPMergeModel$awardID)] <- "No"
MVPMergeModel$awardID <- as.factor(MVPMergeModel$awardID)
MVPMergeModel[is.na(MVPMergeModel)]<-0

MVPMergeModel$playerID<-NULL
MVPMergeModel$yearID<-NULL

ModelPrepData <- MLBMVPTotalPIDF
ModelPrepData

#now create data for modeling using the 5491 MVP vote getters
MVPMergeModel2 <- merge(MVPQWARSModelF, ModelPrepData[c('playerID',"yearID","MVPCan")], by=c('playerID',"yearID"),all.x=TRUE)

#clean up the data
MVPMergeModel2$MVPCan[is.na(MVPMergeModel2$MVPCan)] <- "No"
MVPMergeModel2$MVPCan <- as.factor(MVPMergeModel2$MVPCan)
MVPMergeModel2[is.na(MVPMergeModel2)]<-0
summary(MVPMergeModel2$MVPCan)
plot(MVPMergeModel2$MVPCan)#5491,52819
text(MVPMergeModel2$MVPCan, cex=0.65, pos=3,col="red")
MVPMergeModel2$playerID<-NULL
MVPMergeModel2$yearID<-NULL

###Start With Tree Models
#First just see what would of been like if only used the two MVPs from everyear (204)
set.seed(89)
TrainTest1 <- sort(sample(nrow(MVPMergeModel), nrow(MVPMergeModel)*.7))
MVPMergeModelTrain <- MVPMergeModel[TrainTest1, ]
MVPMergeModelTest <-MVPMergeModel[-TrainTest1, ]
str(MVPMergeModelTrain)
str(MVPMergeModelTest)
MVPMergeModelTest_L <- MVPMergeModelTest[,25]
MVPMergeModelTest_NL <- MVPMergeModelTest[,-25]

#create the train model
MVPTreeMod1 <- rpart(awardID ~ ., data = MVPMergeModelTrain, method="class")
summary(MVPTreeMod1)

#use the test data
MVPTreePred1 <- predict(MVPTreeMod1, MVPMergeModelTest_NL, type="class")
summary(MVPTreePred1)
str(MVPTreePred1)
FullSampleConf <- confusionMatrix(MVPTreePred1, MVPMergeModelTest_L) #99.61%, but only 4 of 16 MVPs classified correctly
#fancyRpartPlot(MVPTreeMod1) 

#now use the 5491 vote getters
set.seed(35)
TrainTest2 <- sort(sample(nrow(MVPMergeModel2), nrow(MVPMergeModel2)*.7))
MVPMergeModel2Train <- MVPMergeModel2[TrainTest2, ]
MVPMergeModel2Test <-MVPMergeModel2[-TrainTest2, ]
str(MVPMergeModel2Train)
str(MVPMergeModel2Test)
MVPMergeModel2Test_L <- MVPMergeModel2Test[,25]
MVPMergeModel2Test_NL <- MVPMergeModel2Test[,-25]

#create the train model
MVPTreeMod2 <- rpart( MVPCan~., data = MVPMergeModel2Train, method="class")
summary(MVPTreeMod2)

#use the test data
MVPTreePred2 <- predict(MVPTreeMod2, MVPMergeModel2Test_NL, type="class")
summary(MVPTreePred2)
str(MVPTreePred2)
confusionMatrix(MVPTreePred2, MVPMergeModel2Test_L) #93.36
#fancyRpartPlot(MVPTreeMod2)
rpart.plot(MVPTreeMod2, cex =.75, 
           main="DT Model, All Variables Included, All Standard Data, 93.36% Accuracy" )

#try some new parameters
MVPTreeMod3 <- rpart( MVPCan~., data = MVPMergeModel2Train, method="class", minsplit=0, minbucket=0)
summary(MVPTreeMod3)

#use the test data
MVPTreePred3 <- predict(MVPTreeMod3, MVPMergeModel2Test_NL, type="class")
summary(MVPTreePred3)
str(MVPTreePred3)
confusionMatrix(MVPTreePred3, MVPMergeModel2Test_L) #93.36 still
##fancyRpartPlot(MVPTreeMod3)

###Some stats in the list I don't think are very useful for analyzing performance, 
# so removing them: games played, at bats, games started, innings pitch
MVPMergeModel3 <- MVPMergeModel2
MVPMergeModel3 <- MVPMergeModel3[,-c(1,2,11,17)]
MVPMergeModel3

#sample/build model
set.seed(789)
TrainTest4 <- sort(sample(nrow(MVPMergeModel3), nrow(MVPMergeModel3)*.7))
MVPMergeModel3Train <- MVPMergeModel3[TrainTest4, ]
MVPMergeModel3Test <-MVPMergeModel3[-TrainTest4, ]

MVPMergeModel3Test_L <- MVPMergeModel3Test[,21]
MVPMergeModel3Test_NL <- MVPMergeModel3Test[,-21]

#test model
MVPTreeMod4 <- rpart( MVPCan~., data = MVPMergeModel3Train, method="class")
MVPTreePred4 <- predict(MVPTreeMod4, MVPMergeModel3Test_NL, type="class")
confusionMatrix(MVPTreePred4, MVPMergeModel3Test_L) #93.51 slightly up##############Highest#####################
#fancyRpartPlot(MVPTreeMod4)
plot(MVPTreePred4, MVPMergeModel3Test_L)
rpart.plot(MVPTreeMod4, cex = .75)

#going to normalize the data that isn't already a percentage trait(rows 1:13)
MVPMergeModel4 <- MVPMergeModel3
MVPMergeModel4[,1:13] <- MVPMergeModel4[,1:13]/colSums(MVPMergeModel4[,1:13])

#sample/build model
set.seed(123)
TrainTest5 <- sort(sample(nrow(MVPMergeModel4), nrow(MVPMergeModel4)*.7))
MVPMergeModel4Train <- MVPMergeModel4[TrainTest5, ]
MVPMergeModel4Test <-MVPMergeModel4[-TrainTest5, ]

MVPMergeModel4Test_L <- MVPMergeModel4Test[,21]
MVPMergeModel4Test_NL <- MVPMergeModel4Test[,-21]

#test model
MVPTreeMod5 <- rpart( MVPCan~., data = MVPMergeModel4Train, method="class")
MVPTreePred5 <- predict(MVPTreeMod5, MVPMergeModel4Test_NL, type="class")
confusionMatrix(MVPTreePred5, MVPMergeModel4Test_L) #93.01, slightly down
#fancyRpartPlot(MVPTreeMod5)

#altering parameters again based on what has worked in past models
MVPTreeMod6 <- rpart(MVPCan~., data = MVPMergeModel4Train, method="class", 
                     control= rpart.control(minsplit = 1, minbucket = 1,maxdepth = 10, cp = 0))
MVPTreePred6 <- predict(MVPTreeMod6, MVPMergeModel4Test_NL, type="class")
confusionMatrix(MVPTreePred6, MVPMergeModel4Test_L) #92.57, slightly down
#fancyRpartPlot(MVPTreeMod6)

#try raising maxdepth
MVPTreeMod7 <- rpart( MVPCan~., data = MVPMergeModel4Train, 
                      method="class", control= rpart.control(minsplit = 1, minbucket = 1,maxdepth = 30, cp = 0))
MVPTreePred7 <- predict(MVPTreeMod7, MVPMergeModel4Test_NL, type="class")
confusionMatrix(MVPTreePred7, MVPMergeModel4Test_L) #90.57, down more
#fancyRpartPlot(MVPTreeMod7)


#alter model again to only use the stats that are already percentages, 
# leaving Wins, SO, SV in for pitchers since they dont have as many of 
# those stats, as leaving them as not normalized since accuracy went down after changing it
MVPMergeModel5 <- MVPMergeModel3
MVPMergeModel5 <- MVPMergeModel5[,-c(1:6,8,10:12)]

#sample/build model
set.seed(110)
TrainTest6 <- sort(sample(nrow(MVPMergeModel5), nrow(MVPMergeModel5)*.7))
MVPMergeModel5Train <- MVPMergeModel5[TrainTest6, ]
MVPMergeModel5Test <-MVPMergeModel5[-TrainTest6, ]

MVPMergeModel5Test_L <- MVPMergeModel5Test[,11]
MVPMergeModel5Test_NL <- MVPMergeModel5Test[,-11]

#test model
MVPTreeMod6 <- rpart( MVPCan~., data = MVPMergeModel5Train, method="class")
MVPTreePred6 <- predict(MVPTreeMod6, MVPMergeModel5Test_NL, type="class")
confusionMatrix(MVPTreePred6, MVPMergeModel5Test_L) #93.25, slightly up
#fancyRpartPlot(MVPTreeMod6)

#try normalizing everything (besides percent stats) before anything is removed
MVPMergeModel6 <- MVPMergeModel2
MVPMergeModel6[,1:17] <- MVPMergeModel6[,1:17]/colSums(MVPMergeModel6[,1:17])

#sample/build model
set.seed(1)
TrainTest7 <- sort(sample(nrow(MVPMergeModel6), nrow(MVPMergeModel6)*.7))
MVPMergeModel6Train <- MVPMergeModel6[TrainTest7, ]
MVPMergeModel6Test <-MVPMergeModel6[-TrainTest7, ]

MVPMergeModel6Test_L <- MVPMergeModel6Test[,25]
MVPMergeModel6Test_NL <- MVPMergeModel6Test[,-25]

#test model
MVPTreeMod7 <- rpart( MVPCan~., data = MVPMergeModel6Train, method="class")
MVPTreePred7 <- predict(MVPTreeMod7, MVPMergeModel6Test_NL, type="class")
confusionMatrix(MVPTreePred7, MVPMergeModel6Test_L) #93.04, slightly down
#fancyRpartPlot(MVPTreeMod7)
rpart.plot(MVPTreeMod7, cex=.75, main="DT Model, Non-Percent Stats Normalized, 93.04% Accuracy")

#normalize WAR also just to see if that does anthing...
MVPMergeModel7 <- MVPMergeModel6
MVPMergeModel7[,18] <- MVPMergeModel7[,18]/sum(MVPMergeModel7[18])

#sample/build model
set.seed(145)
TrainTest8 <- sort(sample(nrow(MVPMergeModel7), nrow(MVPMergeModel7)*.7))
MVPMergeModel7Train <- MVPMergeModel7[TrainTest8, ]
MVPMergeModel7Test <-MVPMergeModel7[-TrainTest8, ]

MVPMergeModel7Test_L <- MVPMergeModel7Test[,25]
MVPMergeModel7Test_NL <- MVPMergeModel7Test[,-25]

#test model
MVPTreeMod8 <- rpart( MVPCan~., data = MVPMergeModel7Train, method="class")
MVPTreePred8 <- predict(MVPTreeMod8, MVPMergeModel7Test_NL, type="class")
confusionMatrix(MVPTreePred8, MVPMergeModel7Test_L) #93, slightly down
#fancyRpartPlot(MVPTreeMod8)

#normalize the rates that are already percentages just to be sure
MVPMergeModel8 <- MVPMergeModel7
MVPMergeModel8[,19:24] <- MVPMergeModel8[,19:24]/colSums(MVPMergeModel8[,19:24])

#sample/build model
set.seed(678)
TrainTest9 <- sort(sample(nrow(MVPMergeModel8), nrow(MVPMergeModel8)*.7))
MVPMergeModel8Train <- MVPMergeModel8[TrainTest9, ]
MVPMergeModel8Test <-MVPMergeModel8[-TrainTest9, ]

MVPMergeModel8Test_L <- MVPMergeModel8Test[,25]
MVPMergeModel8Test_NL <- MVPMergeModel8Test[,-25]

#test model
MVPTreeMod9 <- rpart( MVPCan~., data = MVPMergeModel8Train, method="class")
MVPTreePred9 <- predict(MVPTreeMod9, MVPMergeModel8Test_NL, type="class")
confusionMatrix(MVPTreePred9, MVPMergeModel8Test_L) #92.9
#fancyRpartPlot(MVPTreeMod9)

#removing everything else besides normalizing WAR and percentage Stats
MVPMergeModel9 <- MVPMergeModel8
MVPMergeModel9 <- MVPMergeModel9[,18:25]

#sample/build model
set.seed(345)
TrainTest10 <- sort(sample(nrow(MVPMergeModel9), nrow(MVPMergeModel9)*.7))
MVPMergeModel9Train <- MVPMergeModel9[TrainTest10, ]
MVPMergeModel9Test <-MVPMergeModel9[-TrainTest10, ]

MVPMergeModel9Test_L <- MVPMergeModel9Test[,8]
MVPMergeModel9Test_NL <- MVPMergeModel9Test[,-8]

#test model
MVPTreeMod10 <- rpart( MVPCan~., data = MVPMergeModel9Train, method="class")
MVPTreePred10 <- predict(MVPTreeMod10, MVPMergeModel9Test_NL, type="class")
confusionMatrix(MVPTreePred10, MVPMergeModel9Test_L) #92.89, slightly down
#fancyRpartPlot(MVPTreeMod10)

#try normalized percentage stats, standard other stats
MVPMergeModel10 <- MVPMergeModel2
MVPMergeModel10[18:24] <- MVPMergeModel10[18:24]/colSums(MVPMergeModel10[18:24])

#sample/build model
set.seed(694)
TrainTest11 <- sort(sample(nrow(MVPMergeModel10), nrow(MVPMergeModel10)*.7))
MVPMergeModel10Train <- MVPMergeModel10[TrainTest11, ]
MVPMergeModel10Test <-MVPMergeModel10[-TrainTest11, ]

MVPMergeModel10Test_L <- MVPMergeModel10Test[,25]
MVPMergeModel10Test_NL <- MVPMergeModel10Test[,-25]

#test model
MVPTreeMod11 <- rpart( MVPCan~., data = MVPMergeModel10Train, method="class")
MVPTreePred11 <- predict(MVPTreeMod11, MVPMergeModel10Test_NL, type="class")
confusionMatrix(MVPTreePred11, MVPMergeModel10Test_L) #92.81 down
#fancyRpartPlot(MVPTreeMod11)

#trying 93.51 model again with higher depth
MVPTreeMod12 <- rpart( MVPCan~., data = MVPMergeModel3Train, method="class", 
                       control= rpart.control(minsplit = 1, minbucket = 1,maxdepth = 30, cp = 0))
MVPTreePred12 <- predict(MVPTreeMod12, MVPMergeModel3Test_NL, type="class")
confusionMatrix(MVPTreePred12, MVPMergeModel3Test_L) #90.86, down
#fancyRpartPlot(MVPTreeMod12)

#higher CP
MVPTreeMod13 <- rpart( MVPCan~., data = MVPMergeModel3Train, method="class", 
                       control= rpart.control(minsplit = 1, minbucket = 1,maxdepth = 1, cp = 1))
MVPTreePred13 <- predict(MVPTreeMod13, MVPMergeModel3Test_NL, type="class")
confusionMatrix(MVPTreePred13, MVPMergeModel3Test_L) #99.1% but predicting all no's
#fancyRpartPlot(MVPTreeMod13)

#low CP, bucket, split
MVPTreeMod14 <- rpart( MVPCan~., data = MVPMergeModel3Train, method="class", 
                       control= rpart.control(minsplit = 0, minbucket = 0,maxdepth = 1, cp = 0))
MVPTreePred14 <- predict(MVPTreeMod14, MVPMergeModel3Test_NL, type="class")
confusionMatrix(MVPTreePred14, MVPMergeModel3Test_L) #91.15
#fancyRpartPlot(MVPTreeMod14)

#raise bucket/split
MVPTreeMod15 <- rpart( MVPCan~., data = MVPMergeModel3Train, method="class", 
                       control= rpart.control(minsplit = 3, minbucket = 3,maxdepth = 1, cp = 0))
MVPTreePred15 <- predict(MVPTreeMod15, MVPMergeModel3Test_NL, type="class")
confusionMatrix(MVPTreePred15, MVPMergeModel3Test_L) #still 91.15
#fancyRpartPlot(MVPTreeMod15)

#sticking with 93.51 model for trees

###Now Naive Bayes Models
#First just the 204 MVP Winners over history of award
MVPNBMod0 <- naive_bayes(awardID ~.,data=MVPMergeModelTrain, laplace = .1)
MVPNBPred0 <- predict(MVPNBMod0,MVPMergeModelTest_NL, type="class")
confusionMatrix(MVPNBPred0,MVPMergeModelTest_L) # all NaN
summary(MVPNBMod1)

#all standard data
MVPNBMod1 <- naive_bayes(MVPCan ~.,data=MVPMergeModel2Train, laplace = .1)
MVPNBPred1 <- predict(MVPNBMod1,MVPMergeModel2Test_NL, type="class")
confusionMatrix(MVPNBPred1,MVPMergeModel2Test_L) # all NaN
summary(MVPNBMod1)

#all standard data, lower laplace
MVPNBMod2 <- naive_bayes(MVPCan ~.,data=MVPMergeModel2Train, laplace = .0001)
MVPNBPred2 <- predict(MVPNBMod2,MVPMergeModel2Test_NL, type="class")
confusionMatrix(MVPNBPred2,MVPMergeModel2Test_L) #All Nan Still
summary(MVPNBMod2)

#try no laplace
MVPNBMod3 <- naive_bayes(MVPCan ~.,data=MVPMergeModel2Train)
MVPNBPred3 <- predict(MVPNBMod3,MVPMergeModel2Test_NL, type="class")
confusionMatrix(MVPNBPred3,MVPMergeModel2Test_L) # All Nan still
summary(MVPNBMod3)

#try scale
MVPNBMod4 <- naive_bayes(MVPCan ~.,data=MVPMergeModel2Train, scale=1)
MVPNBPred4 <- predict(MVPNBMod4,MVPMergeModel2Test_NL, type="class")
confusionMatrix(MVPNBPred4,MVPMergeModel2Test_L) # All Nan still
summary(MVPNBMod4)

#try multinomial
MVPMergeModel2TrainTrainX <- MVPMergeModel2Train[,-25]
MVPMergeModel2TrainTrainY <- MVPMergeModel2Train[,25]

MVPNBMod5 <- multinomial_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY)
MVPNBPred5 <- predict(MVPNBMod5, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred5 , MVPMergeModel2Test_L) # All Nan still :(
plot(MVPNBPred5)

#bernoulli
MVPNBMod6 <- bernoulli_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY)
MVPNBPred6 <- predict(MVPNBMod6, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred6 , MVPMergeModel2Test_L) #52 :(
plot(MVPNBPred6)

#warning spit out recommended laplace
MVPNBMod7 <- bernoulli_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY, laplace = 1)
MVPNBPred7 <- predict(MVPNBMod7, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred7 , MVPMergeModel2Test_L) #0%, NaN produced
plot(MVPNBPred7) 

#lower laplace
MVPNBMod8 <- bernoulli_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY, laplace = .0001)
MVPNBPred8 <- predict(MVPNBMod8, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred8, MVPMergeModel2Test_L) #still 0%, NaN produced, need higher laplace
plot(MVPNBPred8)

#higher laplace
MVPNBMod9 <- bernoulli_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY, laplace = 1.1)
MVPNBPred9 <- predict(MVPNBMod9, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred9, MVPMergeModel2Test_L) #still 0%, NaN produced
plot(MVPNBPred9)

#much higher laplace
MVPNBMod10 <- bernoulli_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY, laplace = 10)
MVPNBPred10 <- predict(MVPNBMod10, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred10, MVPMergeModel2Test_L) #still 0%, NaN produced, next method
plot(MVPNBPred10)

#gaussian
MVPNBMod11 <- gaussian_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY)
MVPNBPred11 <- predict(MVPNBMod11, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred11, MVPMergeModel2Test_L) #still 0%, NaN produced, next method
plot(MVPNBPred11)

#nonparametric
MVPNBMod12 <- nonparametric_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY)
MVPNBPred12 <- predict(MVPNBMod12, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred12, MVPMergeModel2Test_L) #88.61
plot(MVPNBPred12)

#add laplace
MVPNBMod13 <- nonparametric_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY, laplace=.0001)
MVPNBPred13 <- predict(MVPNBMod13, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred13, MVPMergeModel2Test_L) #88.61 still, no affect
plot(MVPNBPred13)

#raise laplace
MVPNBMod14 <- nonparametric_naive_bayes(MVPMergeModel2TrainTrainX,MVPMergeModel2TrainTrainY, laplace=1)
MVPNBPred14 <- predict(MVPNBMod14, as.matrix(MVPMergeModel2Test_NL))
confusionMatrix(MVPNBPred14, MVPMergeModel2Test_L) #88.61 still, no affect
plot(MVPNBPred14)

#try all normalized data
MVPMergeModel8TrainTrainX <- MVPMergeModel8Train[,-25]
MVPMergeModel8TrainTrainY <- MVPMergeModel8Train[,25]

MVPNBMod15 <- nonparametric_naive_bayes(MVPMergeModel8TrainTrainX,MVPMergeModel8TrainTrainY, laplace=1)
MVPNBPred15 <- predict(MVPNBMod15, as.matrix(MVPMergeModel8Test_NL))
confusionMatrix(MVPNBPred15, MVPMergeModel8Test_L) #91.04
plot(MVPNBPred15)

#lower laplace
MVPNBMod16 <- nonparametric_naive_bayes(MVPMergeModel8TrainTrainX,MVPMergeModel8TrainTrainY, laplace=.0001)
MVPNBPred16 <- predict(MVPNBMod16, as.matrix(MVPMergeModel8Test_NL))
confusionMatrix(MVPNBPred16, MVPMergeModel8Test_L) #91.04
plot(MVPNBPred16)

#remove laplace
MVPNBMod17 <- nonparametric_naive_bayes(MVPMergeModel8TrainTrainX,MVPMergeModel8TrainTrainY)
MVPNBPred17 <- predict(MVPNBMod17, as.matrix(MVPMergeModel8Test_NL))
confusionMatrix(MVPNBPred17, MVPMergeModel8Test_L) #91.04
plot(MVPNBPred17)

#some removed, all standard
MVPMergeModel3TrainTrainX <- MVPMergeModel3Train[,-21]
MVPMergeModel3TrainTrainY <- MVPMergeModel3Train[,21]

MVPNBMod18 <- nonparametric_naive_bayes(MVPMergeModel3TrainTrainX,MVPMergeModel3TrainTrainY)
MVPNBPred18 <- predict(MVPNBMod18, as.matrix(MVPMergeModel3Test_NL))
confusionMatrix(MVPNBPred18, MVPMergeModel3Test_L) #87.85
plot(MVPNBPred18, main="NB Model, Some Variables Removed, All Standard Data, 87.5% Accuracy")

#some still removed, normalize non standard 
MVPMergeModel4TrainTrainX <- MVPMergeModel4Train[,-21]
MVPMergeModel4TrainTrainY <- MVPMergeModel4Train[,21]

MVPNBMod19 <- nonparametric_naive_bayes(MVPMergeModel4TrainTrainX,MVPMergeModel4TrainTrainY)
MVPNBPred19 <- predict(MVPNBMod19, as.matrix(MVPMergeModel4Test_NL))
NBMatrix <- confusionMatrix(MVPNBPred19, MVPMergeModel4Test_L) #92.79 #############################
plot(MVPNBPred19)
probplot(MVPNBPred19)
print(MVPNBPred19)
plot(MVPNBMod19)
plot.function(MVPNBPred19)
boxplot(MVPNBMod19)
plot(MVPNBMod19[["data"]][["x"]])
hist(MVPNBMod19[["data"]][["x"]])
boxplot(MVPNBMod19[["data"]][["x"]])
plot(MVPNBMod19[["dens"]][["WAR"]][["No"]][["x"]],MVPNBMod19[["dens"]][["WAR"]][["No"]][["y"]])
hist(MVPNBMod19[["dens"]][["WAR"]][["No"]][["x"]],MVPNBMod19[["dens"]][["WAR"]][["No"]][["y"]])

#leave percent stats, standard W,SO,SV
MVPMergeModel5TrainTrainX <- MVPMergeModel5Train[,-11]
MVPMergeModel5TrainTrainY <- MVPMergeModel5Train[,11]

MVPNBMod20 <- nonparametric_naive_bayes(MVPMergeModel5TrainTrainX,MVPMergeModel5TrainTrainY)
MVPNBPred20 <- predict(MVPNBMod20, as.matrix(MVPMergeModel5Test_NL))
confusionMatrix(MVPNBPred20, MVPMergeModel5Test_L) #90.32########
plot(MVPNBPred20)

#everything but percent stats normalized
MVPMergeModel6TrainTrainX <- MVPMergeModel6Train[,-25]
MVPMergeModel6TrainTrainY <- MVPMergeModel6Train[,25]

MVPNBMod21 <- nonparametric_naive_bayes(MVPMergeModel6TrainTrainX,MVPMergeModel6TrainTrainY)
MVPNBPred21 <- predict(MVPNBMod21, as.matrix(MVPMergeModel6Test_NL))
confusionMatrix(MVPNBPred21, MVPMergeModel6Test_L) #92.04########
plot(MVPNBPred21)

#normalize war in addition
MVPMergeModel7TrainTrainX <- MVPMergeModel7Train[,-25]
MVPMergeModel7TrainTrainY <- MVPMergeModel7Train[,25]

MVPNBMod22 <- nonparametric_naive_bayes(MVPMergeModel7TrainTrainX,MVPMergeModel7TrainTrainY)
MVPNBPred22 <- predict(MVPNBMod22, as.matrix(MVPMergeModel7Test_NL))
confusionMatrix(MVPNBPred22, MVPMergeModel7Test_L) #91.85########
plot(MVPNBPred22)

#normalized percentage rates
MVPMergeModel9TrainTrainX <- MVPMergeModel9Train[,-8]
MVPMergeModel9TrainTrainY <- MVPMergeModel9Train[,8]

MVPNBMod23 <- nonparametric_naive_bayes(MVPMergeModel9TrainTrainX,MVPMergeModel9TrainTrainY)
MVPNBPred23 <- predict(MVPNBMod23, as.matrix(MVPMergeModel9Test_NL))
confusionMatrix(MVPNBPred23, MVPMergeModel9Test_L) #91.6########
plot(MVPNBPred23, main="NB Model, Normalized Percentage and WAR Rates, 91.6% Accuracy")

#normal percnt, standard everything else
MVPMergeModel10TrainTrainX <- MVPMergeModel10Train[,-25]
MVPMergeModel10TrainTrainY <- MVPMergeModel10Train[,25]

MVPNBMod24<- nonparametric_naive_bayes(MVPMergeModel10TrainTrainX,MVPMergeModel10TrainTrainY)
MVPNBPred24 <- predict(MVPNBMod24, as.matrix(MVPMergeModel10Test_NL))
confusionMatrix(MVPNBPred24, MVPMergeModel10Test_L) #89.41########
plot(MVPNBPred24)

#sticking with 92.79 model

###Now Try SVMs
#svm, linear kernal, cost 1, scale false
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel2Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel2Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel2Test_L)
#Fails with: Error in predict.svm(MVPSVMMod1, MVPMergeModel2Test_NL) : 
#NA/NaN/Inf in foreign function call (arg 23)

#try changing type
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel2Train, kernel="linear", cost = 1, scale = FALSE, type="nu-class")
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel2Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel2Test_L)
#Error in svm.default(x, y, scale = scale, ..., na.action = na.action) : 
#specified nu is infeasible!

#change kernel
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel2Train, kernel="polynomial", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel2Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel2Test_L)
#error in predict again

#radial
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel2Train, kernel="radial", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel2Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel2Test_L)
#error in predict again

#sigmoid
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel2Train, kernel="sigmoid", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel2Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel2Test_L)
#error in predict again

#try with some variable removed
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel3Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel3Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel3Test_L)
#error in predict again

#some still removed, normalize non standard
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel4Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel4Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel4Test_L)
#error in predict again

#leave percent stats, standard W,SO,SV
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel5Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel5Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel5Test_L)
#error in predict again

#everything but percent stats normalized
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel6Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel6Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel6Test_L)
#error in predict again

#normalize war in addition,scale of 1
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel7Train, kernel="linear", cost = 1, scale = 1)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel7Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel7Test_L)
#error in predict again

#normalized percentage rates
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel8Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel8Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel8Test_L)
#error in predict again

#normalized percentage rates
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel9Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel9Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel9Test_L)
#error in predict again

#normal percnt, standard everything else
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel10Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel10Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel10Test_L)
#error in predict again

#normal percnt, standard everything else
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel10Train, kernel="linear", cost = 100, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel10Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel10Test_L)
#error in predict again

#normal percnt, standard everything else
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel10Train, kernel="polynomial", cost = 100, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel10Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel10Test_L)
#error in predict again

#normal percnt, standard everything else
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel10Train, kernel="radial", cost = 100, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel10Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel10Test_L)
#error in predict again

#normal percnt, standard everything else
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel10Train, kernel="sigmoid", cost = 100, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel10Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel10Test_L)
#error in predict again

#try adding 1 to everything in all normal data set?
MVPMergeModel2P1 <- MVPMergeModel2
MVPMergeModel2P1[,1:24] <- MVPMergeModel2[,1:24]+1

set.seed(35)
SVMTest1 <- sort(sample(nrow(MVPMergeModel2P1), nrow(MVPMergeModel2P1)*.7))
MVPMergeModel2P1Train <- MVPMergeModel2P1[SVMTest1, ]
MVPMergeModel2P1Test <-MVPMergeModel2P1[-SVMTest1, ]
str(MVPMergeModel2P1Train)
str(MVPMergeModel2P1Test)
MVPMergeModel2P1Test_L <- MVPMergeModel2P1Test[,25]
MVPMergeModel2P1Test_NL <- MVPMergeModel2P1Test[,-25]

#normal perecnt, standard everything else
MVPSVMMod1 <- svm(MVPCan ~., data=MVPMergeModel2P1Train, kernel="linear", cost = 1, scale = FALSE)
MVPSVMPred1 <- predict(MVPSVMMod1, MVPMergeModel2P1Test_NL)
confusionMatrix(MVPSVMPred1 , MVPMergeModel2P1Test_L)
#error in predict again

################################################################################
###Now use created models for trying to predict 2022 MVPs#######################
################################################################################

###First download/extract required 2022 data. In case there are issues downloading the data, 
# it has also been provided and imported at the beginning of the document, the files
# are DF2022_1, DF2022_2, DF2022_3, and DF2022_4.

url <- ('https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2Fmajors%2F2022-value-batting.shtml&div=div_players_value_batting')
urldata <- GET(url)
ValBatting2022 <- readHTMLTable(rawToChar(urldata$content),stringsAsFactors = FALSE)

write.csv(ValBatting2022[["players_value_batting"]],str_glue("{default_directory}/datafiles/ValBatting2022.csv"), row.names = FALSE)

url <- ('https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2Fmajors%2F2022-value-pitching.shtml&div=div_players_value_pitching')
urldata <- GET(url)
ValPitching2022 <- readHTMLTable(rawToChar(urldata$content),stringsAsFactors = FALSE)

write.csv(ValPitching2022[["players_value_pitching"]],str_glue("{default_directory}/datafiles/ValPitching2022.csv"), row.names = FALSE)

url <- ('https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2Fmajors%2F2022-standard-batting.shtml&div=div_players_standard_batting')
urldata <- GET(url)
StandBat2022 <- readHTMLTable(rawToChar(urldata$content),stringsAsFactors = FALSE)

write.csv(StandBat2022[["players_standard_batting"]],str_glue("{default_directory}/datafiles/StandBat2022.csv"), row.names = FALSE)

url <- ('https://widgets.sports-reference.com/wg.fcgi?css=1&site=br&url=%2Fleagues%2Fmajors%2F2022-standard-pitching.shtml&div=div_players_standard_pitching')
urldata <- GET(url)
StandPitch2022 <- readHTMLTable(rawToChar(urldata$content),stringsAsFactors = FALSE)

write.csv(StandPitch2022[["players_standard_pitching"]],str_glue("{default_directory}/datafiles/StandPitch2022.csv"), row.names = FALSE)

#extract the needed stats from the downloaded data
DF2022_1=data.frame(StandBat2022[["players_standard_batting"]][["Name"]],
                    StandBat2022[["players_standard_batting"]][["Age"]],
                    StandBat2022[["players_standard_batting"]][["Tm"]], 
                    StandBat2022[["players_standard_batting"]][["G"]], 
                    StandBat2022[["players_standard_batting"]][["AB"]], 
                    StandBat2022[["players_standard_batting"]][["R"]], 
                    StandBat2022[["players_standard_batting"]][["H"]], 
                    StandBat2022[["players_standard_batting"]][["HR"]], 
                    StandBat2022[["players_standard_batting"]][["RBI"]], 
                    StandBat2022[["players_standard_batting"]][["SB"]], 
                    StandBat2022[["players_standard_batting"]][["BB"]], 
                    StandBat2022[["players_standard_batting"]][["BA"]], 
                    StandBat2022[["players_standard_batting"]][["OBP"]],
                    StandBat2022[["players_standard_batting"]][["SLG"]], 
                    StandBat2022[["players_standard_batting"]][["OPS"]])

DF2022_2=data.frame(ValBatting2022[["players_value_batting"]][["Name"]], 
                    ValBatting2022[["players_value_batting"]][["Age"]], 
                    ValBatting2022[["players_value_batting"]][["Tm"]], 
                    ValBatting2022[["players_value_batting"]][["WAR"]])  

DF2022_3=data.frame(StandPitch2022[["players_standard_pitching"]][["Name"]],
                    StandPitch2022[["players_standard_pitching"]][["Age"]],
                    StandPitch2022[["players_standard_pitching"]][["Tm"]],
                    StandPitch2022[["players_standard_pitching"]][["W"]],
                    StandPitch2022[["players_standard_pitching"]][["L"]],
                    StandPitch2022[["players_standard_pitching"]][["ERA"]], 
                    StandPitch2022[["players_standard_pitching"]][["WHIP"]],
                    StandPitch2022[["players_standard_pitching"]][["GS"]],
                    StandPitch2022[["players_standard_pitching"]][["SV"]],
                    StandPitch2022[["players_standard_pitching"]][["IP"]], 
                    StandPitch2022[["players_standard_pitching"]][["H"]], 
                    StandPitch2022[["players_standard_pitching"]][["HR"]], 
                    StandPitch2022[["players_standard_pitching"]][["BB"]], 
                    StandPitch2022[["players_standard_pitching"]][["SO"]])

DF2022_4=data.frame(ValPitching2022[["players_value_pitching"]][["Name"]],
                    ValPitching2022[["players_value_pitching"]][["Age"]],
                    ValPitching2022[["players_value_pitching"]][["Tm"]],
                    ValPitching2022[["players_value_pitching"]][["WAR"]])

###Now clean up the data
#clean up column names
colnames(DF2022_1) <- c("Name","Age","Tm","G","AB" ,'R' ,'H' ,'HR' ,'RBI',
                        'SB' ,'BB', 'BA', 'OBP', 'SLG', 'OPS')
colnames(DF2022_2) <- c("Name","Age","Tm", "B_WAR")
colnames(DF2022_3) <- c("Name","Age","Tm",'W',' L' ,'ERA', 'WHIP' ,'GS' ,'SV',
                        'IP',' H_A', 'HR_A', 'BB_A', 'SO_A')
colnames(DF2022_4) <- c("Name","Age","Tm", "P_WAR")

#preserve work done already
DF2022STB <- DF2022_1
DF2022VB <- DF2022_2
DF2022STP <- DF2022_3
DF2022VP <- DF2022_4

#remove not needed rows
DF2022STB<- DF2022STB[!DF2022STB$Tm=="TOT",]
DF2022STB<- DF2022STB[!DF2022STB$Name=="Name",]   
DF2022STP<- DF2022STP[!DF2022STP$Tm=="TOT",]
DF2022STP<- DF2022STP[!DF2022STP$Name=="Name",]
DF2022VB<- DF2022VB[!DF2022VB$Tm=="TOT",]
DF2022VB<- DF2022VB[!DF2022VB$Name=="Name",]
DF2022VP<- DF2022VP[!DF2022VP$Tm=="TOT",]
DF2022VP<- DF2022VP[!DF2022VP$Name=="Name",]

#get rid of special characters in players names
DF2022STB$Name <- (str_replace_all(DF2022STB$Name, "[^[:alnum:]]", "")) 
DF2022STP$Name <- (str_replace_all(DF2022STP$Name, "[^[:alnum:]]", ""))
DF2022VB$Name <- (str_replace_all(DF2022VB$Name, "[^[:alnum:]]", ""))
DF2022VP$Name <- (str_replace_all(DF2022VP$Name, "[^[:alnum:]]", ""))

#data conversion and cleanup
DF2022STB[,4:15] <- lapply(DF2022STB[,4:15], as.numeric)
DF2022STP[,4:14] <- lapply(DF2022STP[,4:14], as.numeric)
DF2022VB[,4] <- as.numeric(DF2022VB[,4])
DF2022VP[,4] <- as.numeric(DF2022VP[,4])

DF2022STB[is.na(DF2022STB)]=0
DF2022STP[is.na(DF2022STP)]=0
DF2022VP[is.na(DF2022VP)]=0
DF2022VB[is.na(DF2022VB)]=0

#seperate tallied data and percentage data so their rows can be aggregated seperately
aggBatNorm = DF2022STB[,-(2:11)]
aggBatNorm = aggregate(.~Name, data=aggBatNorm, mean)
aggBatStand = DF2022STB[,-c(2:3, 12:15)]
aggBatStand = aggregate(.~Name, data=aggBatStand, sum)
aggPitchNorm = DF2022STP[, -c(2:5,8:14)]
aggPitchNorm = aggregate(.~Name, data=aggPitchNorm, mean)
aggPitchStand = DF2022STP[,-c(2:3,6:7)]
aggPitchStand = aggregate(.~Name, data=aggPitchStand, sum)

#bring it all back together
aggFullBat <- merge(aggBatStand, aggBatNorm, 'Name', all.x = TRUE, all.y = TRUE)
aggFullPitch <- merge(aggPitchStand, aggPitchNorm, 'Name', all.x = TRUE, all.y = TRUE)

aggFullBW <- merge(aggFullBat, DF2022VB[,c('Name','B_WAR')], 'Name', all.x = TRUE, all.y = TRUE)
aggFullPW <- merge(aggFullPitch, DF2022VP[,c('Name','P_WAR')], 'Name', all.x = TRUE, all.y = TRUE)

#combine P and B WAR
aggFull2022 <- merge(aggFullBW, aggFullPW, 'Name', all.x = TRUE, all.y = TRUE)
aggFull2022[is.na(aggFull2022)]=0
aggFull2022$WAR <- aggFull2022$B_WAR + aggFull2022$P_WAR
aggFull2022$B_WAR <- NULL
aggFull2022$P_WAR <- NULL
aggFull2022

#calc the MVPCutOff for everyone
aggFull2022$IPperc <- 100 * (aggFull2022$IP/ sum(aggFull2022$IP))
aggFull2022$ABperc <- 100 * (aggFull2022$AB/ sum(aggFull2022$AB))
aggFull2022$MVPCutoff <-  (aggFull2022$IPperc + aggFull2022$ABperc)/2
count(aggFull2022[aggFull2022$MVPCutoff>=0.04545401,]) #807 out od 1472
NotMVPQualify2022 <- aggFull2022[aggFull2022$MVPCutoff<0.04545401,]
MVPQualify2022 <- aggFull2022[aggFull2022$MVPCutoff>=0.04545401,]
MVPQualify2022 <- MVPQualify2022[,-c(1,26:28)]

#Start modeling
#use model built with only the 204 MVP award winners
MVPQualify2022_204<- rename(MVPQualify2022, 'L'=' L', 'H_A' =  ' H_A') #column names had spaces in them
Tree204Pred <- predict(MVPTreeMod1,MVPQualify2022_204, type="class")
summary(Tree204Pred)
plot(Tree204Pred, main="2022 Data Used In Model Built Only With 204 Awarded MVPs")

#best DT model
DTMVPQualify2022 <- MVPQualify2022[,-c(1,2,15,17)]
DTMVPQualify2022 <- rename(MVPQualify2022, 'L'=' L', 'H_A' =  ' H_A') #column names had spaces in them
TreePred2022 <- predict(MVPTreeMod4, DTMVPQualify2022, type="class")
TreePred2022
summary(TreePred2022)

#Best NB model
NBMVPQualify2022 <- rename(MVPQualify2022, 'L'=' L', 'H_A' =  ' H_A')
NBMVPQualify2022 <- NBMVPQualify2022[names(MVPMergeModel4Test_NL)]
NBMVPQualify2022NS <- NBMVPQualify2022
NBMVPQualify2022[,1:13] <- NBMVPQualify2022[,1:13]/colSums(NBMVPQualify2022[,1:13])
NBPred2022<- predict(MVPNBMod19, as.matrix(NBMVPQualify2022))

#try with non standard data just to see
NBPred2022Non_Stand <- predict(MVPNBMod19, as.matrix(NBMVPQualify2022NS))
summary(NBPred2022)

#best NB model was not good, trying with second best NB model
NBMVPQualify2022_2 <- MVPQualify2022
NBMVPQualify2022_2[,1:17] <- NBMVPQualify2022_2[,1:17]/colSums(NBMVPQualify2022_2[,1:17])
NBPred2022_2 <- predict(MVPNBMod21, as.matrix(NBMVPQualify2022_2))

#put all of the outputs together
MVPCompList <- aggFull2022[aggFull2022$MVPCutoff>=0.04545401,]
MVPCompDF <- data.frame(MVPCompList$Name, TreePred2022, NBPred2022, NBPred2022_2)
summary(NBPred2022_2)
write.csv(MVPCompDF,"C:/<directory path here>/IST 707/group project/2022ModelPredictions.csv", row.names = FALSE)

#recode the DF to make easy GGplot
Prediction= c("204 MVP Pred", "204 MVP Pred", "Tree Pred", "Tree Pred", 
              "NB Pred", "NB Pred", "NB Pred 2", "NB Pred 2")
MVP = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")
PredCount = c(0, 804 ,23, 781, 177, 627, 60, 744)
ggMVPcompDF = data.frame(Prediction, "MVP"= as.factor(MVP), PredCount)

ggplot(ggMVPcompDF, aes(Prediction, PredCount, fill=MVP)) + 
  geom_bar(stat = "identity", position='dodge') + 
  geom_text(aes(label=PredCount), position=position_dodge(width=0.9), vjust=-0.25) +ggtitle("2022 DT and NB Model Predictions") 
+ theme(plot.title=element_text(hjust=0.5))

#code sources:
#https://www.projectpro.io/data-science-in-r-programming-tutorial/r-tutorial-importing-data-from-web
#https://www.datacamp.com/tutorial/r-data-import-tutorial#read-csv,-txt,-html,-and-other-common-files-into-r
#https://www.geeksforgeeks.org/string-concatenation-in-r-programming/
#https://stackoverflow.com/questions/27662162/error-in-my-code-target-of-assignment-expands-to-non-language-object
#https://www.geeksforgeeks.org/concatenation-of-elements-without-separator-in-r-programming-paste0-function/
#https://www.statmethods.net/management/merging.html#:~:text=To%20join%20two%20data%20frames,be%20in%20the%20same%20order.&text=If%20data%20frameA%20has%20variables,variables%20in%20data%20frameA%20or
#https://www.geeksforgeeks.org/check-if-an-object-of-the-specified-name-is-defined-or-not-in-r-programming-exists-function/#:~:text=rm()%20Function-,Check%20if%20an%20Object%20of%20the%20Specified%20Name%20is%20Defined,R%20Programming%20%E2%80%93%20exists()%20Function&text=exists()%20function%20in%20R,if%20the%20object%20is%20found.
#https://stackoverflow.com/questions/16566799/change-variable-name-in-for-loop-using-r
#https://stackoverflow.com/questions/24191497/left-join-only-selected-columns-in-r-with-the-merge-function
#https://www.geeksforgeeks.org/remove-all-special-characters-from-string-in-r/