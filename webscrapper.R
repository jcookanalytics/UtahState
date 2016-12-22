#URL that needs to be set
#example is left in to show what it would do without this being in the function
#pull the HTML tables
library(XML)
library(stringr)
library(reshape2)
library(plyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)

########## Grab the data from site and combine into a single data frame #########
source("analysis/data/scraphtml.R")


#add the score to the game table
#create score table
Score <- Game[grep("Utah State", Game$Play),]
Score <- Score[Score$Possession=="",]
Score$row <- row.names(Score)

#break off into a seperate vectors and then into a table
numbers <- as.character(unlist(str_extract_all(Score$Play, "[0-9]+")))
teams <- as.character(unlist(str_extract_all(Score$Play, "[A-z]+ [A-z]+")))
numbers <- data.frame(matrix(numbers, ncol = 2, byrow = TRUE))
teams <- teams[1:2]
teams[teams!="Utah State"] <- "Opponent"
teams[teams=="Utah State"] <- "UtahState"
names(numbers) <- teams

#combine back into the main score table
Score$UtahState <- numbers$UtahState 
Score$Opponent <- numbers$Opponent
rm(numbers)

#fill the score in for everyplay





#Drop unnecessary rows
Game <- Game[-grep("won the toss|Coin Toss", Game$Play),]
Game <- Game[-grep("Drive", Game$Play),]
Game <- Game[-grep("ball on", Game$Play),]
Game <- Game[-grep("at QB", Game$Play),]
Game <- Game[-grep("Utah State", Game$Play),]
Game <- Game[-grep("drive start", Game$Play),]
Game <- Game[-grep("End of", Game$Play),]
Game <- Game[-grep("FINAL SCORE", Game$Play),]

#Make possession capitalized
Game$Possession <- toupper(Game$Possession)

#Downs and distances
Game$Down <- str_extract(Game$Down.Distance,"([1-4])")
Game$Down <- as.numeric(Game$Down)
Game$Distance <- str_extract(Game$Down.Distance,"(?<=-)[0-9]+")
Game$Distance <- as.numeric(Game$Distance)




#Pull apart the Yard line
Game$Side <- str_extract(Game$Yard.Line,"[A-Z][a-z]+")
Game$Side <- toupper(Game$Side)
Game$Position <- str_extract(Game$Yard.Line,"[0-9]+")
Game$Position <- as.numeric(Game$Position)
gamerows <- nrow(Game)
field <- numeric(gamerows)

for (i in 1:gamerows) {
  tryCatch({
    if (Game$Possession[i]==Game$Side[i]){
      field[i] <- (50-Game$Position[i])+50}
    else{field[i] <- Game$Position[i]}
  }, error=function(e){})
}

Game$Position <- field
Game$Position[Game$Position==0] <- NA

for (i in grep("G",Game$Down.Distance)){
  Game$Distance[i] <- Game$Position[i]
}

#Type of plays
type <- paste("(?<![A-Za-z0-9])[0-9]+","no gain","kickoff","rush","pass","punt","lost","sack","PENALTY","field goal","return","drop","loss","incomplete","TOUCHDOWN","DOWN","touchback","intercepted","fumble","NO PLAY","GOOD","MISSED",sep="|")
Game$Type <- str_extract_all(Game$Play,type)
Game$Type <- as.character(Game$Type)
Game$Type[Game$Type=="character(0)"] <- ""
Game$Type <- gsub(x= Game$Type, pattern = "^c","")
Game$Type <- gsub(x= Game$Type, pattern = "[[:punct:]]","")

#Penalties
Game$Penalty <- str_extract(Game$Type,"(?i)(?<=PENALTY\\D)\\d+")
Game$Penalty <- as.numeric(Game$Penalty)
Game$PenaltyOn <- str_extract(Game$Play,"(?<=PENALTY )[A-Z]+")
Game$PenaltyOn[is.na(Game$PenaltyOn)] <- ""

#First Downs (note that in college a touchdown that is not goal to go is considered a first down)
Game$FirstDown <- 0
Game$FirstDown[grep("1 DOWN", Game$Type)] <- 1
Game$FirstDown <- as.numeric(Game$FirstDown)

#Touchdowns
Game$Touchdown <- 0
Game$Touchdown[grep("TOUCHDOWN", Game$Type)] <- 1

#Field goals
Game$FieldGoal <- str_extract(Game$Type,"(?i)(?<=field goal\\D)\\d+")
Game$FieldGoal <- as.numeric(Game$FieldGoal)
Game$FieldGoalStatus <- 0
Game$FieldGoalStatus[grep("GOOD",Game$Type)] <- 1

#Success
Success <- numeric(gamerows)
for (i in 1:gamerows){
  if (Game$FirstDown[i]==1){
    Success[i] <- 1}
}
for (i in 1:gamerows){
  if (Game$Touchdown[i]==1){
    Success[i] <- 2}
}  
for (i in 1:gamerows){
  if (Game$FieldGoalStatus[i]==1){
    Success[i] <- 3}
}
Game$Success <- Success
Game$Success <- factor(Game$Success)
levels(Game$Success) <- c("0","1","2","3")

#Kickoff
Game$Kickoff <- str_extract(Game$Type,"(?i)(?<=kickoff\\D)\\d+")
Game$Kickoff <- as.numeric(Game$Kick)
#Touchbacks
Game$Touchback <- NA
Game$Touchback[grep("touchback",Game$Type)] <- 1

#Punts
Game$Punt <- str_extract(Game$Type,"(?i)(?<=punt\\D)\\d+")
Game$Punt <- as.numeric(Game$Punt)

#Returns includes all returns
Game$Return <- str_extract(Game$Type,"(?i)(?<=return\\D)\\d+")
Game$Return <- as.numeric(Game$Return)

#Rushes and sacks
Game$Rush <- str_extract(Game$Type,"(?i)(?<=rush\\D)\\d+")
Game$Rush <- as.numeric(Game$Rush)
Game$RushLoss <- str_extract(Game$Type,"(?i)(?<=rush loss\\D)\\d+")
Game$RushLoss <- as.numeric(Game$RushLoss)
Game$RushLoss <- Game$RushLoss*-1
Game$Sack <- str_extract(Game$Type,"(?i)(?<=sack loss\\D)\\d+")
Game$Sack <- as.numeric(Game$Sack)
Game$Sack <- Game$Sack*-1

#RushNet
RushNet <- numeric(gamerows)
for (i in 1:gamerows) {
  if (Game$Possession[i]==Game$PenaltyOn[i]) {
    RushNet[i] <- 0 }
  else if (!is.na(Game$Rush[i])) {
    RushNet[i] <- Game$Rush[i] }
  else if (!is.na(Game$Sack[i])) {
    RushNet[i] <- Game$Sack[i] }
  else if  (!is.na(Game$RushLoss[i])) {
    RushNet[i] <- Game$RushLoss[i] }
}
Game$RushNet <- RushNet
Game$RushNet[setdiff(1:gamerows,grep("rush|sack",Game$Type))] <- NA

#Calculate passing for each play
Game$Pass <- str_extract(Game$Type,"(?i)(?<=pass\\D)\\d+")
Game$Pass <- as.numeric(Game$Pass)
Game$PassLoss <- str_extract(Game$Type,"(?i)(?<=pass loss\\D)\\d+")
Game$PassLoss <- as.numeric(Game$PassLoss)
Game$PassLoss <- Game$PassLoss*-1

PassNet <- numeric(gamerows)
for (i in 1:gamerows) {
  if (Game$Possession[i]==Game$PenaltyOn[i]) {
    PassNet[i] <- 0 }
  else if (!is.na(Game$Pass[i])) {
    PassNet[i] <- Game$Pass[i] }
  else if  (!is.na(Game$PassLoss[i])) {
    PassNet[i] <- Game$PassLoss[i] }
}

Game$PassNet <- PassNet
Game$PassNet[setdiff(1:gamerows,grep("pass",Game$Type))] <- NA

#Utah State Analysis
UtahState <- Game[Game$Possession=="USU",]

UtahStateDownsDataSuccess <- data.frame(table(UtahState$Success,UtahState$Distance,UtahState$Down))

names(UtahStateDownsDataSuccess) <- c("Success","Distance","Down","Count")


myColors <- brewer.pal(4,"Set1")
colScale <- scale_colour_manual(name = "Success",values = myColors,breaks=c(0,1,2,3),labels=c("Failed","FieldGoal","First Down","Touchdown"))



play <- character(nrow(UtahStateDownsDataSuccess))
for (i in 1:nrow(UtahStateDownsDataSuccess)){
  if (UtahStateDownsDataSuccess$Success[i]==0){
    play[i] <- "Failed"
  }else if (UtahStateDownsDataSuccess$Success[i]==1){
    play[i] <- "First Down"
  }else if (UtahStateDownsDataSuccess$Success[i]==2){
    play[i] <- "Touchdown"
  }else if (UtahStateDownsDataSuccess$Success[i]==3){
    play[i] <- "Field Goal"
  }
}
UtahStateDownsDataSuccess$Play <- play

FirstDown <- ggplot(data=UtahStateDownsDataSuccess[UtahStateDownsDataSuccess$Down==1,],aes(x=Distance, y=Count ,fill=Play)) + geom_bar(stat="identity") + theme(legend.position = "none")
SecondDown <- ggplot(data=UtahStateDownsDataSuccess[UtahStateDownsDataSuccess$Down==2,],aes(x=Distance, y=Count ,fill=Play)) + geom_bar(stat="identity") + theme(legend.position = "none")
ThirdDown <- ggplot(data=UtahStateDownsDataSuccess[UtahStateDownsDataSuccess$Down==3,],aes(x=Distance, y=Count ,fill=Play)) + geom_bar(stat="identity") + theme(legend.position = "none")
FourthDown <- ggplot(data=UtahStateDownsDataSuccess[UtahStateDownsDataSuccess$Down==4,],aes(x=Distance, y=Count ,fill=Play)) + geom_bar(stat="identity") + theme(legend.position = "none")
grid.arrange(FirstDown,SecondDown,ThirdDown,FourthDown, ncol=2, top = "Downs by Frequency and Success")


