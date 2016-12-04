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

siteurl <- "http://www.utahstateaggies.com/sports/m-footbl/stats/2016-2017/usu01.html"
tables <- readHTMLTable(siteurl)

#####Pull the tables for each quarter
first_quarter_table <- tables[[43]]
second_quarter_table <- tables[[44]]
third_quarter_table <- tables[[45]]
fourth_quarter_table <- tables[[46]]

###First Quarter Info
first_quarter_name <- t(as.data.frame(names(first_quarter_table)))
colnames(first_quarter_table) <- c("Possession","Down.Distance","Yard.Line","Play")
colnames(first_quarter_name) <- c("Possession","Down.Distance","Yard.Line","Play")
first_quarter <- rbind(first_quarter_name,first_quarter_table, make.row.names =TRUE)
first_quarter$Quarter <- 1

###Second Quarter
second_quarter_name <- t(as.data.frame(names(second_quarter_table)))
colnames(second_quarter_table) <- c("Possession","Down.Distance","Yard.Line","Play")
colnames(second_quarter_name) <- c("Possession","Down.Distance","Yard.Line","Play")
second_quarter <- rbind(second_quarter_name,second_quarter_table, make.row.names =TRUE)
second_quarter$Quarter <- 2

###Third Quarter
third_quarter_name <- t(as.data.frame(names(third_quarter_table)))
colnames(third_quarter_table) <- c("Possession","Down.Distance","Yard.Line","Play")
colnames(third_quarter_name) <- c("Possession","Down.Distance","Yard.Line","Play")
third_quarter <- rbind(third_quarter_name,third_quarter_table, make.row.names =TRUE)
third_quarter$Quarter <- 3

###Fourth Quarter
fourth_quarter_name <- t(as.data.frame(names(fourth_quarter_table)))
colnames(fourth_quarter_table) <- c("Possession","Down.Distance","Yard.Line","Play")
colnames(fourth_quarter_name) <- c("Possession","Down.Distance","Yard.Line","Play")
fourth_quarter <- rbind(fourth_quarter_name,fourth_quarter_table, make.row.names =TRUE)
fourth_quarter$Quarter <- 4

###Combine Data Sets
Game <- rbind(first_quarter,second_quarter,third_quarter,fourth_quarter, make.row.names=FALSE)

#Drop unnecessary rows
Game <- Game[-grep("won the toss", Game$Play),]
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

UtahStateDownsDataSuccess <- count(UtahState, vars= c("Down","Distance","Success"))
names(UtahStateDownsDataSuccess) <- c("Down","Distance","Success","Count")


FirstDown <- ggplot(UtahStateDownsDataSuccess[UtahStateDownsDataSuccess$Down==1,],aes(x=Distance,y=Count,fill=factor(Success))) + geom_bar(stat="identity", width = .9)+theme_bw()+ggtitle("First Down") + xlim(0,15) + theme(legend.position = "none") + scale_fill_manual(values=c("red","blue","green","yellow"))
SecondDown <- ggplot(UtahStateDownsDataSuccess[UtahStateDownsDataSuccess$Down==2,],aes(x=Distance,y=Count,fill=factor(Success))) + geom_bar(stat="identity", width = .9)+theme_bw()+ggtitle("Second Down") + xlim(0,15) + theme(legend.position = "none") + scale_fill_manual(values=c("red","green","blue","yellow"))
ThirdDown <- ggplot(UtahStateDownsDataSuccess[UtahStateDownsDataSuccess$Down==3,],aes(x=Distance,y=Count,fill=factor(Success))) + geom_bar(stat="identity", width = .9)+theme_bw()+ggtitle("Third Down") + xlim(0,15) + theme(legend.position = "none") + scale_fill_manual(values=c("red","green","blue","yellow"))
FourthDown <- ggplot(UtahStateDownsDataSuccess[UtahStateDownsDataSuccess$Down==4,],aes(x=Distance,y=Count,fill=factor(Success))) + geom_bar(stat="identity", width = .9)+theme_bw()+ggtitle("Fourth Down") + xlim(0,15) + theme(legend.position = "none") + scale_fill_manual(values=c("red","green","blue","yellow"))
grid.arrange(FirstDown,SecondDown,ThirdDown,FourthDown, ncol=2, top = "Downs by Frequency and Success")




  








