

library(XML)


#training set http://www.utahstateaggies.com/sports/m-footbl/stats/2016-2017/usu01.html
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