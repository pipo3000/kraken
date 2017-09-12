library("RSQLite")

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="kraken.sqlite")

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

trades_ETHEUR <- lDataFrames[[which(tables=="ETHEUR_Trades")]]
trades_XBTEUR <- lDataFrames[[which(tables=="XBTEUR_Trades")]]
write.csv(trades_ETHEUR, "trades_ETHEUR.csv", fileEncoding = "UTF-8")
write.csv(trades_XBTEUR, "trades_XBTEUR.csv", fileEncoding = "UTF-8")
