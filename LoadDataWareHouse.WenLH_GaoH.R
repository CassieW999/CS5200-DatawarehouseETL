
# 1. Library
library(RSQLite)
dbfile = "db.db"
dbcon <- dbConnect(RSQLite::SQLite(), "db.db")

# 2. 
library(sqldf)
df.authorship <- dbGetQuery(dbcon, "select a.*, (b.cnt - 1) as numofcoauthors from ArticleAuthor a
                                    left join (select PMID, count(*) as cnt from ArticleAuthor group by PMID) b
                                    on a.PMID = b.PMID")
df.authorinfo <- dbGetQuery(dbcon, "select a.AuthorID, (a.ForeName || a.LastName) as authorname, b.cnt as numofarticles from Authors a
                                    left join (select AuthorID, count(*) as cnt from ArticleAuthor group by AuthorID) b
                                    on a.AuthorID = b.AuthorID")

# 1. Library
library(RMySQL)

# 2. Settings
db_user <- 'admin'
db_password <- 'password'
db_name <- 'ArticleDB'
db_host <- 'cs5200.cligdkonlz0p.us-east-1.rds.amazonaws.com' # AWS Host
db_port <- 3306

# 3. Read data from db
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)


# 4. Create and populate a star schema for author facts
# Each row in this fact table will represent one author fact. 
# It must include the authors id, author name, number of articles by that author, total number of co-authors across all articles.
dbSendQuery(mydb, "CREATE SCHEMA IF NOT EXISTS authorfact;")
dbSendQuery(mydb, "USE authorfact;")
dbSendQuery(mydb, "DROP TABLE IF EXISTS authorship;")
dbSendQuery(mydb, "DROP TABLE IF EXISTS authorfact;")
dbSendQuery(mydb, "CREATE TABLE authorfact.authorship (
                    PMID INT,
                    AuthorID INT,
                    numofcoauthors INT
                   );")

dbWriteTable(mydb, "authorship", df.authorship, overwrite = T)

dbSendQuery(mydb, "CREATE TABLE authorfact.authorfact (
                    authorid INT,
                    authorname TEXT,
                    numofarticles INT,
                    totalcoauthors INT
                   );")

for (r in 1:nrow(df.authorinfo)) {
  authorid <- df.authorinfo$AuthorID[r]
  sqlcmd <- paste0('select SUM(numofcoauthors) as SumOfAuthors from authorfact.authorship where AuthorID = "', authorid, '";')
  totalcoauthors <- dbGetQuery(mydb, sqlcmd)
  df.authorinfo$totalcoauthors[r] <- totalcoauthors$SumOfAuthors[1]
}
dbWriteTable(mydb, "authorfact", df.authorinfo, overwrite = T)

table1 <- dbGetQuery(mydb, "select * from authorfact;")

# 5. Create and populate a star schema for journal facts. 
# Each row in this fact table will represent one journal fact. 
# It must include the journal name, number of articles per year, per quarter, and per month.
df.journal <- dbGetQuery(dbcon, "select a.JournalID, b.ISSN, b.Title, b.Year, b.Month from Articles a left join Journals b on a.JournalID == b.JournalID")

dbSendQuery(mydb, "CREATE SCHEMA IF NOT EXISTS journalschema;")
dbSendQuery(mydb, "USE journalschema;")
dbSendQuery(mydb, "DROP TABLE IF EXISTS journalDim;")
dbSendQuery(mydb, "DROP TABLE IF EXISTS journalfact;")

dbSendQuery(mydb, "CREATE TABLE journalDim (
                    JournalID TEXT,
                    ISSN TEXT,
                    Title TEXT,
                    Year TEXT,
                    Month TEXT,
                    Quater TEXT
                   );")

 dbSendQuery(mydb, "CREATE TABLE journalfact (
                    ISSN TEXT,
                    Title TEXT,
                    month TEXT,
                   articlepermonth INT,
                   quater TEXT,
                   articleperquarter INT,
                   year TEXT,
                   articleperyear INT
                  );")

# df.journal
for (r in 1:nrow(df.journal)) {
  year <- df.journal$Year[r]
  month <- df.journal$Month[r]
  if (is.null(month) | is.na(month)) {
    quater <- ""
    next
  }
  print(month)
  quater <- ceiling(as.numeric(month) / 3)
  month <- paste0(year, df.journal$Month[r]) 
  quaternew <- paste0(year, quater)
  df.journal$Month[r] <- month
  df.journal$Quater[r] <- quaternew
}
dbWriteTable(mydb, "journalDim", df.journal, overwrite = T)

table2 <- dbGetQuery(mydb, "select c.ISSN, Title, month, articlepermonth,quater, articleperquarter, year,  articleperyear from 
                  (select JournalID, ISSN, Title, month, count(*) as articlepermonth from journalDim group by ISSN, month) c 
                  left join (select JournalID, ISSN, quater, count(*) as articleperquarter from journalDim group by ISSN, quater) b on c.JournalID = b.JournalID
                  left join (select JournalID, ISSN, Year, count(*) as articleperyear from journalDim group by ISSN, Year) a on a.JournalID = c.JournalID;")

dbWriteTable(mydb, "journalfact", table2, overwrite = T)

dbGetQuery(mydb, "select * from journalfact")

dbDisconnect(mydb)

# journalname, year, articleperyear, quater,     articleperquarter, month,      articlepermonth
# aaa           2018  3             2018Q1          2                 2018Q103       1
# aaa           2018  3             2018Q2          1                 2018Q103       1