library(XML)
library(RSQLite)
library(DBI)
library(knitr)
library(stringr)

dbfile = "db.db"
mydb <- dbConnect(RSQLite::SQLite(), "db.db")

# Create tables

dbSendQuery(mydb, "drop table if exists Journals")
dbSendQuery(mydb, "drop table if exists Articles")
dbSendQuery(mydb, "drop table if exists Authors")
dbSendQuery(mydb, "drop table if exists ArticleAuthor")

# CitedMedium TEXT,
# Volume int,
# Issue int,
# Create Journals table
dbSendQuery(mydb, "CREATE TABLE Journals (
            JournalID TEXT NOT NULL PRIMARY KEY,
            ISSN TEXT,
            
            
            Year int,
            Month int,
            Title TEXT,
            ISOAbbreviation TEXT
)")

# Create Articles table
dbSendQuery(mydb, "CREATE TABLE Articles (
            PMID int NOT NULL PRIMARY KEY,
            JournalID TEXT,
            Language TEXT,
            ArticleTitle TEXT
)")

# ValidYN TEXT
# Create Authors table
dbSendQuery(mydb, "CREATE TABLE Authors (
            AuthorID TEXT NOT NULL PRIMARY KEY,
            LastName TEXT,
            ForeName TEXT,
            Initials TEXT
            
)")

# Create Article-Author table
dbSendQuery(mydb, "CREATE TABLE ArticleAuthor (
            PMID int,
            AuthorID TEXT
)")


xmlDOM <- xmlParse("pubmed22n0001-tf.xml")

# get the root node of the DOM tree
r <- xmlRoot(xmlDOM)

numPA <- xmlSize(r)

# CitedMedium = character(),
# Volume = integer(),
# Issue = integer(),

df.journals <- data.frame (JournalID = character(),
                           ISSN = character(),
                           
                           
                           Year = integer(),
                           Month = integer(),
                           Title = character(),
                           ISOAbbreviation = character(),
                           stringsAsFactors = F)


df.articles <- data.frame (PMID = integer(),
                           JournalID = character(),
                           Language = character(),
                           ArticleTitle = integer(),
                           stringsAsFactors = F)

# ValidYN = character(),
df.authors <- data.frame (AuthorID = character(),
                          LastName = character(),
                          ForeName = character(),
                          Initials = character(),
                          
                          stringsAsFactors = F)

df.articleauthor <- data.frame (PMID = character(),
                                AuthorID = character(),
                                stringsAsFactors = F)

# check if that element is already in the data frame
keyExists <- function (aKey, aDF)
{
  n <- nrow(aDF)
  if (n == 0)
  {
    # data frame is empty, so can't exist
    return(0)
  }
  
  for (a in 1:n)
  {
    # check if all columns match for a row; ignore the aID column
    if (aDF[a,1] == aKey)
    {
      # found a match; return it's ID
      return(a)
    }
  }
  
  # none matched
  return(0)
}

rowExists <- function (aRow, aDF)
{
  n <- nrow(aDF)
  if (n == 0)
  {
    # data frame is empty, so can't exist
    return(0)
  }
  
  for (a in 1:n)
  {
    # check if all columns match for a row; ignore the aID column
    if (all(aDF[a,] == aRow[1,]))
    {
      # found a match; return it's ID
      return(a)
    }
  }
  
  # none matched
  return(0)
}

p <- function(..., sep='-') {
  paste(..., sep=sep, collapse=sep)
}

# iterate over the first-level child elements off the root:
# the <PubmedArticle> elements

LanguageGroup <- xpathSApply(xmlDOM, "//Language",xmlValue)
ArticleTitleGroup <- xpathSApply(xmlDOM, "//ArticleTitle",xmlValue)
ISSNGroup <- xpathSApply(xmlDOM,"//ISSN",xmlValue)
VolumeGroup <- xpathSApply(xmlDOM, "//Volume",xmlValue)
IssueGroup <- xpathSApply(xmlDOM, "//Issue",xmlValue)
PubDateGroup <- xpathSApply(xmlDOM, "//PubDate",xmlValue)
ISOAbbreviationGroup <- xpathSApply(xmlDOM, "//ISOAbbreviation",xmlValue)
LastNameGroup <- xpathSApply(xmlDOM, "//LastName",xmlValue)
ForeNameGroup <- xpathSApply(xmlDOM, "//ForeName",xmlValue)
InitialsGroup <- xpathSApply(xmlDOM, "//Initials",xmlValue)
TitleGroup <- xpathSApply(xmlDOM, "//Title",xmlValue)
JournalGroup <- xpathSApply(xmlDOM, "//Journal",xmlValue)


print(length(LanguageGroup))
print(length(JournalGroup))
print(length(ArticleTitleGroup))
print(length(ISSNGroup))
print(length(VolumeGroup))
print(length(IssueGroup))
print(length(PubDateGroup))
print(length(ISOAbbreviationGroup))
print(length(LastNameGroup))
print(length(ForeNameGroup))
print(length(InitialsGroup))
print(length(TitleGroup))
print(length(numPA))


AuthorCount = 1
AuthorListCount = 1
JournalCount = 1


for (i in 1:numPA)
{
  # get next PubmedArticle node
  aPA <- r[[i]]
  PMID <- xmlAttrs(aPA)
  if (length(PMID) == 0) {
    PMID <- 0
  }
  print(aPA)

  # get next Article node
  aAT <- aPA[[1]]

  Language <- xpathSApply(aAT, "./Language",xmlValue)
  if (length(Language) != 1) {
    Language <- "Language"
  }

  ArticleTitle <- xpathSApply(aAT, "./ArticleTitle",xmlValue)
  if (length(ArticleTitle) == 0) {
    ArticleTitle <- "ArticleTitle"
  }
  
  # get next Journal node
  aJN <- xpathSApply(aAT, "./Journal")
  
  ISSN <- xpathSApply(aPA, "./Article/Journal/ISSN",xmlValue)
  if (length(ISSN) == 0) {
    ISSN <- "ISSN"
  }
  
  # aJournalIssue <- aJN[[2]]
  # CitedMedium <- xmlAttrs(aJournalIssue)
  #Volume <- VolumeGroup[[i]]
  #Issue <- IssueGroup[[i]]
  PubDate <- xpathSApply(aPA, "./Article/Journal/JournalIssue/PubDate",xmlValue)
  if (length(PubDate) == 0) {
    PubDate <- "1900JAN"
  }

  Title <- xpathSApply(aPA, "./Article/Journal/Title",xmlValue)
  if (length(Title) == 0) {
    Title <- "Title"
  }

  ISOAbbreviation <- xpathSApply(aPA, "./Article/Journal/ISOAbbreviation",xmlValue)
  if (length(ISOAbbreviation) == 0) {
    ISOAbbreviation <- "ISOAbbreviation"
  }

  # Split PubDate to Year and Month
  spaceCount <- str_count(PubDate, ' ')
  PubDate2 <- PubDate
  
  if (spaceCount > 0) {
    Year <- substring(PubDate,1,4)
    MonthStr <- substring(PubDate2,6,8)
    Month <- match(MonthStr,month.abb)
  } else {
    Year <- substring(PubDate,1,4)
    MonthStr <- substring(PubDate2,5,7)
    Month <- match(MonthStr,month.abb)
  }
  
  
  
  JournalID <- p(ISSN, PubDate)
  
  journalResult <- keyExists(JournalID, df.journals)
  if (journalResult == 0) {
    df.journals[JournalCount,'JournalID'] <- as.character(JournalID)
    df.journals[JournalCount,'ISSN'] <- as.character(ISSN)
    #df.journals[JournalCount,'CitedMedium'] <- as.character(CitedMedium)
    #df.journals[JournalCount,'Volume'] <- as.integer(Volume)
    #df.journals[JournalCount,'Issue'] <- as.integer(Issue)
    df.journals[JournalCount,'Year'] <- as.integer(Year)
    df.journals[JournalCount,'Month'] <- as.integer(Month)
    df.journals[JournalCount,'Title'] <- as.character(Title)
    df.journals[JournalCount,'ISOAbbreviation'] <- as.character(ISOAbbreviation)
    JournalCount = JournalCount + 1
  }
  
  
  df.articles[i,'PMID'] <- as.integer(PMID)
  df.articles[i,'JournalID'] <- as.character(JournalID)
  df.articles[i,'Language'] <- as.character(Language)
  df.articles[i,'ArticleTitle'] <- as.character(ArticleTitle)
  
  # get next Author node
  aAuthorList <- aAT[[4]]

  numAuthor <- xmlSize(aAuthorList)
  if (numAuthor == 0) {
    next
  }
  for (j in 1:numAuthor)
  {
    aAuthor <- aAuthorList[[j]]
    LastName <- xpathSApply(aAuthor, "./LastName",xmlValue)
    if (length(LastName) == 0) {
      LastName <- "LastName"
    }
    ForeName <- xpathSApply(aAuthor, "./ForeName",xmlValue)
    if (length(ForeName) == 0) {
      ForeName <- "ForeName"
    }
    # ValidYN <- xmlAttrs(aAuthor)
    AuthorID <- p(ForeName, LastName)
    
    Initials <- xpathSApply(aAuthor, "./Initials",xmlValue)
    if (length(Initials) == 0) {
      Initials <- "Initials"
    }
    
    df.articleauthor[AuthorCount,'PMID'] <- as.character(PMID)
    df.articleauthor[AuthorCount,'AuthorID'] <- as.character(AuthorID)
    
    AuthorCount = AuthorCount + 1
    
    result <- keyExists(AuthorID, df.authors)
    if (result == 0) {
      df.authors[AuthorListCount,'AuthorID'] <- as.character(AuthorID)
      df.authors[AuthorListCount,'LastName'] <- as.character(LastName)
      df.authors[AuthorListCount,'ForeName'] <- as.character(ForeName)
      df.authors[AuthorListCount,'Initials'] <- as.character(Initials)
      # df.authors[AuthorListCount,'ValidYN'] <- as.character(ValidYN)
      AuthorListCount = AuthorListCount + 1
    }
    
  }
}


dbWriteTable(mydb, "Articles", df.articles, overwrite = T)
dbWriteTable(mydb, "Journals", df.journals, overwrite = T)
dbWriteTable(mydb, "Authors", df.authors, overwrite = T)
dbWriteTable(mydb, "ArticleAuthor", df.articleauthor, overwrite = T)
