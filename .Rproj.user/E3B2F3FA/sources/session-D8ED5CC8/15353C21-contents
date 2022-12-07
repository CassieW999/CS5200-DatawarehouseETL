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

AuthorCount = 1
AuthorListCount = 1
JournalCount = 1

print(length(ISSNGroup))
print(length(TitleGroup))

for (i in 1:numPA)
{
  # get next PubmedArticle node
  aPA <- r[[i]]
  PMID <- xmlAttrs(aPA)
  # Extract PMID
  # PMID <- as.numeric(a[1])
  
  # get next Article node
  aAT <- aPA[[1]]
  Language <- LanguageGroup[[i]]
  ArticleTitle <- ArticleTitleGroup[[i]]
  
  # get next Journal node
  aJN <- aAT[[1]]
  ISSN <- ISSNGroup[[i]]
  
  aJournalIssue <- aJN[[2]]
  # CitedMedium <- xmlAttrs(aJournalIssue)
  #Volume <- VolumeGroup[[i]]
  #Issue <- IssueGroup[[i]]
  PubDate <- PubDateGroup[[i]]
  Title <- TitleGroup[[i]]
  ISOAbbreviation <- ISOAbbreviationGroup[[i]]
  
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
  
  # get next Author node
  aAuthorList <- aAT[[4]]
  
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
  
  
  
  numAuthor <- xmlSize(aAuthorList)
  for (j in 1:numAuthor)
  {
    aAuthor <- aAuthorList[[j]]
    LastName <- LastNameGroup[[AuthorCount]]
    ForeName <- ForeNameGroup[[AuthorCount]]
    # ValidYN <- xmlAttrs(aAuthor)
    AuthorID <- p(ForeName, LastName)
    Initials <- InitialsGroup[[AuthorCount]]
    
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
