## Data Analysis and Statistical Inference
#
# Course Project, Sept 2014
# Duke University via Coursera
# Nick Burns
##

##install.packages("XML")
##install.packages("RCurl")
library(XML)
library(plyr)
setwd("C:\\DataSciToolkit\\myGitHub\\nzelections2014\\data_extraction")

## ------------------------------------------------------------------------
##
# extract
# Is a function which takes a dataframe (ID, url)
# and returns a dataframe of the election data for the electorate
# This function will be passed to ddply later on

extract <- function(x) {
  # 1. read in the HTML and extract the table data (td elements)
  # 2. coerce the data into a matrix and transpose such that for each row we have 6 variables
  #    those variables are:
  #       {"ID", PartyName", "PartyVotes", "Blank", "CandidateName", "CandidateParty", "CandidateVotes"}
  
  doc <- htmlTreeParse(x$url, useInternalNodes=T)  
  
  rawData <- c(   xpathSApply(doc, "//table/tr[contains(@class, 'hhevy')]/td", xmlValue)
                  , xpathSApply(doc, "//table/tr[contains(@class, 'hlite')]/td", xmlValue))  
  
  lclData <- as.data.frame(t(matrix(rawData, nrow=6, ncol=length(rawData)/6)))
  
  rm(doc)
  rm(rawData)
  
  return (lclData)
}

## ------------------------------------------------------------------------
##
## Extract Electorate Names, Numbers and linked pages
#  This section prepares the various elements we will require

# 1. Extract full HTML document

# 2. Extract the hyperlinks themselves, but only those for the elcetorate data
#    The electorate data links all have a pattern like: 'electorate-XX.html'
#    We will filter based on a pattern like: 'electorate-' using grep
#
# NOTE: grep creates a ordinal index where TRUE. So we are filtering all links
#       based on their position in the vector which coincide with the 'electorate-' pattern
#
# 3. Create the full url address, and associate this with an electoral index
#
# 4. Use XQuery to extract the Electorate names from the hyperlinks, creating a dataframe of {ID, Electorate}

electURL <- "http://www.electionresults.govt.nz/electionresults_2014/electorateindex.html"
electDOC <- htmlTreeParse(electURL, useInternalNodes = TRUE)

rawLinks <- xpathSApply(electDOC, '//a/@href')
electLinks <- rawLinks[grep("electorate-", rawLinks)]

electURLs <- paste("http://www.electionresults.govt.nz/electionresults_2014/", electLinks, sep='')
electURLs <- data.frame(ID=c(1:length(electURLs)), url=electURLs)

electNames <- xpathSApply(electDOC, "//a[contains(@href,'electorate-')]", xmlValue)
electNames <- data.frame(ID = c(1:length(electNames)), Electorate=electNames)

# clean up
rm(electURL)
rm(electDOC)
rm(electLinks)
rm(rawLinks)

## ------------------------------------------------------------------------
##
## Gather the data for each electorate
#
#  Using the URLs gathered above, we pass these to extract()
#  Extract() reads in the data of interest, coercing it into a data frame of variables
#  ddply allows us to repeat this for all electorates (psuedo-setbased)


votingData <- ddply(electURLs, "ID", extract)

## ------------------------------------------------------------------------
##
## Clean the Data
#
#  1. Rename the votingData, first removing the 4 column which is blank
#  2. Merge the Electoral Names data with the voting data
#  3. Write out the cleansed data

votingData <- votingData[c(-4)]

names(votingData) <- c("ID", "Party", "PartyVotes", "MPName", "MPParty", "MPVotes")

cleanData <- merge(electNames, votingData)

# Finally, I need to tweak the vote counts to remove the commas and ensure they can
# be read in as numeric data
cleanData$PartyVotes <- gsub(",", "", cleanData$PartyVotes)
cleanData$MPVotes <- gsub(",", "", cleanData$MPVotes)

write.csv(cleanData, file="clean_electorate_data.csv")

rm(votingData)
rm(cleanData)
rm(electNames)
rm(electURLs)