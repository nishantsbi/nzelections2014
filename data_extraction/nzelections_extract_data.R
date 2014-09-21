## Data Analysis and Statistical Inference
#
# Course Project, Sept 2014
# Duke University via Coursera
# Nick Brns
##

##install.packages("XML")
##install.packages("RCurl")
library(XML)
library(RCurl)
setwd("C:\\DataSciToolkit\\myGitHub\\nzelections2014\\data_extraction")


## ------------------------------------------------------------------------
##
## Extract Electorate Names, Numbers and linked pages

# 1. Extract full HTML document

# 2. Extract the hyperlinks themselves, but only those for the elcetorate data
#    The electorate data links all have a pattern like: 'electorate-XX.html'
#    We will filter based on a pattern like: 'electorate-' using grep
#
# NOTE: grep creates a ordinal index where TRUE. So we are filtering all links
#       based on their position in the vector which coincide with the 'electorate-' pattern

# 3. Use XQuery to extract the Electorate names from the hyperlinks

electURL <- "http://www.electionresults.govt.nz/electionresults_2014/electorateindex.html"
electDOC <- htmlTreeParse(electURL, useInternalNodes = TRUE)
rawLinks <- xpathSApply(electDOC, '//a/@href')

electLinks <- rawLinks[grep("electorate-", rawLinks)]
electNames <- xpathSApply(electDOC, "//a[contains(@href,'electorate-')]", xmlValue)

# clean up
rm(electURL)
rm(electDOC)
rm(rawLinks)

## ------------------------------------------------------------------------
##
## Read in the data from each electorate, and extract the data of interest

# 1. Extract full HTML document for each electorate

# 2. Extract:
#       - the table of data
#       - the rows of class "hhevy" or "hlite"
#       - the columns of data
#
#    The columns correspond to: {PartyName, PartyVotes, CandidateName, CandidateParty, CandidateVotes}

electURLs <- paste("http://www.electionresults.govt.nz/electionresults_2014/", electLinks, sep='')


testURL <- "http://www.electionresults.govt.nz/electionresults_2014/electorate-1.html"
testDOC <- htmlTreeParse(testURL, useInternalNodes=T)

xpathSApply(testDOC, "//table/tr[contains(@class, 'hhevy')]/td")
xpathSApply(testDOC, "//table/tr[contains(@class, 'hhevy')]/td", xmlValue) ## This is perfect, it just gives me the data
                                                                           ## each row contains 6 variables, some of which may be empty
                                                                           ## (Party, votes, NULL, Candidate, Party, Votes)
t <- c(xpathSApply(testDOC, "//table/tr[contains(@class, 'hhevy')]/td", xmlValue)
       , xpathSApply(testDOC, "//table/tr[contains(@class, 'hlite')]/td", xmlValue))
# this gives me a vector of length 108, where there are 18 observations with 6 cols each

# either of these are equivalent, the second reduces memory cost
s <- matrix(t, nrow=6, ncol=18)

# create a data frame
data <- as.data.frame(t(s))
names(data) <- c("PartyName", "PartyVotes", "Blank", "CandidateName", "CandidateParty", "CandidateVotes")

mergeData <- function(url) {
  doc <- htmlTreeParse(url, useInternalNodes=T)
  t <- c(xpathSApply(doc, "//table/tr[contains(@class, 'hhevy')]/td", xmlValue)
         , xpathSApply(doc, "//table/tr[contains(@class, 'hlite')]/td", xmlValue))
  lclData <- as.data.frame(t(matrix(t, nrow=6, ncol=length(t)/6)))
  #lclData <- cbind(x[1], lclData)
  lclData
}

u <- c("http://www.electionresults.govt.nz/electionresults_2014/electorate-1.html"
       , "http://www.electionresults.govt.nz/electionresults_2014/electorate-2.html"
       , "http://www.electionresults.govt.nz/electionresults_2014/electorate-3.html")
ldply(u, mergeData)
