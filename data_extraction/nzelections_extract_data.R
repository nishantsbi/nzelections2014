setwd("C:/Users/earth/Desktop")
list.files()
##install.packages("XML")
library(XML)


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

electURL <- "http://www.electionresults.govt.nz/electorateindex.html"
electDOC <- htmlTreeParse(electURL, useInternalNodes = TRUE)
rawLinks <- xpathSApply(electDOC, '//a/@href')

electLinks <- rawLinks[grep("electorate-", rawLinks)]
electNames <- xpathSApply(electDOC, "//a[contains(@href,'electorate-')]", xmlValue)

# clean up
rm(electURL)
rm(electDoc)
rm(rawLinks)

## ------------------------------------------------------------------------
##
## Read in the data from each electorate, and extract the data of interest