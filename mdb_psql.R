
# in 2010 RODBC didnt work iwth mdbtools
# https://stat.ethz.ch/pipermail/r-help/2010-April/236983.html
# install unixodbc{,-dev} odbc-mdbtools
# edit /etc/odbc.ini to include
# check with odbcinst -j ; isql -v LunaJetDB
# library(RODBC)
# con <- odbcConnect("LunaJetDB")
# sqlTables(con, tableType = "TABLE")$TABLE_NAME

library(Hmisc)
library(dplyr)
library(lubridate)
library(tidyr)
library(LNCDR) # for col_ungroup
# install mdbtools, use Hmisc::mdb.get 
db <- "/Volumes/L/bea_res/Database/LunaDB.mdb"
dbtbl <-function( tble)  mdb.get(db, tble)
tables <- mdb.get(db, tables=TRUE)

# parse dates, correct dates that are too far in the future (esp early dobs)
dbdate <- function(x, nyears=2) {
  x<- mdy_hms(as.character(x))
  badidx <- x>now() + dyears(nyears)
  x[badidx] <- x[badidx] - dyears(100)
  return(x)
}

# df with cols: Conact1Email Contact2Email Contact1Address Contact2Address
# to df like: ContactNum, Email, Address

## Person
# all people into one table
subj <- dbtbl("tSubjectInfo")  %>% mutate(fromtbl="subj",    ID=LunaID)
hotl <- dbtbl("tHotline")      %>% mutate(fromtbl="hotline", ID=HotlineID)
allp <- rbind(subj %>% select(-LunaID),
              hotl %>% select(-HotlineID) ) %>%
        mutate(pid=1:n())

p <- allp %>%
     select(pid, ID, fromtbl,
            fname=SubjectFirstName,
            lname=SubjectLastName,
            sex=SexID,
            hand=HandID,
            dob=DateOfBirth) %>%
     mutate(dob=dbdate(dob),
            sex=cut(sex, breaks=c(-1, 0,   1,   2),
                         labels=c(   "U", "M", "F") ),
            hand=factor(as.character(hand),
                        labels=list("-1"="U", "1"="R", "2"="L", "3"="A")))

contacts <- allp %>%
   select(pid, matches("Contact.*")) %>%
   col_ungroup("^Contact[0-9]", "relation") %>%
   unite("who", FirstName, LastName, sep=" ") %>%
   unite("Address", Address1, Address2, City, State, ZipCode, sep=" ") %>%
   gather("ctype", "cvalue", -pid, -who, -relation) %>%
   filter(cvalue!="", !grepl("^[-() ,PA]*$", cvalue))
##


vlog <- dbtbl("tVisitlog")
