
# in 2010 RODBC didnt work iwth mdbtools
# https://stat.ethz.ch/pipermail/r-help/2010-April/236983.html
# install unixodbc{,-dev} odbc-mdbtools
# edit /etc/odbc.ini to include
# check with odbcinst -j ; isql -v LunaJetDB
# library(RODBC)
# con <- odbcConnect("LunaJetDB")
# sqlTables(con, tableType = "TABLE")$TABLE_NAME

library(Hmisc) # mdb.get
library(dplyr)
library(lubridate)
library(tidyr)
library(LNCDR) # for col_ungroup
library(jsonlite)
library(reshape2)
library(stringr)

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

## contacts

contacts <- allp %>%
   select(pid, matches("Contact.*")) %>%
   col_ungroup("^Contact[0-9]", "relation") %>%
   unite("who", FirstName, LastName, sep=" ") %>%
   unite("Address", Address1, Address2, City, State, ZipCode, sep=" ") %>%
   gather("ctype", "cvalue", -pid, -who, -relation) %>%
   filter(cvalue!="", !grepl("^[-() ,PA]*$", cvalue))

## Visits

vlog <- dbtbl("tVisitlog") %>%
   mutate_at(vars(VisitTime, VisitDate),
             function(x) x %>% as.character %>% mdy_hms)

# pid and age
visit_pid <- merge(vlog, p, by.x="LunaID", by.y="ID")  %>%
   mutate(age=as.numeric(VisitDate - dob)/(365.25)) %>%
   select(pid, vid=VisitID, age)
# time and date stored separately, sort of -- combine
visit_time <-
   vlog %>%
   mutate(tdiff = ifelse(is.na(VisitTime),
                         0,
                         VisitTime - floor_date(VisitTime, "day")),
          vtimestamp=floor_date(VisitDate, "day") + tdiff) %>%
   select(vid=VisitID, VisitDate, VisitTime, tdiff, vtimestamp)

# visit type: from wide to long (then collapse by character)
vtpl <- vlog %>%
   select(vid=VisitID, "Behavioral", "MEG", "Scan") %>%
   melt(id.var="vid") %>%
   filter(value>0) %>%
   group_by(vid) %>%
   summarise(vtype = paste(collapse=",", variable))

# 12 with 2
#  ...melt... %>% group_by(vid) %>% summarise(s=sum(value)) %>% filter(s!=1) 

conf_tp <- vlog %>%
   mutate(Notes = as.character(Notes),
          visitno =  Notes %>%
             str_extract("(^| )x[0-9]+") %>% gsub("x", "", .) %>% as.numeric,
          vscore = Notes %>%
             str_extract("[0-9]+([.0-9]+)?(/5)?$|[0-9.]+/5") %>%
             gsub("/5", "", .) %>%
             as.numeric
          ) %>%
   select(vid=VisitID, visitno, vscore, Notes)

visit <-
   visit_time %>%
   select(vid, vtimestamp) %>%
   left_join(vtpl,by="vid") %>%
   left_join(visit_pid, by="vid") %>%
   left_join(conf_tp %>% select(-Notes), by="vid") %>%
   mutate(vstatus="checkedin")

## Study
vt <- dbtbl("tVisitStudies")

# get cohort to merge
disorder_lookup <- dbtbl("tDisorders") %>% mutate(Disorder = gsub("None", "", Disorder))
cohort <- vlog %>%
   select(vid=VisitID, DisorderID, Diagnosed, Control) %>%
   left_join(disorder_lookup, by="DisorderID") %>%
   mutate(cohort=paste(sep="_",
                       Disorder,
                       ifelse(Control==1, "control", "")) %>%
                # _control and e.g. PVL_
                gsub("^_|_$", "", .) %>%
                # 2 double controls. they're just control
                gsub("control_control", "control", .) %>%
                # nothing then control
                gsub("^$", "control", .))

#N.B. Diagnosed column dropped. all true for Eiplepsy non-controls. ==1 no where else

visit_study <-
   vt %>% select(-VisitDate, -LunaID) %>%
   melt(id.var="VisitID") %>%
   filter(value==1) %>% 
   group_by(VisitID) %>%
   select(vid=VisitID, study=variable) %>%
   left_join(cohort %>% select(vid, cohort), by="vid")

## visit action
# TODO: add from vlog

## Tasks
task_tables <- grep("^d", tables, value=T)
col2json <- function(d) {
   if (nrow(d) < 1) return(measures=list())
   lapply(1:nrow(d), function(i)
          toJSON(d[i, ], na="string") %>% gsub("^\\[|\\]$", "", .) %>%
          gsub('"NA"', "null", .))
}
mkmeasure <- function(d)  d %>%
   select(vid=VisitID) %>%
   mutate(measures = d %>%
            select(-LunaID, -VisitID, -VisitDate) %>%
            col2json %>%
            unlist)

all_measures <- lapply(task_tables, dbtbl)
visit_measures <-
   lapply(task_tables, function(tn) dbtbl(tn) %>%
                            mkmeasure %>% mutate(task=gsub("^d", "", tn))) %>%
   bind_rows

## Enroll
bircs <- dbtbl("tBIRCIDS") %>%
   rename(vid=VisitID, id=BIRCID, ID=LunaID) %>%
   left_join(p %>% select(ID, pid), by="ID") %>%
   mutate(eid=1:n(), etype="BIRC", edate=mdy_hms(VisitDate)) %>%
   select(eid, pid, etype, id, edate, vid)

# grab first visit for each luna to get edate
# start eid after end of birc ids
lunas <-
   left_join(p %>% filter(fromtbl=="subj"),
             visit,
             by="pid") %>%
   group_by(pid) %>%
   mutate(r=rank(vtimestamp, ties.method="first"),
          etype="LunaID") %>%
   filter(r==1) %>%
   select(pid, etype, id=ID, edate=vtimestamp) %>%
   ungroup() %>%
   mutate(eid=(1:n()) +max(bircs$eid))

# combine lunas and bircs
enroll <- rbind(bircs %>% select(-vid), lunas)

# bircs specific to a visit, add to join table
visit_enroll <- bircs %>% select(eid, vid)

## Notes
subj_notes <- subj %>%
   left_join(p, by=c("LunaID"="ID")) %>%
   select(pid, Dropped, Notes) %>%
   mutate(ndate=ymd_hms(NA), vid=NA)
visit_notes <-
   vlog %>% select(vid=VisitID, Dropped, Notes) %>%
   left_join(visit, by="vid") %>%
   select(pid, vid, ndate=vtimestamp, Dropped, Notes)

# combine
notes_and_dropped <- rbind( subj_notes, visit_notes ) %>%
   filter(Notes!="" | Dropped != 0) %>%
   mutate(nid=1:n())

# extract needed tables
notes <- notes_and_dropped %>% select(nid, pid, ndate, note=Notes)
person_note <- notes_and_dropped %>% filter(is.na(vid)) %>% select(pid, nid)
visit_note <- notes_and_dropped %>% filter(!is.na(vid))  %>% select(vid, nid)


## Drops
dropped_vid <-
   notes_and_dropped %>% filter(Dropped==1) %>%
   mutate(dropcode=ifelse(is.na(vid), "OLDDBSUBJ", "OLDDBVISIT"),
          did=1:n())
dropped <-  dropped_vid %>% select(did, pid, dropcode)
# TODO: visit_drop, drop_note

## Tasks
# TODO: everything
tasks <- dbtbl("tTasks")

## Studies
# TODO: everything


## all done add to db
list(person=p, contact=contacts, visit=visit,
     visit_study=visit_study, visit_tasks=visit_measures,
     enroll=enroll, visit_enroll=visit_enroll,
     note=notes, person_note=person_note, visit_note=visit_note,
     dropped=dropped,
)
