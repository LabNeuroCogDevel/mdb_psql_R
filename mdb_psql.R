#!/usr/bin/env Rscript

# run me from Makefile:
# `make all` (file sourced with `make data` between `schema` and `triggers`)

# in 2010 RODBC didnt work iwth mdbtools
# https://stat.ethz.ch/pipermail/r-help/2010-April/236983.html
# install unixodbc{,-dev} odbc-mdbtools
# edit /etc/odbc.ini to include
# check with odbcinst -j ; isql -v LunaJetDB
# library(RODBC)
# con <- odbcConnect("LunaJetDB")
# sqlTables(con, tableType = "TABLE")$TABLE_NAME

library(Hmisc) # mdb.get
library(lubridate)
library(tidyr)
library(jsonlite)
library(reshape2)
library(stringr)
library(dplyr)
# library(LNCDR) # LNCDR::col_ungroup for contact

logit<-function(...) print(paste0("[", now(), "] ", ...))

# install mdbtools, use Hmisc::mdb.get 
db <- "LunaDB.mdb"
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

logit("reading in all people and visits")
## Person
# all people into one table
subj <- dbtbl("tSubjectInfo")  %>% mutate(fromtbl="subj",    ID=LunaID)
# remove any duplicates in hotline
hotl <- dbtbl("tHotline")      %>% mutate(fromtbl="hotline", ID=HotlineID) %>%
        filter(SubjectLastName!="", SubjectLastName!="",
              !duplicated(paste(SubjectFirstName, SubjectLastName)))

# combine everyone
allp <- rbind(subj %>% select(-LunaID),
              hotl %>% select(-HotlineID)) %>%
        filter(!duplicated(paste(ID,fromtbl))) %>%
        mutate(pid=1:n())

p_withdups <- allp %>%
     select(pid, ID, fromtbl,
            fname=SubjectFirstName,
            lname=SubjectLastName,
            sex=SexID,
            hand=HandID,
            dob=DateOfBirth) %>%
     mutate(dob=as.Date(dbdate(dob)),
            sex=cut(sex, breaks=c(-1, 0,   1,   2),
                         labels=c(   "U", "M", "F") ),
            hand=factor(as.character(hand),
                        labels=list("-1"="U", "1"="R", "2"="L", "3"="A")))

# grab lunaid of duplicated
dupidx <- duplicated(p_withdups %>% select(fname, lname, dob)) # Dups 11046 and 11063
p_dups <- p_withdups[dupidx, ]
p_nodups <- p_withdups[!dupidx, ]
to_enroll <- inner_join(p_dups %>% select(-pid),
           p_nodups %>% select(pid, fname, lname, dob, origid=ID))
p <- rbind(p_nodups, to_enroll %>% select(-origid))

replace_lookup <- p_dups %>%
    select(duppid=pid, lname, fname, dob) %>%
    inner_join(p_nodups %>% select(usepid=pid, lname, fname, dob)) %>%
    select(usepid, duppid)

replace_dup_pid <- function(d) {
   d %>%
      left_join(replace_lookup, by=c("pid"="duppid")) %>%
      mutate(pid=ifelse(!is.na(usepid), usepid, pid)) %>%
      select(-usepid)
}
## contacts

contacts <- allp %>%
   replace_dup_pid %>%
   select(pid, matches("Contact.*")) %>%
   LNCDR::col_ungroup("^Contact[0-9]", "relation") %>%
   unite("who", FirstName, LastName, sep=" ") %>%
   unite("Address", Address1, Address2, City, State, ZipCode, sep=" ") %>%
   gather("ctype", "cvalue", -pid, -who, -relation) %>%
   mutate(cvalue = cvalue %>%
                  gsub("(^| )NA( |$)", " ", .) %>%
                  gsub(" +", " ", .) %>%
                  gsub("^ ?PA ?$", "", .),
         # remove non-numbers from phone numbers
         cvalue = ifelse(grepl("Phone", ctype),
                         gsub("[^0-9]", "", cvalue), cvalue)
   ) %>%
   filter(!grepl("^[-() ,]*$", cvalue),
          !is.na(cvalue),
          cvalue!="",
          !duplicated(.))


## Visits

vlog <- dbtbl("tVisitlog") %>%
   mutate_at(vars(VisitTime, VisitDate),
             function(x) x %>% as.character %>% mdy_hms) %>%
   filter(VisitID!=3418) # duplicate pid + vtype + vtimestamp

# pid and age
visit_pid <- merge(vlog, p, by.x="LunaID", by.y="ID")  %>%
   mutate(age=as.numeric(ymd(VisitDate) - dob) / 365.25) %>%
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
             str_extract("([^0-9-]|^)[0-5](\\.[0-9])?(/5)?$|[0-5](\\.[0-9])?/5") %>%
             gsub("/5", "", .) %>%
             as.numeric
          ) %>%
   select(vid=VisitID, visitno, vscore, Notes)

visit_withdups <-
   visit_time %>%
   select(vid, vtimestamp) %>%
   left_join(vtpl, by="vid") %>%
   left_join(visit_pid, by="vid") %>%
   left_join(conf_tp %>% select(-Notes), by="vid") %>%
   mutate(vstatus="checkedin")

visit <- visit_withdups %>% filter(!duplicated(paste(pid, vtype, vtimestamp)))

## deal with duplicate visits
replace_lookup_vid <-
    visit_withdups %>%
    filter(duplicated(paste(pid, vtype, vtimestamp))) %>%
    select(dupvid=vid, pid, vtype, vtimestamp) %>%
    inner_join(visit %>% select(usevid=vid, pid, vtype, vtimestamp)) %>%
    select(usevid, dupvid)

replace_dup_vid <- function(d) {
   d %>%
      left_join(replace_lookup_vid, by=c("vid"="dupvid")) %>%
      ungroup %>%
      mutate(vid=ifelse(!is.na(usevid), usevid, vid)) %>%
      select(-usevid) %>%
      filter(!duplicated(.))
}

# TODO: remove vstatus and age? use visit_summary view instead?

## Study
vt <- dbtbl("tVisitStudies") %>% filter(VisitID != 3418)

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
   left_join(cohort %>% select(vid, cohort), by="vid") %>%
   replace_dup_vid


## Tasks
logit("identifying all tasks")
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

logit("reading in all measures for all visits (big db read)")
all_measures <- lapply(task_tables, dbtbl)

logit("all visits to json")
visit_task_list <-
   mapply(function(tb, dn) mkmeasure(tb) %>% mutate(task=gsub("^d", "", dn)),
         tb=all_measures, dn=task_tables)
logit("making visit_task (large bind_rows)")
visit_task <-
   visit_task_list %>%
   bind_rows %>%
   replace_dup_vid

## Enroll
logit("setting up enrollment, notes, and drops")
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
visit_enroll <- bircs %>% select(eid, vid) %>% replace_dup_vid

## Notes
subj_notes <- subj %>%
   left_join(p, by=c("LunaID"="ID")) %>%
   select(pid, Dropped, Notes) %>%
   mutate(ndate=ymd_hms(NA), vid=NA)
visit_notes <-
   vlog %>% select(vid=VisitID, Dropped, Notes) %>%
   replace_dup_vid %>%
   left_join(visit, by="vid") %>%
   select(pid, vid, ndate=vtimestamp, Dropped, Notes)

# combine
notes_and_dropped <- rbind( subj_notes, visit_notes ) %>%
   filter(Notes!="" | Dropped != 0) %>%
   mutate(nid=1:n())

# extract needed tables
notes <- notes_and_dropped %>% select(nid, pid, ndate, note=Notes)
person_note <- notes_and_dropped %>% filter(is.na(vid)) %>% select(pid, nid)
visit_note <-
   notes_and_dropped %>% filter(!is.na(vid)) %>%
   select(vid, nid) %>%
   replace_dup_vid


## Drops
dropped_vid <-
   notes_and_dropped %>% filter(Dropped==1) %>%
   mutate(dropcode=ifelse(is.na(vid), "OLDDBDSUBJ", "OLDDBDVIST"),
          did=1:n())

# dropped main table
dropped <-  dropped_vid %>% select(did, pid, dropcode)

# join tables
drop_note  <- dropped_vid %>% select(nid, did)
drop_visit <- dropped_vid %>% select(vid, did)



## Tasks: task, measures, files, modes
tasks <- dbtbl("tTasks")

# task and "modes"
task_modes <- tasks %>%
 select(task=Task, Behavioral, Scan, MEG, Questionnaire) %>%
 melt(id.var="task") %>%
 filter(value == 1) %>%
 group_by(task) %>%
 summarise(modes = toJSON(variable)) %>%
 mutate(task = as.character(task))

# task_tables already defined for visit_tasks
# but we could get from tasks table like:
# task_tables <- tasks$TableName %>% as.character %>% .[.!=""]

# this is probably redudent with visit_task creation
# slow. why to just get schema instead of all data?

logit("building list of all tasks")
#task_measure_lists <- lapply(task_tables, function(x) dbtbl(x) %>% names )
task_measure_lists <- lapply(all_measures, names)
task_measurements <-
   data.frame(task=gsub("^d", "", task_tables),
              measures=sapply(task_measure_lists,
                        function(x) x %>%
                         grep("VisitID|LunaID|VisitDate", ., value=T, invert=T) %>%
                         toJSON)
              ) %>% mutate_all(as.character)

task <- left_join(task_modes, task_measurements, by="task")

# TODO: files

logit("assocating with studies and actions")
# associate tasks with studies
study_task <- tasks %>%
 select(task=Task,
        Cannabis, MEGEmo, CogR01, RewardR01,
        EBS, PVL, SlotReward, RingReward) %>%
 melt(id.var="task") %>%
 filter(value == 1) %>%
 select(study=variable, task)




## Studies
study <- data.frame(study= c(study_task$study %>% as.character %>% unique,
                           "SlotReward", "RingReward", "RewardR21")) %>%
  mutate(grantname=study)


## visit action
# aid vid action ra vatimestamp
visit_action <-
 vlog %>%
 select(vid=VisitID, sched=ScheduledBy, checkedin=CheckedInBy) %>%
 mutate_all(as.character) %>%
 # if no one checked it in, say it was checked in
 mutate(checkedin=ifelse(paste0(sched,checkedin) == "", 'OLDDB', checkedin)) %>%
 # put each action (sched, checkedin) on it's own row, remove empty actions
 melt(id.var="vid") %>% filter(value!="") %>%
 # add aid and ra column (remove 1upmc-acct\)
 mutate(aid=1:n(),
        ra = gsub(".*\\\\", "", value),
        vid=as.numeric(vid)) %>%
 select(aid, vid, action=variable, ra) %>%
 replace_dup_vid
 

missing_tasks <-
   visit_task %>% select(tk=task) %>%
   group_by(tk) %>% tally %>%
   filter( ! tk  %in% task$task) %>%
   mutate(task=tk, modes="[]", measures="[]") %>%
   select(task, modes, measures)


logit("adding to db")
## collect everything we should add to DB
dbdfs <- list(
     person=p %>% select(-ID, -fromtbl) %>% filter(!duplicated(pid)),
     visit=visit, study=study,
     task=rbind(task, missing_tasks), contact=contacts,
     visit_study=visit_study, visit_task=visit_task,
     enroll=enroll, visit_enroll=visit_enroll,
     note=notes, person_note=person_note, visit_note=visit_note,
     dropped=dropped, study_task=study_task,
     drop_note=drop_note, drop_visit=drop_visit, visit_action=visit_action
)

## add to databae
# run `make schema` before running this

library(DBI)
# readRenviron(".Renviron") # db settings stored there (and in ~/.pgpass)
con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname="lncddb_r",
                 user=Sys.getenv("pg_user"))
for (tblname in names(dbdfs)) {
   dbWriteTable(con, tblname, dbdfs[[tblname]], row.names=F, append=T)
}
