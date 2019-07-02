
create table person (
   pid     serial primary key  ,
   fname   varchar(50) not null,
   lname   varchar(50) not null,
   dob     date        not null,
   sex     char(1)     not null,
   hand    char(1)     not null,
   nick    varchar(50)         ,
   addDate date                ,
   source  varchar(50)         ,
   unique(fname,lname,dob)
);

-- needed for ndar guid. get from survey
--  mname   varchar(50)         ,
--  birth_city varchar(50)      ,

-- score a subject 
-- responsive, performant, instructable
create table person_score (
  pid int references person(pid) not null,
  ra varchar(50),
  stimestamp timestamp,
  pscore float
);

create table contact (
   cid      serial primary key,
   pid      int references person(pid),
   ctype    varchar(50)  not null,
   cvalue   varchar(200) not null,
   relation varchar(50),
   who      varchar(50),
   added    timestamp,
   -- note       text,    --- e.g. call between 9-5
   unique (who,ctype,cvalue,relation,pid) -- use pid b/c same person might be 2 contacts
);
-- use contact_note -> note for contacted


create table enroll (
   eid     serial primary key  ,
   pid     int references person(pid) not null,
   etype   varchar(50) not null,
   id      varchar(20) not null,
   edate   date,
   unique (etype,id)
);

create table screener (
  pid          int references person(pid) not null,
  stimestamp   timestamp,
  measures jsonb
);

create table family (
  pidy int references person(pid) not null, -- younger
  pido int references person(pid) not null, -- older
  gendist int not null, -- 0: same generation; 1: one generation between; ....
  nucdist int not null, -- 0: same family;     1: e.g. cousin, aunt;      2: dad's cousin
  fdesc varchar(50) -- e.g. 2nd cousins
);



-- onto visits --------------------------
create type status as enum ('sched','assigned', 'complete','checkedin','cancelled','noshow','unknown','other');
create table visit (
   vid     serial primary key  ,
   pid     int references person(pid) not null,
   vtype   varchar(20) not null, -- MEG, Behave, PET, MR,
   vscore  numeric(3,2),  -- allow e.g 4.25
   dur_hr  float,         -- duration in hours
   age   float,
   vtimestamp   timestamp,
   visitno int,
   googleuri varchar(255),
   -- visit status, reference to latest visit_action, circular is bad
   -- aid  int references visit_action(aid)

   -- status of visit -- redudent info in visit_action
   vstatus status,
   unique (pid,vtimestamp,vtype)
);


create table task (
  task varchar(50) primary key,
  tdesc text,
  measures jsonb, -- array of messures, value type
  files jsonb,    -- array of files to collect, default location
  settings jsonb, -- e.g dist from mirror to projector
  modes jsonb     -- scan,MEG,survey, behave
);

create table study (
  study varchar(50) primary key,
  grantname varchar(50),
  cohorts jsonb, -- expect array
  visit_types jsonb -- MEG, PET, MRx1
);

-- checkin  schedual cancel etc
create table visit_action (
  aid     serial primary key,
  vid     int references visit(vid) not null,
  action  status not null,
  ra      varchar(50) not null,
  vatimestamp timestamp
);

create table visit_study(
  vid int references visit(vid) not null,
  study varchar(50) references study(study) not null,
  cohort varchar(50) not null, -- default "control"
  unique(vid,study) -- visits should only be in a study once
);

create table visit_task(
  vtid     serial primary key  ,
  vid      int references visit(vid) not null,
  task     varchar(50) references task(task) not null,
  measures jsonb,
  files     jsonb,
  unique(vid,task) -- any one task can only be in a visit once
);

create table vt_analysed(
  vtid int references visit_task(vtid) not null,
  name varchar(50) not null, -- name for analysis, searchable
  data jsonb not null
);

create table visit_old(
  vid int references visit(vid) not null,
  ovid int not null
);


-- tie studies and tasks together
create table study_task (
  study varchar(50) references study(study),
  task varchar(50) references task(task),
  unique(study,task)
);

create table diag(
  pid   int references person(pid) not null,
  diag  varchar(50) not null,
  onset timestamp,
  better timestamp,
  known timestamp,
  isrx  boolean
);

----- drops and notes ----------

-- nodrop < future < sometasks < visit < somevisits < subject < family
create type droplevels as enum('nodrop','future','sometasks','visit','somevisits','mostvisits','subject','family');

-- any drop:
-- select * from droplevels where droplevel > 'nodrop' and editof is null;
-- select max(dropcode.droplevel) from .... join person_note join visit_note join note join dropcode where note.editof is not null

-- its own table so we are consistant
create table dropcode (
  dropcode varchar(15) primary key,
  droplevel droplevels not null
);


create table note (
 nid serial primary key,
 pid int references person(pid) not null,
 vid int references visit(vid),
 dropcode varchar(15) references dropcode(dropcode),
 ra    varchar(50), -- not null,
 ndate timestamp, -- not null,
 note  text not null
);


-- contact status = ways people could have interactied
create type cstatus as enum ('update_info','bad_info', 'realtime', 'sent_waiting', 'replied');
-- alter type cstatus add value 'dont_use'
create table contact_note (
  cnid        serial primary key,
  cid         int references contact(cid) not null,
  cstatus     cstatus, 
  ctimestamp  timestamp,
  detail      text
);

-- some IDs are unique to a visit (e.g. BIRC)
create table visit_enroll (
  eid   int references enroll(eid) not null,
  vid   int references visit(vid) not null
);

create table ra (
   ra varchar(50) not null,
   upmcid varchar(50),
   abbr varchar(4),
   start_date timestamp,
   end_date timestamp,
   bday timestamp,
   studies json
);



 -- start out with some drop codes
INSERT into dropcode (dropcode,droplevel) values ('OLDDBDSUBJ','subject');
INSERT into dropcode (dropcode,droplevel) values ('OLDDBDVIST','visit');

INSERT into dropcode (dropcode,droplevel) values ('NOT',       'nodrop'); -- dont need this anymore 

INSERT into dropcode (dropcode,droplevel) values ('NOINTEREST','future'); -- 
INSERT into dropcode (dropcode,droplevel) values ('HARD_SUBJ' ,'future'); -- 
INSERT into dropcode (dropcode,droplevel) values ('TASK_ISSUE','sometasks');
INSERT into dropcode (dropcode,droplevel) values ('TECH_ISSUE','visit');
INSERT into dropcode (dropcode,droplevel) values ('EYE_ISSUE' ,'visit');
INSERT into dropcode (dropcode,droplevel) values ('SUBJ_ISSUE','visit');

INSERT into dropcode (dropcode,droplevel) values ('CLAUSTROPHOBIC','somevisits');
INSERT into dropcode (dropcode,droplevel) values ('BAD_VEIN'  ,'somevisits');

INSERT into dropcode (dropcode,droplevel) values ('LOWIQ'     ,'subject');
INSERT into dropcode (dropcode,droplevel) values ('EXCLDCRTRA','subject');

INSERT into dropcode (dropcode,droplevel) values ('DEPRESSED' ,'family');

-- 20190702 from 7T sheet
INSERT into dropcode (dropcode,droplevel) values ('MRI_UNCOMFY' ,'somevisits');
INSERT into dropcode (dropcode,droplevel) values ('INATTENTIVE' ,'subject');
INSERT into dropcode (dropcode,droplevel) values ('PSYCH_CLINICAL', 'subject');
INSERT into dropcode (dropcode,droplevel) values ('METAL', 'somevisits');
INSERT into dropcode (dropcode,droplevel) values ('MOVER', 'somevisits');
INSERT into dropcode (dropcode,droplevel) values ('BADSUBJ', 'subject');
INSERT into dropcode (dropcode,droplevel) values ('NO_ADULT', 'somevisits');




-- GRANT CONNECT ON DATABASE lncddb TO lncd;
-- GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO web;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO lncd;
