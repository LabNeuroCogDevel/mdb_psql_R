--
-- triggers 
--  o update visit status when insert visit_action
--  o enroll and tie to visit 
--  o note   and tie to visit
-- ---
--  o create a visit with study, action, and note all joined
--  o before remove visit, remove action, note, and study
-- --
--  o checkin visit with tasks, note, and enrollment
--
-- 20190628
-- keep only: person_search_view, visit_person_view, visit_summary





-- ------
--  when a new visit_action is added, update visit's vstatus
-- NB 20190628: consider removing vstatus and age from visit, and use view instead
-- ------ 

create or replace function visit_status_to_visit()
returns trigger
language plpgsql
as $func$
BEGIN
 UPDATE visit SET vstatus = NEW.action where vid = NEW.vid;
 RETURN NEW;
END;
$func$;

create trigger update_visit_status 
 after insert on visit_action
 for each row execute procedure visit_status_to_visit();


--  ----------------------
--  ways to organize notes
--  ----------------------

-- just notes that are for visits
drop view if exists visit_notes CASCADE;
create view visit_notes as
  select
    vid,
    max(ndate) as lastnstamp,
    json_agg(distinct note)  as notes,
    max(dropcode) as maxvdrop
  from note
  where vid is not null
  group by vid;

-- just notes that are for people
drop view if exists person_only_notes CASCADE;
create view person_only_notes as
  select
    pid,
    max(ndate) as lastnstamp,
    json_agg(distinct note)  as notes,
    max(dropcode) as maxpdrop
  from note
  where vid is null
  group by pid;

-- all notes by person
drop view if exists person_all_notes CASCADE;
create view person_all_notes as
  select
    pid,
    max(ndate) as lastnstamp,
    json_agg(distinct note)  as notes,
    max(dropcode) as maxdrop
  from note
  group by pid;


-- --------
-- information view to use instead of visit
-- used to insert status and visit at same time
--  -- should be used instead of a visit with age and vstatus 
--     these are computed columns potentially poluting the data :)
-- --------
drop view if exists visit_summary;
create view visit_summary as
   select
    distinct on (visit.vid)
    person.pid,
    visit.vid,
    visit.vtimestamp,
    visit.vtype,
    visit.vscore,
    visit.visitno,
    visit.googleuri,
    date_part('day',(visit.vtimestamp-person.dob))/365.25 as age,
    visit_study.study, 
    visit_study.cohort, 
    visit_action.action,
    visit_action.ra,
    visit.dur_hr,
    vn.notes,
    vn.maxvdrop as dvisit,
    pn.maxdrop as dperson
   from visit
     natural join person
     natural left join visit_action
     natural left join visit_study
     left join visit_notes vn on vn.vid=visit.vid
     left join person_all_notes pn on pn.pid=person.pid
   order by visit.vid, visit_action.vatimestamp desc;

-- --------
-- REMOVE VISIT, likely via "cancel"
-- this should have been a on delete cascade ?
-- remove actions and studies
-- but panic if we have visit_task ore visit_measures
-- --------

create or replace function delete_visit_actions_studies()
 returns trigger 
 language plpgsql
 as $$
DECLARE
 cnt integer;
BEGIN
 cnt :=
    -- only scheduled
    (select count(*) as nacts from visit_action where vid=OLD.vid and action not in ('sched'::status, 'assigned'::status))::integer +
    -- already enrolled, dont detele anything
    (select count(*) as nenroll from visit_enroll where vid=OLD.vid)::integer +
    -- have tasks, dont delte anything
    (select count(*) as ntasks from visit_task   where vid=OLD.vid)::integer;
 IF cnt > 0 THEN
    RAISE EXCEPTION 'Cannot remove visit % b/c status is not sched or have enrolled or have tasks',OLD.vid;
 END IF;

 delete from visit_action where vid = OLD.vid; 
 delete from visit_study  where vid = OLD.vid; 
 delete from note using visit_note where visit_note.nid = note.nid and visit_note.vid = OLD.vid ;
 RETURN OLD;

END;
$$;

create trigger remove_visit_cascade before delete on visit
for each row
execute procedure delete_visit_actions_studies();


---- actual visit summary view
-- query like:
--  select * from visits_view where studies::jsonb @> '[{"study": "MEGEmo"}]';
drop view if exists visits_view;
create view  visits_view as
 select
    max( (select e.id from (select e.id) as _  where e.etype like 'LunaID')) as lunaid,
    to_char(vtimestamp,'YYYYmmdd') as ymd,
    max(dropcode.droplevel) as subjmaxdrop,
   p.fname,p.lname,p.sex,p.dob,v.*,
  json_agg(distinct vn.maxvdrop) as visit_dropped,
  json_agg( distinct (select row_to_json(_) from (select vs.study, vs.cohort) as _)::jsonb ) as studies,
  json_agg( (select row_to_json(_) from (select e.etype, e.id) as _) ) as enrolls,
  json_agg( (select row_to_json(_) from (select va."action", va.ra, va.vatimestamp) as _) ) as actions,
  json_agg(vn.notes)->0 as notes, -- already collapsed so just need first
  json_agg(distinct t.task) as tasks
  from visit v natural join visit_study vs
   natural left join visit_task  t
   natural left join person p
   natural left join enroll e
   natural left join visit_action va
   left join visit_notes vn on vn.vid = v.vid
   left join person_all_notes an on an.pid = v.vid
   left join dropcode on an.maxdrop = dropcode.dropcode
  group by p.pid,v.vid;
 


-- person search view
drop view if exists person_search_view;
create view person_search_view as
select
   p.pid,
   -- lunaid: from grouped pid: 
   -- get get distinct id from all id's of etype=lunaid
   -- get the first one (should only every be one) and convert to text
   -- NULL if text is "null"
   -- turn into intenger
   NULLIF((json_agg(DISTINCT e.id) FILTER (WHERE e.etype::text ~~ 'LunaID'::text) ->> 0)::text, 'null'::text)::int AS lunaid,
   -- full name is first and last combined
   concat(p.fname,' ',p.lname) as fullname,
   p.fname,
   p.lname,
   p.dob,p.sex,p.hand,p.adddate,p.source,
   -- cur age is now - date of birth
   date_part('day',(now()-dob))/365.25 as curage,
   -- but we also want to search on whole numbers so floor the current age 
   floor(date_part('day',(now()-dob))/365.25) as curagefloor,
   -- summary stats on visits
   max(v.vtimestamp)                   as lastvisit,
   count(distinct v.vid)               as numvisits,
   count(distinct vs.study)        as nstudies,
   count(distinct d.nid)           as ndrops,
   -- all ids as array of ojbects[{id:, etype:, edate:},...]
   json_agg( distinct (select _ from ( select id,etype,edate ) as _ ) )::jsonb as ids,
   -- counts
   json_agg(distinct vs.study)::jsonb     as studies,
   json_agg(distinct v.vtype)::jsonb      as visittypes,
   max(dc.droplevel)               as maxdrop
 from person p
   left join visit v   on v.pid=p.pid and v.vstatus in ('sched','checkedin','complete')
   left join enroll e  on e.pid=p.pid
   left join visit_study vs on v.vid=vs.vid
   left join note d      on d.pid=p.pid and d.dropcode is not null
   left join dropcode dc    on dc.dropcode = d.dropcode
 group by p.pid;
