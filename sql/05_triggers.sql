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




-- ------
--  when a new visit_action is added, update visit's vstatus
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



-- ----
-- visit_note_view so we can insert a visit note without having to first insert a note
drop view if exists visit_note_view;
create view visit_note_view as
 select pid,vid,nid,ndate,note,ra from visit natural join note;

-- --- add note and visit_note at the same time
create or replace function insert_new_visit_note()
 returns trigger
 language plpgsql
as $$
DECLARE
 noteid int;
BEGIN
 -- get pid if we dont have one
 if new.pid is null and 
    new.vid is not null then
    new.pid := (select pid from visit where vid = new.vid limit 1);
 end if;

 if new.nid  is     null and 
    new.note is not null then

   -- note table, nid column, serial primary key sequence generated
   noteid = nextval('note_nid_seq'); 

   INSERT into note (nid,pid,ra,ndate,note) values (noteid,NEW.pid,NEW.ra,now(), NEW.note);
   INSERT into visit_note (vid,nid) values (NEW.vid,noteid);
   return new;
 else
    RAISE EXCEPTION 'cannot insert visit_note b/c nid is not null, note is null, or dont have personid';
 end if;
   
END;
$$;

-- add trigger to view with instead of insert
create trigger new_visit_note_trigger instead of insert on visit_note_view
for each row
execute procedure insert_new_visit_note();


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
    person.pid,visit.vid,visit.vtimestamp,visit.vtype,visit.vscore,visit.visitno,visit.googleuri, 
    date_part('day',(visit.vtimestamp-person.dob))/365.25 as age,
    visit_study.study, 
    visit_study.cohort, 
    visit_action.action,
    visit_action.ra,
    visit.dur_hr,
    note.note,
    vd.dropcode as dvisit,
    pd.dropcode as dperson
   from visit
     natural join person
     natural join visit_action
     natural left join visit_note
     natural left join visit_study
     left join note on visit_note.nid = note.nid
     left join visit_drop on visit.vid = visit_drop.vid
     left join dropped as vd on visit_drop.did=vd.did
     left join dropped as pd on pd.pid = person.pid
   order by visit.vid, visit_action.vatimestamp desc ,note.ndate desc;

-- --------
-- add a visit_action for 'sched' when a visit is added without a vstatus
-- we need to be given an RA field, so we use an insert on the view to get it
-- --------
create or replace function insert_new_visit_ra() 
 returns trigger
 LANGUAGE plpgsql 
as $$
BEGIN
 NEW.vid := nextval('visit_vid_seq'); -- visit table, vid column, serial primary key sequence generated
 NEW.age := (select date_part('day',(NEW.vtimestamp-dob))/365.25 as age from person where pid = NEW.pid);
 -- --- add visit --- --
 -- another trigger will  set visit's vstatus when adding the action below
 INSERT into visit (pid,vid,vtype,vscore,age,vtimestamp,visitno,googleuri,dur_hr) values
    (new.pid,new.vid,new.vtype,new.vscore,new.age,new.vtimestamp,new.visitno,new.googleuri,new.dur_hr);

 -- --- add action --- --
 -- if we dont specify an action we are schedualing, and use the now time
 if new.action is null then
  INSERT into visit_action (action,ra,vatimestamp,vid) values ('sched',NEW.ra,now(), NEW.vid);
 -- otherwise we are adding something that already happened
 -- use the date provided in vtimestamp and the action specified
 -- probably only useful for action == 'complete'
 else
  INSERT into visit_action (action,ra,vatimestamp,vid) values (new.action,NEW.ra,new.vtimestamp, NEW.vid);
 end if;

 -- --- add note --- --
 if new.note is not null then
   insert into visit_note_view (vid,note,ra) values (NEW.vid,NEW.note,NEW.ra);
 end if;

 -- --- add study --- --
 if new.study is not null then
   INSERT into visit_study (vid,study,cohort) values (NEW.vid,NEW.study,NEW.cohort);
 end if;

 RETURN NEW;
END;
$$;

create trigger update_visit_status instead of insert on visit_summary
for each row
execute procedure insert_new_visit_ra();





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
 delete from visit_note   where vid = OLD.vid; 
 delete from note using visit_note where visit_note.nid = note.nid and visit_note.vid = OLD.vid ;
 RETURN OLD;

END;
$$;

create trigger remove_visit_cascade before delete on visit
for each row
execute procedure delete_visit_actions_studies();


-- ------
-- checkout view mainly for quick insert/update
-- ------
-- drop view if exists visit_checkin_view;
-- create view  visit_checkin_viewas
--  select 
--   v.vid,v.score,v.status
-- 

-- --------
-- view a visit and person with notes for only that person or visit
-- --------
drop view if exists visit_person_view;
create view visit_person_view as
 select 
   v.*, p.fname, p.lname,p.dob,p.hand,p.sex,
   json_agg(distinct (select row_to_json(_) from (select e.id,e.etype) as _)::jsonb ) as ids,
   json_agg(distinct (select row_to_json(_) from (select s.cohort,s.study) as _)::jsonb ) as studys,
   json_agg(distinct n.note)   as notes
  from visit v
   join person p  on p.pid = v.pid
   left join visit_study s on s.vid = v.vid
   left join note         n on p.pid = n.pid
   left join visit_note  vn on n.nid= vn.nid
   left join enroll e on p.pid = e.pid
  where vn.vid = v.vid or vn.vid is null
  group by v.vid, p.pid;



-- -----------------------------
-- view for enroll+visit
-- add enrollment with visit joined

drop view if exists enroll_visit_enroll_view;
create view enroll_visit_enroll_view as
 select * from enroll natural join visit_enroll;

-- insert into enroll_visit_enroll_view (vid,etype,id) values (3893,'LunaID','9')
create or replace function insert_new_visit_id()
 returns trigger
 language plpgsql
 as $$
BEGIN
 new.eid = nextval('enroll_eid_seq'); 
 new.pid = (select pid from visit where vid = new.vid limit 1);
 insert into enroll (eid,pid,etype,id,edate) values (new.eid,new.pid,new.etype,new.id,now());
 insert into visit_enroll (eid,vid) values (new.eid,new.vid);
 return new;
END
$$;

-- actually trigger
create trigger enroll_visit_enroll_trigger 
  instead of insert on enroll_visit_enroll_view
  for each row
  execute procedure insert_new_visit_id();





-- --------
-- CHECKIN VISIT
-- help from http://stackoverflow.com/questions/34955742/insert-for-each-element-in-jsonb-array-with-instead-of-trigger-on-a-view
-- --------
-- insert into visit_checkin_view (vid,ra,vscore,note,ids,tasks) values
-- (3893,'testRA',4,'TEST CHECKINNOTE', 
--  '[{"etype": "TestID", "id": "9"}, {"etype": "LunaID", "id": "9"}]'::jsonb,
--  '["fMRIRestingState","SpatialWorkingMem","ScanSpit"]'::jsonb);

drop view if exists visit_checkin_view;
create view visit_checkin_view as
 select 
  v.vid::integer,
  json_agg(distinct (select row_to_json(_) from (select e.id,e.etype) as _)::jsonb )::jsonb as ids,
  json_agg(distinct (select row_to_json(_) from (select s.cohort,s.study) as _)::jsonb ) as studys,
  vscore,
  max(va.ra) as ra, -- exect only sched, so no other RAs, use max to make sure we only get one
  array_agg(distinct n.note)::text as note,
  json_agg(distinct (select row_to_json(_) from (select vt.vtid, vt.task, vt.measures,vt.files) as _)::jsonb )::jsonb as tasks
 from visit v
  join person p on p.pid=v.pid 
  left join visit_note vn on v.vid = vn.vid
  left join visit_task vt on v.vid=vt.vid
  left join visit_study s on s.vid=v.vid
  left join note n on vn.nid = n.nid 
  left join enroll e on p.pid = e.pid
  join visit_action va on v.vid = va.vid
  where v.vstatus = 'sched'::status
 group by v.vid,p.pid;


-- -- helper function to make measurement array into null map
create or replace function array_to_nullvalmap(jsonb)
returns jsonb language sql as $$
    select json_object_agg(e, null)::jsonb
    from json_array_elements_text($1::json) e
$$;


-- trigger update status/score, insert ids, insert notes
create or replace function checkin_visit()
 returns trigger 
 language plpgsql
 as $$
BEGIN

    -- visit score and status
    update visit set vscore = new.vscore::numeric
     where vid = NEW.vid::integer;

    -- inserting into visit_action also updates vstatus in visit table
    insert into visit_action (vid,action,ra,vatimestamp) 
       values (NEW.vid::integer,'checkedin'::status, NEW.ra,now());

    -- insert note
    if NEW.note is not null then
      insert into visit_note_view (vid,note,ra) values (new.vid,NEW.note,NEW.ra);
    end if;


    -- tasks
    if NEW.tasks is not null and 
       NEW.tasks::text not like 'null' then

      -- raise EXCEPTION 'new.tasks is %',new.tasks::text;

      insert into visit_task (vid,task,measures)
       select vid::integer, task, array_to_nullvalmap(measures)
       from (
           select NEW.vid::integer, task
           from jsonb_array_elements_text(NEW.tasks) task
           ) sub
       join task using(task);
    end if;

    -- enroll ids
    if NEW.ids is not null and 
       NEW.ids::text not like 'null' then
      insert into enroll_visit_enroll_view  (vid,etype,id)
        select new.vid, 
               t->>'etype' as etype,
               t->>'id' as id  
        from jsonb_array_elements(NEW.ids) t;
    end if;

    -- TODO: drop reasons

    return new;
END
$$;

create trigger checkin_visit_trigger instead of insert on visit_checkin_view
for each row
execute procedure checkin_visit();







---- actual visit summary view
-- query like:
--  select * from visits_view where studies::jsonb @> '[{"study": "MEGEmo"}]';

drop view if exists visits_view;
create view  visits_view as
 select
   max( (select e.id from (select e.id) as _  where e.etype like 'LunaID')) as lunaid,
   to_char(vtimestamp,'YYYYmmdd') as ymd,
   max(dc.droplevel) as subjmaxdrop,
  p.fname,p.lname,p.sex,p.dob,v.*,
 json_agg(distinct d.dropcode) as visit_dropped,
 json_agg( distinct (select row_to_json(_) from (select vs.study, vs.cohort) as _)::jsonb ) as studies,
 json_agg( (select row_to_json(_) from (select e.etype, e.id) as _) ) as enrolls,
 json_agg( (select row_to_json(_) from (select va."action", va.ra, va.vatimestamp) as _) ) as actions,
 json_agg(distinct n.note) as notes,
 json_agg(distinct t.task) as tasks
 from visit v natural join visit_study vs
  natural left join visit_task  t 
  natural left join person p
  natural left join enroll e
  natural left join visit_action va
  -- visit notes
  natural left join visit_note vn left join note n on n.nid = vn.nid
  -- visit drops
  natural left join visit_drop vd left join dropped d on vd.did = d.did  and d.dropcode is not null
  -- person drop?
  left join dropped pd on p.pid = pd.pid
  left join dropcode dc on pd.dropcode=dc.dropcode
 group by p.pid,vid;
 



--- contacts_view
-- returns nested:
-- pid,relation,who,lastcontact,contacts:[{cid,ctype,cvalue,nogood,note:[{note,ndate,ra}] }]
drop view if exists contacts_view;
create view  contacts_view as
 select c.pid,  relation,  who, max(lastcontact) as lastcontact,
   json_agg( (select row_to_json(_) from (
              select cid,ctype,cvalue,nogood,n.notes) as _) ) as contacts
   from contact  c
   left join ( select
     cn.cid,
     max(nn.ndate) as lastcontact,
     json_agg( (select row_to_json(_) from (select note,ndate,ra) as _)  ) as notes
     -- json_agg(note)
     from contact_note cn 
     left join note nn 
       using(nid) 
     group by cn.cid  ) n using (cid)
   group by c.pid, who,relation;


--- drops

drop view if exists drops_view;
create view drops_view as
  select did,pid,vid,nid,dropcode,droplevel,n.ra,n.ndate,n.note
   from dropped d
   natural join dropcode dc
   natural left join visit_drop vd
   natural left join drop_note dn
   natural left join note n;


-- trigger update status/score, insert ids, insert notes
create or replace function add_drop_from_view()
 returns trigger 
 language plpgsql
 as $$
BEGIN
    -- NEW has: did,pid,vid,nid,dropcode,droplevel,ra,ndate,note

    -- if we are lazy and have a vid but no pid, get the pid
    if NEW.vid is not null and NEW.pid is null then
      new.pid := (select pid from visit where vid = new.vid limit 1);
    end if;

    -- we are inserting a drop 
    -- get id
    NEW.did := nextval('dropped_did_seq'); 
    -- insert
    insert into dropped
      (did,pid,dropcode) values
      (NEW.did,NEW.pid,NEW.dropcode);
  
    -- insert note (timestamp as now)
    NEW.nid := nextval('note_nid_seq'); 
    insert into note
      (nid,pid,ra,note,ndate) values
      (NEW.nid,NEW.pid,NEW.ra,NEW.note,now());
    
    -- join new note to this new drop
    insert into drop_note (nid,did) values (NEW.nid,NEW.did);
  
    -- add to visit_note and visit_drop if we have a visit id
    if NEW.vid is not null then
      insert into visit_note (vid,nid) values (NEW.vid,NEW.nid);
      insert into visit_drop (vid,did) values (NEW.vid,NEW.did);
    else
      insert into person_note (pid,nid) values (NEW.pid,NEW.nid);
    end if;


    return new;
END
$$;

create trigger add_drop_from_view_trigger instead of insert on drops_view
for each row
execute procedure add_drop_from_view();






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
   count(distinct d.did)           as ndrops,
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
   left join dropped d      on d.pid=p.pid
   left join dropcode dc    on dc.dropcode = d.dropcode
 group by p.pid;


   -- to get all ids, previously used below -- many repeats
   -- json_agg( (select row_to_json(_) from (
   --                          select id,etype,edate) as _) ) as ids,


-- ----------
-- ROLES for postgrest
-- https://compose.io/articles/your-sql-schema-is-your-json-api-with-postgrest/
-- ----------
CREATE ROLE lncd;
grant SELECT, INSERT, UPDATE,  DELETE on ALL tables in schema public to lncd;
-- allow lncd to play use triggers (seq incrementers)
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO lncd;

CREATE ROLE anonymous;
-- GRANT SELECT, INSERT ON TABLE users TO anon;  
-- GRANT EXECUTE ON FUNCTION  
--   login(text,text),
--   TO anonymous;
