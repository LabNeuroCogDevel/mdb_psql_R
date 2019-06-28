-- keys were manually inserted need to update sequences
select setval('person_pid_seq', (select max(pid) from person));
select setval('visit_vid_seq', (select max(vid) from visit));
select setval('enroll_eid_seq', (select max(eid) from enroll));
select setval('note_nid_seq', (select max(nid) from note));
select setval('visit_action_aid_seq', (select max(aid) from visit_action));

