create role foranw with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO foranw;
grant all privileges on all tables in schema public to foranw;
grant all privileges on all functions in schema public to foranw;
grant all privileges on all sequences in schema public to foranw;


create role fedorjm2 with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO fedorjm2;
grant all privileges on all tables in schema public to fedorjm2;
grant all privileges on all functions in schema public to fedorjm2;
grant all privileges on all sequences in schema public to fedorjm2;


create role thompsonl12 with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO thompsonl12;
grant all privileges on all tables in schema public to thompsonl12;
grant all privileges on all functions in schema public to thompsonl12;
grant all privileges on all sequences in schema public to thompsonl12;

create role missarm with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO missarm;
grant all privileges on all tables in schema public to missarm;
grant all privileges on all functions in schema public to missarm;
grant all privileges on all sequences in schema public to missarm;

create role rockcasn with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO rockcasn;
grant all privileges on all tables in schema public to rockcasn;
grant all privileges on all functions in schema public to rockcasn;
grant all privileges on all sequences in schema public to rockcasn;


insert into ra (ra, abbr) values ('foranw', 'WF');
insert into ra (ra, abbr) values ('fedorjm2', 'JF');
insert into ra (ra, abbr) values ('thompsonl12', 'LT');
insert into ra (ra, abbr) values ('missarm', 'MM');
insert into ra (ra, abbr) values ('rockcasn', 'NR');
