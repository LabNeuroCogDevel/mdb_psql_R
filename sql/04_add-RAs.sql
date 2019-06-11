create role foranw with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO foranw;

create role fedorjm2 with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO fedorjm2;

create role thompsonl12 with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO thompsonl12;

create role missarm with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO missarm;

create role rockcasn with LOGIN REPLICATION password NULL;
GRANT ALL PRIVILEGES ON DATABASE lncddb TO rockcasn;


insert into ra (ra) values ('foranw');
insert into ra (ra) values ('fedorjm2');
insert into ra (ra) values ('thompsonl12');
insert into ra (ra) values ('missarm');
insert into ra (ra) values ('rockcasn');
