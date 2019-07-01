.PHONY: all schema data triggers push

all: push

schema:
	cat sql/01_makedb.sql sql/02_mkroles.sql sql/03_mkschema.sql | sudo -u postgres psql

dbdfs.Rdata: mdb_psql.R 
	Rscript mdb_psql.R

data: dbdfs.Rdata db_add.R schema LunaDB.mdb
	Rscript db_add.R

triggers: data
	cat  sql/05_triggers.sql sql/06_update_seq.sql sql/04_add-RAs.sql | sudo -u postgres psql lncddb_r

push: triggers
	./push_to_prod.bash localhost lncddb

push-arnold: triggers
	./push_to_prod.bash arnold.wpic.upmc.edu lncddb

	
