MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

.PHONY: all push

all: push
push: last_visit_update.txt

dbdfs.Rdata: mdb_psql.R  LunaDB.mdb
	Rscript mdb_psql.R

last_visit_update.txt:  dbdfs.Rdata
	cat sql/01_makedb.sql sql/02_mkroles.sql sql/03_mkschema.sql | sudo -u postgres psql
	Rscript db_add.R
	cat sql/05_triggers.sql sql/06_update_seq.sql sql/04_add-RAs.sql | sudo -u postgres psql lncddb_r
	./push_to_prod.bash localhost lncddb
	psql lncddb lncd -c "SELECT max(pg_xact_commit_timestamp(xmin)) FROM visit;" > last_visit_update.txt

push-arnold: last_visit_update.txt
	./push_to_prod.bash arnold.wpic.upmc.edu lncddb

	
