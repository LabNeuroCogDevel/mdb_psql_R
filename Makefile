.PHONY: all schema data triggers

all: triggers

schema:
	cat sql/01_makedb.sql sql/02_mkroles.sql sql/03_mkschema.sql | sudo -u postgres psql

data: schema mdb_psql.R
	Rscript mdb_psql.R

triggers: data
	cat sql/04_add-RAs.sql sql/05_triggers.sql | sudo -u postgres psql
