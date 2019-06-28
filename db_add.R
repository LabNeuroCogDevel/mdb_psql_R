#!/usr/bin/env Rscript

# 20190628WF - init
#   separate mdb read from psql write.


## add to databae
# run `make schema` before running this
library(jsonlite)
library(dplyr)
library(DBI)
load("dbdfs.Rdata")
# readRenviron(".Renviron") # db settings stored there (and in ~/.pgpass)
con <- dbConnect(RPostgreSQL::PostgreSQL(),
                 dbname="lncddb_r",
                 user=Sys.getenv("pg_user"))
for (tblname in names(dbdfs)) {
   dbWriteTable(con, tblname, dbdfs[[tblname]], row.names=F, append=T)
}
