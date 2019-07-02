#!/usr/bin/env bash
cd $(dirname $0)
trap 'e=$?; [ $e -ne 0 ] && echo "$0 exited in error"' EXIT
set -euo pipefail

#
# 20190620WF - init
#    push lncdb_r created by mdb_psql.R to "production" server
#    should run make in ../pull_from_sheets afterwards
# 20190628WF - remove defaults
#           push_to_prod.bash localhost lncddb # for local
#           push_to_prod.bash arnold lncddb    # for final


[ $# -ne 2 ] && echo "give me a host and database: $0 arnold lncddb" && exit 1
remote_host="$1"; shift
remote_db="$1"

set -x
echo "SELECT pg_terminate_backend(pg_stat_activity.pid)
FROM pg_stat_activity
WHERE pg_stat_activity.datname = '$remote_db'
  AND pid <> pg_backend_pid();
drop database if exists $remote_db;create database $remote_db;" | psql -U postgres -h $remote_host
pg_dump -U postgres lncddb_r | psql -U postgres -h $remote_host $remote_db 
cat sql/06_update_seq.sql sql/04_add-RAs.sql sql/07_update_roles.sql | psql -U postgres -h $remote_host $remote_db
