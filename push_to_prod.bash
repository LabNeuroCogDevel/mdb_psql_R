#!/usr/bin/env bash
set -euo pipefail
trap 'e=$?; [ $e -ne 0 ] && echo "$0 exited in error"' EXIT

#
# 20190620WF - init
#    push lncdb_r created by mdb_psql.R to "production" server
#    should run make in ../pull_from_sheets afterwards
#


[ $# -eq 0 ] && remote_db=lncddb_dev || remote_db=$1
remote_host=arnold # defined in ~/.pg_pass and /etc/hosts

set -x
echo "SELECT pg_terminate_backend(pg_stat_activity.pid)
FROM pg_stat_activity
WHERE pg_stat_activity.datname = '$remote_db'
  AND pid <> pg_backend_pid();
drop database if exists $remote_db;create database $remote_db;" | psql -U postgres -h $remote_host
pg_dump -U postgres lncddb_r | psql -U postgres -h $remote_host $remote_db 
