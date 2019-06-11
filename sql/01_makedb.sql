-- schema for postgresql database
-- use like:
--    sudo -u postgres psql < makedb.sql

-- kill anyone still connected
SELECT pg_terminate_backend(pg_stat_activity.pid)
FROM pg_stat_activity
WHERE pg_stat_activity.datname = 'lncddb'
  AND pid <> pg_backend_pid();

-- kill db to reserect it
drop database if exists lncddb;
create database lncddb;
\c lncddb;
