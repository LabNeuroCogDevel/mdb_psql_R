-- ----------
-- ROLES for postgrest
-- https://compose.io/articles/your-sql-schema-is-your-json-api-with-postgrest/
-- ----------
CREATE ROLE lncd;
grant SELECT, INSERT, UPDATE,  DELETE on ALL tables in schema public to lncd;
-- allow lncd to play use triggers (seq incrementers)
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO lncd;

CREATE ROLE anonymous;
-- GRANT SELECT, INSERT ON TABLE users TO anon;  
-- GRANT EXECUTE ON FUNCTION  
--   login(text,text),
--   TO anonymous;
