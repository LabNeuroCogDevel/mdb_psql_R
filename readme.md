convert MS Access .mdb to pgsql. With schema modifications:
   * `contact` and `visit_task` represent data in long format instead of wide
   * JSON columns are used to increase flexability
   * people are collapsed into one table w/enrollment stored in a join table
   * notes exist in their own table (w/ join table for note <-> visit)
   * drop info exists in own table (join table for note <-> drop)

clojure version: bitbucket.org/LabNeuroCogDevel/postgresql-export
   * copied `sql/` from there
