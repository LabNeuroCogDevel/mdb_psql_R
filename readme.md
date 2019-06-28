# Flow

* `make` to create local database (`lncddb_r`)
* `cd ../pull_from_sheets/Makefile; make` to add visits from newer studies.
* to re-apply changes from `pull_from_sheets`, run `./push_to_prod.bash localhost lncddb`
* `make push-arnold`: push changes to arnold with

# What
convert MS Access .mdb to pgsql. With schema modifications:
   * `contact` and `visit_task` represent data in long format instead of wide
   * JSON columns are used to increase flexability
     * `visit_task` JSON `measures` column takes the place of many tables in old schema
   * people are collapsed into one table w/enrollment stored in a join table (`tHotline` is merged with `tSubjectInfo`)
   * notes exist in their own table (w/ join table for note <-> visit)
   * drop info exists in own table (join table for note <-> drop)

# See also
clojure version: bitbucket.org/LabNeuroCogDevel/postgresql-export
   * copied `sql/` from there
