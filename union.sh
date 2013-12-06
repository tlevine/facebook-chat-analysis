#!/bin/sh
# Union all of the sqlite databases.
set -e

db=/tmp/logs.db
rm -f "${db}"

union() {
  sqlite3 "${db}" "
  ATTACH DATABASE '$1' AS new;
  INSERT INTO main.log_msg SELECT * FROM new.log_msg;
  INSERT INTO main.log_status SELECT * FROM new.log_status;
  DETACH DATABASE new;
  "
}

# Apply the schema
sqlite3 logs/2012-09-15.db .schema | sqlite3 "${db}"

# Add the new data.
for new in $(ls logs/2013-11-*.db); do
  union "${new}" || echo Error in "${new}"
done
