#!/bin/sh

CSVS=data/raw/FIADB_REFERENCE
if [ ! -d $CSVS ] ; then
  echo "Input not found: $CSVS; aborting." >&2
  exit 1
fi

STAGING=data/raw/SQLite_FIADB_REFERENCE.stage.db

if [ -f $STAGING ] ; then
  echo "Staging database $STAGING already exists; aborting." >&2
  exit 1
fi

(
  for csv in $CSVS/*.csv ; do
    tbl=`basename $csv | sed -E -e 's/.csv//'`
    echo "Loading FIADB_REFERENCE.$tbl from $csv" >&2
    echo ".import $csv $tbl"
  done
) | sqlite3 --csv $STAGING

echo "Done." >&2
