#!/bin/sh

tmpfile=`basename -s .sh $0`.tmp
db=data/raw/SQLite_FIADB_ENTIRE.db

echo "Scanning for primary keys" >&2
cat << ___EOF___ | sqlite3 --list $db > $tmpfile
SELECT t.name AS table_name, c.name AS column_name
FROM sqlite_schema t
CROSS JOIN pragma_table_info((t.name)) c
WHERE column_name = 'CN'
ORDER BY table_name, column_name;
___EOF___

echo "Creating indexes on primary keys" >&2
(
  for fqcn in `cat $tmpfile` ; do
    tbl=`echo $fqcn | sed -E -e 's/\|.*//'`
    col=`echo $fqcn | sed -E -e 's/.*\|//'`
    sql="CREATE UNIQUE INDEX IF NOT EXISTS IDX_${tbl}_${col} ON ${tbl} (${col});"
    echo "    ${sql}" >&2
    echo "${sql}"
  done
) | sqlite3 $db

echo "Scanning for foreign keys"
cat << ___EOF___ | sqlite3 --list $db > $tmpfile
SELECT t.name AS table_name, c.name AS column_name
FROM sqlite_schema t
CROSS JOIN pragma_table_info((t.name)) c
WHERE column_name LIKE '%_CN'
ORDER BY table_name, column_name;
___EOF___

echo "Creating indexes on foreign keys" >&2
(
  for fqcn in `cat $tmpfile` ; do
    tbl=`echo $fqcn | sed -E -e 's/\|.*//'`
    col=`echo $fqcn | sed -E -e 's/.*\|//'`
    sql="CREATE INDEX IF NOT EXISTS IDX_${tbl}_${col} ON ${tbl} (${col});"
    echo "    ${sql}" >&2
    echo "${sql}"
  done
) | sqlite3 $db

echo "Scanning for tables with plot columns"
cat << ___EOF___ | sqlite3 --list $db > $tmpfile
SELECT
  table_name,
  SUM(has_statecd) & SUM(has_countycd) & SUM(has_plot) AS has_location
FROM (
  SELECT t.name AS table_name,
  IIF(c.name = 'STATECD', 1, 0) AS has_statecd,
  IIF(c.name = 'COUNTYCD', 1, 0) AS has_countycd,
  IIF(c.name = 'PLOT', 1, 0) AS has_plot
  FROM sqlite_schema t
  CROSS JOIN pragma_table_info((t.name)) c
)
GROUP BY table_name
HAVING has_location
ORDER BY table_name;
___EOF___

echo "Creating indexes on plot" >&2
(
  for fqcn in `cat $tmpfile` ; do
    tbl=`echo $fqcn | sed -E -e 's/\|.*//'`
    col="STATECD,COUNTYCD,PLOT"
    sql="CREATE INDEX IF NOT EXISTS IDX_${tbl}_PLOT ON ${tbl} (${col});"
    echo "    ${sql}" >&2
    echo "${sql}"
  done
) | sqlite3 $db

echo "Done" >&2

rm $tmpfile
