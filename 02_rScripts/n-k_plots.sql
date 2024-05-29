-- !preview conn=DBI::dbConnect(RSQLite::SQLite(), '00_rawData/N-K_Table_1.db')

SELECT
    SUBSTR("FIA plot code", 1, 2) AS STATECD,
    SUBSTR("FIA plot code", 3, 4) AS INVYR,
    SUBSTR("FIA plot code", 7, 2) AS UNITCD,
    SUBSTR("FIA plot code", 9, 3) AS COUNTYCD,
    SUBSTR("FIA plot code", 12, 5) AS PLOT,
  * 
FROM PLOTS;
