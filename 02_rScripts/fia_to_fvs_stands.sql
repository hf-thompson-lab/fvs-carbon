ATTACH DATABASE '00_rawData/SQLite_FIADB_ENTIRE.db' AS FIA;
ATTACH DATABASE '00_rawData/N-K_Table_1.db' AS NK;

-- StandInit_cond.Stand_ID = concatenation of: 
-- STATECD(4) + 
-- INVYR(4) + CYCLE(2) + SUBCYCLE(2) + 
-- UNITCD(2) + COUNTYCD(3) + PLOT(5) + CONDID (1)

-- In NK, stand_ids appear to be
-- STATECD(2)
-- YEAR(4)
-- UNITCD(2)
-- COUNTYCD(3)
-- PLOT(5)

WITH NK_EXPANDED AS (
  SELECT
    SUBSTR("FIA plot code", 1, 2) AS STATECD,
    SUBSTR("FIA plot code", 3, 4) AS INVYR,
    SUBSTR("FIA plot code", 7, 2) AS UNITCD,
    SUBSTR("FIA plot code", 9, 3) AS COUNTYCD,
    SUBSTR("FIA plot code", 12, 5) AS PLOT,
    *
--  FROM PLOTS
  FROM NK.PLOTS
),
FIA_COND AS (
  SELECT
    STATECD,
    INVYR,
    UNITCD,
    COUNTYCD,
    PLOT,
    STDAGE,
    FLDAGE
  FROM FIA.COND
)
-- SELECT * FROM NK_EXPANDED;
SELECT
  NK_EXPANDED.*,
  FIA_COND.*
FROM NK_EXPANDED
JOIN FIA_COND ON
  NK_EXPANDED.UNITCD = FIA_COND.UNITCD AND
  NK_EXPANDED.STATECD = FIA_COND.STATECD AND
  NK_EXPANDED.COUNTYCD = FIA_COND.COUNTYCD AND
  NK_EXPANDED.PLOT = FIA_COND.PLOT AND
  NK_EXPANDED."Starting stand age" = FIA_COND.STDAGE;

