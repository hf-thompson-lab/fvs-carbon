# fvs-landis-compare
Comparison of FVS and LANDIS-II models for carbon storage projection

# Notes

# 6/5/2024

See NKRevisited.Rmd for updated info on Stand_ID and updating Stand_ID to
correspond to 2023 FIA data.

# 6/5/2023

fvs-fia Stand_ID:  concatenation of:  PLOT.STATECD (4) + PLOT.INVYR (4) + PLOT.CYCLE (2) + PLOT.SUBCYCLE (2) + PLOT.UNITCD (2) + PLOT.COUNTYCD (3) + PLOT.PLOT (5).

excel files created in R use STATECD + INVYR + UNITCD + COUNTYCD + PLOT to indicated STAND_ID.
