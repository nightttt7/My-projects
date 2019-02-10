-- Test passed
-- Delete existing tmp_sqq_mabl table
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_mabl;
-- Create a new tmp_sqq_mabl table
CREATE TABLE risk_analysis.tmp_sqq_mabl
COMMENT 'main&blaze'
STORED AS PARQUET
AS
-- Create an internal table
WITH
Blaze AS (
SELECT
Appl_no,
tempVector6_r,
Pos_jxl_model3_jxlposscorev3
-- PBOC and other information to be added
FROM
Fdl.fdl_aprvadt_blaze_application
WHERE
Dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')
AND
Round_par = 'blaze_ivk_Round3'
)
-- Query block
SELECT
Main.*,
Blaze.tempVector6_r,
Blaze.pos_jxl_model3_jxlposscorev3
FROM
Risk_analysis.tmp_sqq_main AS main
LEFT JOIN
Blaze
ON
Blaze.appl_no = main.appl_no
;
-- SQL ends
-- Refresh
INVALIDATE METADATA risk_analysis.tmp_sqq_mabl;
