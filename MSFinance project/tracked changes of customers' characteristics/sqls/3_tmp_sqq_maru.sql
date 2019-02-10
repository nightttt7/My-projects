-- Test passed
-- Delete existing tmp_sqq_maru table
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_maru;
-- Create a new tmp_sqq_maru table
CREATE TABLE risk_analysis.tmp_sqq_maru
COMMENT 'main&rule'
STORED AS PARQUET
AS
-- Create an internal table
WITH
Main AS (
SELECT
Appl_no
FROM
Risk_analysis.tmp_sqq_main
),
Rule AS (
SELECT
Appl_no,
GROUP_CONCAT(rule_id) AS rule_id_cat
FROM
Dsst.gdl_aprvadt_trig_rule
WHERE
Dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')
AND
(
Rule_id like ('HC%')
OR
Rule_id like ('SLBE%')
OR
Rule_id like ('SLED%')
OR
Rule_id like ('Flag%')
OR
Rule_id like ('SLFD%')
OR
Rule_id like ('FDIN%')
)
GROUP BY
Appl_no
),
-- Note that this is a increment table
Oper AS (
SELECT
Oper_uniq.app_no,
SUM(oper_uniq.oper_rslt) AS ops2ce
FROM
(
SELECT
Oper_rec.app_no,
Oper_rec.oper_rslt,
Row_number() OVER ( PARTITION BY app_no ORDER BY comp_tm DESC) AS rn
FROM
Dsst.fdl_aprvadt_luma_appl_task_oper_rec AS oper_rec
WHERE
Oper_rslt=9
AND
Comp_tm IS NOT NULL
) AS oper_uniq
WHERE
Oper_uniq.rn=1
GROUP BY
Oper_uniq.app_no
)
-- Query block
SELECT
Main.appl_no,
Rule.rule_id_cat,
Oper.ops2ce
FROM
Main
LEFT JOIN
Rule
ON
Main.appl_no = rule.appl_no
LEFT JOIN
Oper
ON
Main.appl_no = oper.app_no
;
-- SQL ends
-- Refresh
INVALIDATE METADATA risk_analysis.tmp_sqq_maru;
