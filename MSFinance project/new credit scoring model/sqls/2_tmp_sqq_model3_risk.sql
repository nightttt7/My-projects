DROP TABLE IF EXISTS risk_analysis.tmp_sqq_model3_risk;
CREATE TABLE risk_analysis.tmp_sqq_model3_risk
COMMENT 'tmp_sqq_model3_risk'
STORED AS PARQUET
AS
WITH
main AS (
SELECT
appl_no,
contra_no
FROM
risk_analysis.tmp_sqq_model3_main
),
triangle AS (
SELECT
contra_no,
max_dpd,
fpd
FROM
dsst.gdl_risk_triangle
WHERE
dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
)
SELECT
main.appl_no,
triangle.max_dpd,
triangle.fpd
FROM
main
LEFT JOIN
triangle
ON
main.contra_no = triangle.contra_no
;
INVALIDATE METADATA risk_analysis.tmp_sqq_model3_risk;
-- 测试完成