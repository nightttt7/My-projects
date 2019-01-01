DROP TABLE IF EXISTS risk_analysis.tmp_sqq_model3_rule;
CREATE TABLE risk_analysis.tmp_sqq_model3_rule
COMMENT 'tmp_sqq_model3_rule'
STORED AS PARQUET
AS
WITH
main AS (
SELECT
appl_no
FROM
risk_analysis.tmp_sqq_model3_main
),
rule AS (
SELECT
appl_no,
appl_tm,
round_par,
workflow_cd,
rule_content,
rule_id
FROM
dsst.gdl_aprvadt_trig_rule
WHERE
dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
AND
prod_cd IN ('3112','3113','3121','3123','3131','3132','3329','3333','3334','3335','3336','3339','3340','3385','3386','3387','3594','3595','3596','3119','3120','3325','3326','3328','3338','3383','3384','3124','3203','3204','3207','3208','3220','3221')
AND
(
to_date(appl_tm) 
BETWEEN '2017-04-16'
AND '2018-04-15'
)
)
SELECT
main.appl_no,
rule.round_par,
rule.workflow_cd,
rule.rule_content,
rule.rule_id
FROM
rule
LEFT JOIN
main
ON
main.appl_no = rule.appl_no
;
INVALIDATE METADATA risk_analysis.tmp_sqq_model3_rule;
-- 测试通过