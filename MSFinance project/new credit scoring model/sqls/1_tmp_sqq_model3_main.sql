DROP TABLE IF EXISTS risk_analysis.tmp_sqq_model3_main;
CREATE TABLE risk_analysis.tmp_sqq_model3_main 
COMMENT 'tmp_sqq_model3_main'
STORED AS PARQUET
AS
SELECT
appl_no,
prod_cd,
contra_no,
mer_no,
appl_tm,
appl_lim,
appl_loan_term,
appl_status,
biz_city_cd
FROM
dsst.gdl_aprvadt_det
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
;
INVALIDATE METADATA risk_analysis.tmp_sqq_model3_main;
-- Test pass
