-- Test passed
-- Delete existing tmp_sqq_main table
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_main;
-- Create a new tmp_sqq_main table
CREATE TABLE risk_analysis.tmp_sqq_main
COMMENT 'mainselectappltm&dt'
STORED AS PARQUET
AS
-- Query block
SELECT
Appl_no,
Prod_cd,
Mer_no,
Union_id,
Contra_no,
Appl_tm,
Appl_lim,
Appl_loan_term,
Down_pay_amt,
Down_pay_pct,
Appl_status,
Basic_checker,
Final_checker,
Biz_city_cd
FROM
Dsst.gdl_aprvadt_det
WHERE
Dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400, 'yyyyMMdd')
AND
Prod_cd IN ('3209', '3210', '3388', '3389', '3390', '3391', '3392', '3393', '3394', '3395', '3396', '3397' , '3398',
'3399', '3507', '3508', '3509', '3530', '3531', '3532', '3112', '3113', '3121', '3123', '3131', '3132 ', '3329',
'3333', '3334', '3335', ​​'3336', '3339', '3340', '3385', '3386', '3387', '3594', '3595', '3596', '3119 ', '3120',
'3325', '3326', '3328', '3338', '3383', '3384', '3124', '3203', '3204', '3207', '3208', '3220', '3221 ', '3551',
'3403', '3404', '3405')
AND
(
To_date(appl_tm)
BETWEEN '2017-04-16'
AND '2018-04-15'
)
;
-- SQL ends
-- Refresh
INVALIDATE METADATA risk_analysis.tmp_sqq_main;
