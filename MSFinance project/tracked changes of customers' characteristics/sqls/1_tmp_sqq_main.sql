-- 测试通过
-- 删除已有tmp_sqq_main表
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_main;
-- 新建tmp_sqq_main表
CREATE TABLE risk_analysis.tmp_sqq_main 
COMMENT 'mainselectappltm&dt'
STORED AS PARQUET
AS
-- 查询语句块
SELECT
appl_no,
prod_cd,
mer_no,
union_id,
contra_no,
appl_tm,
appl_lim,
appl_loan_term,
down_pay_amt,
down_pay_pct,
appl_status,
basic_checker,
final_checker,
biz_city_cd
FROM
dsst.gdl_aprvadt_det
WHERE
dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
AND
prod_cd IN ('3209','3210','3388','3389','3390','3391','3392','3393','3394','3395','3396','3397','3398',
	'3399','3507','3508','3509','3530','3531','3532','3112','3113','3121','3123','3131','3132','3329',
	'3333','3334','3335','3336','3339','3340','3385','3386','3387','3594','3595','3596','3119','3120',
	'3325','3326','3328','3338','3383','3384','3124','3203','3204','3207','3208','3220','3221','3551',
	'3403','3404','3405')
AND
(
to_date(appl_tm) 
BETWEEN '2017-04-16'
AND '2018-04-15'
)
;
-- SQL结束
-- 刷新
INVALIDATE METADATA risk_analysis.tmp_sqq_main;