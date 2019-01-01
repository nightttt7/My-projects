-- 测试通过
-- 删除已有tmp_sqq_macu表
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_macu;
-- 新建tmp_sqq_macu表
CREATE TABLE risk_analysis.tmp_sqq_macu
COMMENT 'wholemain&customchain'
STORED AS PARQUET
AS
-- 创建内部表
WITH
main AS (
SELECT
appl_no
FROM
risk_analysis.tmp_sqq_main
),
cust AS (
SELECT
app_no,
gender_cd,
mo_earn,
oth_earn,
oth_loan,
currt_city,
edu_degree,
soc_id,
work_life,
unit_name,
unit_addr_city,
marg_status,
house_cond
FROM
dsst.fdl_cust_prim_appl_chain
WHERE
CHAIN_STATUS='active'
AND
end_date = '47121231'
)
-- 查询语句块
SELECT
main.appl_no,
cust.gender_cd,
cust.mo_earn,
cust.oth_earn,
cust.oth_loan,
cust.currt_city,
cust.edu_degree,
cust.soc_id,
cust.work_life,
cust.unit_name,
cust.unit_addr_city,
cust.marg_status,
cust.house_cond
FROM
main
LEFT JOIN
cust
ON
main.appl_no = cust.app_no
;
-- SQL结束
-- 刷新
INVALIDATE METADATA risk_analysis.tmp_sqq_macu;