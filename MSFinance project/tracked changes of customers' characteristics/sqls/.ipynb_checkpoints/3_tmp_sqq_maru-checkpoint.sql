-- 测试通过
-- 删除已有tmp_sqq_maru表
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_maru;
-- 新建tmp_sqq_maru表
CREATE TABLE risk_analysis.tmp_sqq_maru
COMMENT 'main&rule'
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
rule AS (
SELECT
appl_no,
GROUP_CONCAT(rule_id) AS rule_id_cat
FROM
dsst.gdl_aprvadt_trig_rule
WHERE
dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
AND
(
rule_id like ('HC%')
OR
rule_id like ('SLBE%')
OR
rule_id like ('SLED%')
OR
rule_id like ('Flag%')
OR
rule_id like ('SLFD%')
OR
rule_id like ('FDIN%')
)
GROUP BY 
appl_no
),
-- 注意,这是张增量表
oper AS (
SELECT
oper_uniq.app_no,
SUM(oper_uniq.oper_rslt) AS ops2ce
FROM
(
SELECT
oper_rec.app_no,
oper_rec.oper_rslt,
row_number() OVER ( PARTITION BY app_no ORDER BY comp_tm DESC) AS rn
FROM 
dsst.fdl_aprvadt_luma_appl_task_oper_rec AS oper_rec
WHERE
oper_rslt=9 
AND 
comp_tm IS NOT NULL
) AS oper_uniq
WHERE 
oper_uniq.rn=1
GROUP BY
oper_uniq.app_no
)
-- 查询语句块
SELECT
main.appl_no,
rule.rule_id_cat,
oper.ops2ce
FROM
main
LEFT JOIN
rule
ON
main.appl_no = rule.appl_no
LEFT JOIN
oper
ON
main.appl_no = oper.app_no
;
-- SQL结束
-- 刷新
INVALIDATE METADATA risk_analysis.tmp_sqq_maru;