-- 测试通过
-- 删除已有tmp_sqq_mabl表
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_mabl;
-- 新建tmp_sqq_mabl表
CREATE TABLE risk_analysis.tmp_sqq_mabl
COMMENT 'main&blaze'
STORED AS PARQUET
AS
-- 创建内部表
WITH
blaze AS (
SELECT
appl_no,
tempVector6_r,
pos_jxl_model3_jxlposscorev3
-- 待添加PBOC等信息
FROM
fdl.fdl_aprvadt_blaze_application
WHERE
dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
AND
round_par = 'blaze_ivk_Round3'
)
-- 查询语句块
SELECT
main.*,
blaze.tempVector6_r,
blaze.pos_jxl_model3_jxlposscorev3
FROM
risk_analysis.tmp_sqq_main AS main
LEFT JOIN
blaze
ON
blaze.appl_no = main.appl_no
;
-- SQL结束
-- 刷新
INVALIDATE METADATA risk_analysis.tmp_sqq_mabl;