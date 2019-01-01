-- 未测试
-- 删除已有tmp_sqq_maoth表
DROP TABLE IF EXISTS risk_analysis.tmp_sqq_maoth;
-- 新建tmp_sqq_maoth表
CREATE TABLE risk_analysis.tmp_sqq_maoth
COMMENT 'main&others(gdl_risk_triangle,gdl_mer_struc,gdl_cust_info)'
STORED AS PARQUET
AS
-- 创建内部表
WITH
main AS (
SELECT
appl_no,
mer_no,
union_id,
contra_no
FROM
risk_analysis.tmp_sqq_main
),
mer AS (
SELECT
mer_no,
mer_name,
mer_status
FROM
dsst.gdl_mer_struc
WHERE
dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
),
triangle AS (
SELECT
contra_no,
max_dpd,
curt_dpd,
contra_status,
curt_term,
fpd,
spd,
tpd,
qpd,
fvpd,
sxpd,
svpd,
etpd,
nnpd,
tnpd,
evpd,
twpd
FROM
dsst.gdl_risk_triangle
WHERE
dt = '20180609'
-- dt = FROM_UNIXTIME(UNIX_TIMESTAMP()-86400,'yyyyMMdd')
),
xsell AS (
SELECT
union_id,
prod_cd,
effc_start_dt,
effc_end_dt
FROM
dsst.fdl_cam_uds_x_sell_chain
WHERE
CHAIN_STATUS='active'
AND
end_date = '47121231'
)
-- 查询语句块
SELECT
main.appl_no,
main.mer_no,
main.union_id,
main.contra_no,
mer.mer_name,
mer.mer_status,
triangle.max_dpd,
triangle.curt_dpd,
triangle.contra_status,
triangle.curt_term,
triangle.fpd,
triangle.spd,
triangle.tpd,
triangle.qpd,
triangle.fvpd,
triangle.sxpd,
triangle.svpd,
triangle.etpd,
triangle.nnpd,
triangle.tnpd,
triangle.evpd,
triangle.twpd,
xsell.prod_cd AS 'xprod_cd',
xsell.effc_start_dt,
xsell.effc_end_dt
FROM
main
LEFT JOIN
mer
ON
main.mer_no = mer.mer_no
LEFT JOIN
triangle
ON
main.contra_no = triangle.contra_no
LEFT JOIN
xsell
ON
main.union_id = xsell.union_id
;
-- SQL结束
-- 刷新
INVALIDATE METADATA risk_analysis.tmp_sqq_maoth;