-- 测试通过
-- 删除已有bak_sqq_custom_qual表
DROP TABLE IF EXISTS risk_analysis.bak_sqq_custom_qual;
-- 刷新bak_sqq_modelscore表
INVALIDATE METADATA risk_analysis.bak_sqq_modelscore;
-- 新建已有bak_sqq_custom_qual表
CREATE TABLE risk_analysis.bak_sqq_custom_qual 
COMMENT '服务贷客群资质数据'
STORED AS PARQUET
AS
-- 查询语句块
SELECT
mabl.*,
maru.rule_id_cat,
maru.ops2ce,
macu.gender_cd,
macu.mo_earn,
macu.oth_earn,
macu.oth_loan,
macu.currt_city,
macu.edu_degree,
macu.soc_id,
macu.work_life,
macu.unit_name,
macu.unit_addr_city,
macu.marg_status,
macu.house_cond,
maoth.mer_name,
maoth.mer_status,
maoth.max_dpd,
maoth.curt_dpd,
maoth.contra_status,
maoth.curt_term,
maoth.fpd,
maoth.spd,
maoth.tpd,
maoth.qpd,
maoth.fvpd,
maoth.sxpd,
maoth.svpd,
maoth.etpd,
maoth.nnpd,
maoth.tnpd,
maoth.evpd,
maoth.twpd,
maoth.xprod_cd,
maoth.effc_start_dt,
maoth.effc_end_dt,
ms.age,
ms.Zhima_m,
ms.bt3_m,
ms.pbjxl_m,
ms.cashl_m,
ms.posjxl20_m,
ms.posjxl21_m,
ms.ins_m,
ms.jxl_m,
ms.cup_m
FROM
risk_analysis.tmp_sqq_mabl AS mabl
JOIN
risk_analysis.tmp_sqq_maru AS maru
ON
mabl.appl_no = maru.appl_no
JOIN
risk_analysis.tmp_sqq_macu AS macu
ON
mabl.appl_no = macu.appl_no
JOIN
risk_analysis.tmp_sqq_maoth AS maoth
ON
mabl.appl_no = maoth.appl_no
LEFT JOIN
risk_analysis.bak_sqq_modelscore AS ms
ON
mabl.appl_no = ms.appl_no
;
-- SQL结束
-- 刷新
INVALIDATE METADATA risk_analysis.bak_sqq_custom_qual;