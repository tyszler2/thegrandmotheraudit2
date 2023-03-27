WITH attr
     AS (SELECT --+ INLINE
        hjs.shrtckn_pidm,
                hjs.shrtckn_term_code,
                hjs.shrtckn_seq_no,
                CASE
                  WHEN shrattr_attr_code IN ( 'Z-SC', 'Z-BM', 'Z-IB', 'Z-MY',
                                              'Z-JS'
                                            )
                   THEN 'JS'
                END AS HIDEJS
         FROM   shrtckn hjs,
                shrattr
         WHERE  hjs.shrtckn_pidm = shrattr.shrattr_pidm(+)
                AND hjs.shrtckn_term_code = shrattr.shrattr_term_code(+)
                AND hjs.shrtckn_seq_no = shrattr.shrattr_tckn_seq_no(+)
                AND shrattr.shrattr_attr_code NOT IN
                    (SELECT bb.shrattr_attr_code
                     FROM   shrattr bb
                     WHERE
                    bb.shrattr_attr_code NOT LIKE 'Z-%'
                                                     ))
SELECT   spriden_id,
                a.shrtckn_subj_code,
                a.shrtckn_crse_numb,
                shrdgmr_program,
                shrdgmr.shrdgmr_seq_no,
                shrdgmr.shrdgmr_levl_code,
                shrtckd.shrtckd_activity_date,
                shrtckd.shrtckd_user_id,
                 CASE
                    WHEN (shrtckd_dgmr_seq_no < shrdgmr_seq_no)
                        THEN 'ADD'
                        ELSE 'UPDATE'
                    END as RECORDSTATUS/*, 
                a.*,
                shrtckd.*,
                shrdgmr.*,
                spriden.*,
                attr.*,
                scbcrse_add.**/
FROM   shrtckd,
       shrtckn a,
       shrdgmr,
       spriden,
       sobcurr,
       attr,
       scbcrse_add
WHERE  shrtckd_tckn_seq_no = a.shrtckn_seq_no
     --  AND shrtckd_pidm = '963885' --luiza test
     -- AND shrtckd_pidm = '652120' --luiza test 2
    --  AND shrtckd_pidm = '501742' --eli fink
    --  AND shrtckd_pidm = '552280' --aryeh margolin
      -- AND shrtckd_pidm = '826296' daniel wainhaus
     --  AND shrtckd_pidm = '921178' --aryeh margolin
     --  AND shrtckd_pidm = '901239' --gozlin
    -- AND shrtckd_pidm = '882182' --jeremy basali
     --   AND shrtckd_pidm = '759031' --jeremy basali
   --   and shrtckd_pidm = '816546' --zachary wild
      -- and shrtckd_pidm = '401130' --shmuel lesher UG stopout
    -- and spriden_id = '800673381' --st-ba
      -- and spriden_id = '800530268' --dp2 - predrp_courses
     --    and spriden_id = '800343250' --dp2 - classic
   --  and spriden_id = '800461742' --dp2 - classic
    -- and spriden_id = '800395150' --dp2 - predrp_courses
   --   and spriden_id = '800530268' --dp2 - predrp_courses
    -- and spriden_id = '800702412' -- valentina test 88
   --  and spriden_id = '800205291' -- valentina test 90
   --    and spriden_id = '800293117' --dp2 -YC-BA-BS
    --     and spriden_id = '800556682' --SO out of order
    -- and spriden_id = '800618369' --add medetsky
    --  and spriden_id = '800342239' --add Meiri
   --     and spriden_id = '800440296' --GPATS
      and rownum <=200
    AND shrdgmr.shrdgmr_seq_no IN
                                 (SELECT --+ INLINE
                                    Max(dd.shrdgmr_seq_no)
                                      FROM   shrdgmr dd
                                      WHERE
           dd.shrdgmr_pidm = shrdgmr.shrdgmr_pidm
           AND
                ((
             -- dd.shrdgmr_program = shrdgmr.shrdgmr_program and
              dd.shrdgmr_levl_code in (select shrtckl_levl_code from shrtckl where a.shrtckn_term_code = shrtckl.shrtckl_term_code and a.shrtckn_seq_no = shrtckl.shrtckl_tckn_seq_no and a.shrtckn_pidm = shrtckl_pidm)
                and (dd.shrdgmr_levl_code = 'UG'
              and dd.shrdgmr_degc_code like 'B%'))
              OR
              (
               dd.shrdgmr_program = shrdgmr.shrdgmr_program and
              dd.shrdgmr_levl_code in (select shrtckl_levl_code from shrtckl where a.shrtckn_term_code = shrtckl.shrtckl_term_code and a.shrtckn_seq_no = shrtckl.shrtckl_tckn_seq_no and a.shrtckn_pidm = shrtckl_pidm)
                --and dd.shrdgmr_levl_code = 'UG'
              and not (dd.shrdgmr_levl_code = 'UG'
              and dd.shrdgmr_degc_code like 'B%')
              )
               ))
       AND shrtckd_dgmr_seq_no = shrdgmr_seq_no
       AND shrtckd_pidm = a.shrtckn_pidm
       AND shrtckd_pidm = shrdgmr_pidm
       AND shrtckd_applied_ind IS NULL
       AND a.shrtckn_term_code = shrtckd_term_code
       AND shrtckd_pidm = spriden_pidm
       AND spriden_change_ind IS NULL
       AND sobcurr_program = shrdgmr_program
       AND sobcurr.sobcurr_lock_ind = 'Y'
       AND shrdgmr_program IS NOT NULL
       AND shrdgmr.shrdgmr_degs_code != 'AW'
       AND (a.shrtckn_ptrm_code NOT LIKE 'IP%' or (a.shrtckn_ptrm_code like 'IP%' and shrtckn_subj_code = 'HES'))
      -- and a.shrtckn_subj_code not in (select DISTINCT(scbcrse_subj_code) from scbcrse where scbcrse_coll_code in ('RE') and scbcrse_eff_term >= '202106')
       AND shrdgmr.shrdgmr_program = scbcrse_add.valid_program_1
       AND a.shrtckn_subj_code = scbcrse_add.scbcrse_subj_code
       AND a.shrtckn_crse_numb = scbcrse_add.scbcrse_crse_numb
       and scbcrse_add.scbcrse_eff_term in (select max(b.scbcrse_eff_term) from scbcrse_add b where b.scbcrse_subj_code = scbcrse_add.scbcrse_subj_code and b.scbcrse_crse_numb = scbcrse_add.scbcrse_crse_numb)
       AND shrtckd.shrtckd_pidm IN (SELECT --+ INLINE
                                   sgbstdn_pidm AS popsel_pidm
                                    FROM   sgbstdn a
                                    WHERE  sgbstdn_term_code_eff =
           (SELECT Max (sgbstdn_term_code_eff)
            FROM   sgbstdn
            WHERE  sgbstdn_pidm = a.sgbstdn_pidm)
           AND EXISTS (SELECT 'x'
                       FROM   stvstst,
                              sgbstdn
                       WHERE  sgbstdn_term_code_eff =
               (SELECT Max (sgbstdn_term_code_eff)
                FROM   sgbstdn
                WHERE  sgbstdn_pidm = a.sgbstdn_pidm)
               AND sgbstdn_pidm = a.sgbstdn_pidm
               AND sgbstdn_stst_code = stvstst_code
               AND stvstst_reg_ind = 'Y')
           AND ( EXISTS (SELECT 'x'
                         FROM   stvrsts,
                                sfrstcr
                         WHERE  stvrsts_code =
                                sfrstcr_rsts_code
                                AND sfrstcr_pidm =
                                    sgbstdn_pidm
                                AND sfrstcr_term_code
                                    >=
                                    '201808')
                  OR sgbstdn_term_code_admit >=
                     '201909' )
           AND sgbstdn_pidm NOT IN
               (SELECT DISTINCT sprhold_pidm
                FROM   sprhold
                WHERE  sprhold_hldd_code LIKE 'Z_'
                and sprhold_to_date > sysdate))
       AND a.shrtckn_pidm = attr.shrtckn_pidm(+)
       AND a.shrtckn_term_code = attr.shrtckn_term_code(+)
       AND a.shrtckn_seq_no = attr.shrtckn_seq_no(+)
       AND attr.hidejs IS NULL
       AND shrdgmr.shrdgmr_program NOT IN ( 'ST-AA', 'IP', 'FG-MA-PSY','FG-MS-SPMS','RE-RB2-KOL','RE-RB-RAB-I')
         AND shrdgmr.shrdgmr_program IN
                              (SELECT --+ INLINE
                              sgvacur_program
                               FROM sgvacur
                               WHERE     sgvacur_pidm = a.shrtckn_pidm
                                     AND sgvacur_levl_code = shrdgmr_levl_code
                                     AND sgvacur_stdn_term_code_eff =
                                         (SELECT MAX (sgbstdn_term_code_eff)
                                          FROM sgbstdn
                                          WHERE     sgbstdn_pidm = a.shrtckn_pidm
                                                AND sgbstdn_term_code_eff <=
                                                    baninst1.F_YU_RETURN_MIN_TERM)
                                     AND sgvacur_order > 0
                                     AND NVL (sgvacur_term_code_end,
                                              '999999'
                                             ) >
                                         sgvacur_stdn_term_code_eff
                                     AND sgvacur_current_cde = 'Y')
       AND shrdgmr_levl_code NOT IN( 'UT', 'HS' )
UNION
SELECT   spriden_id,
                a.shrtckn_subj_code,
                a.shrtckn_crse_numb,
                shrdgmr_program,
                shrdgmr.shrdgmr_seq_no,
                shrdgmr.shrdgmr_levl_code,
                shrtckd.shrtckd_activity_date,
                shrtckd.shrtckd_user_id,
                CASE
                    WHEN (shrtckd_dgmr_seq_no < shrdgmr_seq_no)
                        THEN 'ADD'
                        ELSE 'UPDATE'
                    END as RECORDSTATUS/*,    
                shrtckd.*,
                a.*,
                shrtckd.*,
                shrdgmr.*,
                spriden.*,
                attr.*,
                scbcrse_add.**/
FROM   shrtckd,
       shrtckn a,
       shrdgmr,
       spriden,
       sobcurr,
       attr,
       scbcrse_add
WHERE  shrtckd_tckn_seq_no = a.shrtckn_seq_no
    --   AND shrtckd_pidm = '552289' --luiza test
     -- AND shrtckd_pidm = '652120' --luiza test 2
    --  AND shrtckd_pidm = '501742' --eli fink
    --  AND shrtckd_pidm = '552280' --aryeh margolin
      -- AND shrtckd_pidm = '826296' daniel wainhaus
     --  AND shrtckd_pidm = '921178' --aryeh margolin
     --  AND shrtckd_pidm = '901239' --gozlin
    -- AND shrtckd_pidm = '882182' --jeremy basali
     --   AND shrtckd_pidm = '759031' --jeremy basali
   --   and shrtckd_pidm = '816546' --zachary wild
      -- and shrtckd_pidm = '401130' --shmuel lesher UG stopout
   --  and spriden_id = '800343250' --dp2 - classic
   -- and spriden_id = '800461742' --dp2 - classic
    -- and spriden_id = '800395150' --dp2 - predrp_courses
    --  and spriden_id = '800530268' --dp2 - predrp_courses
    -- and spriden_id = '800702412' -- valentina test 88
   --  and spriden_id = '800205291' -- valentina test 90
    --   and spriden_id = '800293117' --dp2 -YC-BA-BS
   --    and spriden_id = '800556682' --SO out of order
   -- and spriden_id = '800673390' --add
     --    and spriden_id = '800673390' --add
     --    and spriden_id = '800618369' --add medetsky
  --    and spriden_id = '800342239' --add Meiri
    --    and spriden_id = '800440296' --GPATS
      and rownum <=200
       AND shrdgmr.shrdgmr_seq_no in (SELECT --+ INLINE
                                    Max(dd.shrdgmr_seq_no)
                                      FROM   shrdgmr dd
                                      WHERE
           dd.shrdgmr_pidm = shrdgmr.shrdgmr_pidm
           AND
                ((
             -- dd.shrdgmr_program = shrdgmr.shrdgmr_program and
              dd.shrdgmr_levl_code in (select shrtckl_levl_code from shrtckl where a.shrtckn_term_code = shrtckl.shrtckl_term_code and a.shrtckn_seq_no = shrtckl.shrtckl_tckn_seq_no and a.shrtckn_pidm = shrtckl_pidm)
                and (dd.shrdgmr_levl_code = 'UG'
              and dd.shrdgmr_degc_code like 'B%'))
              OR
              (
               dd.shrdgmr_program = shrdgmr.shrdgmr_program and
              dd.shrdgmr_levl_code in (select shrtckl_levl_code from shrtckl where a.shrtckn_term_code = shrtckl.shrtckl_term_code and a.shrtckn_seq_no = shrtckl.shrtckl_tckn_seq_no and a.shrtckn_pidm = shrtckl_pidm)
                --and dd.shrdgmr_levl_code = 'UG'
              and not (dd.shrdgmr_levl_code = 'UG'
              and dd.shrdgmr_degc_code like 'B%')
              )
               ))
     /*  AND shrtckd_dgmr_seq_no > (select max(tm.shrtckd_DGMR_SEQ_NO) from shrtckd tm where tm.shrtckd_pidm = shrtckd.shrtckd_pidm 
                                        --and tm.shrtckd_term_code = shrtckd.shrtckd_term_code
                                        --and tm.shrtckd_tckn_seq_no = shrtckd.shrtckd_tckn_seq_no
                                        )*/
         AND shrtckd.shrtckd_dgmr_seq_no = (select max(tm.shrtckd_DGMR_SEQ_NO) from shrtckd tm where tm.shrtckd_pidm = shrtckd.shrtckd_pidm 
                                        and tm.shrtckd_term_code = shrtckd.shrtckd_term_code
                                        and tm.shrtckd_tckn_seq_no = shrtckd.shrtckd_tckn_seq_no
                                        ) 
         and shrtckd.shrtckd_dgmr_seq_no < shrdgmr.shrdgmr_seq_no                               
       AND shrtckd_pidm = a.shrtckn_pidm
       AND shrtckd_pidm = shrdgmr_pidm
       --AND shrtckd_applied_ind IS NULL
       AND a.shrtckn_term_code = shrtckd_term_code
       AND shrtckd_pidm = spriden_pidm
       AND spriden_change_ind IS NULL
       AND sobcurr_program = shrdgmr_program
       AND sobcurr.sobcurr_lock_ind = 'Y'
       AND shrdgmr_program IS NOT NULL
       AND shrdgmr.shrdgmr_degs_code != 'AW'
       AND (a.shrtckn_ptrm_code NOT LIKE 'IP%' or (a.shrtckn_ptrm_code like 'IP%' and shrtckn_subj_code = 'HES'))
      -- and a.shrtckn_subj_code not in (select DISTINCT(scbcrse_subj_code) from scbcrse where scbcrse_coll_code in ('RE') and scbcrse_eff_term >= '202106')
       AND shrdgmr.shrdgmr_program = scbcrse_add.valid_program_1
       AND a.shrtckn_subj_code = scbcrse_add.scbcrse_subj_code
       AND a.shrtckn_crse_numb = scbcrse_add.scbcrse_crse_numb
       and scbcrse_add.scbcrse_eff_term in (select max(b.scbcrse_eff_term) from scbcrse_add b where b.scbcrse_subj_code = scbcrse_add.scbcrse_subj_code and b.scbcrse_crse_numb = scbcrse_add.scbcrse_crse_numb)
       AND shrtckd.shrtckd_pidm IN (SELECT --+ INLINE
                                   sgbstdn_pidm AS popsel_pidm
                                    FROM   sgbstdn a
                                    WHERE  sgbstdn_term_code_eff =
           (SELECT Max (sgbstdn_term_code_eff)
            FROM   sgbstdn
            WHERE  sgbstdn_pidm = a.sgbstdn_pidm)
           AND EXISTS (SELECT 'x'
                       FROM   stvstst,
                              sgbstdn
                       WHERE  sgbstdn_term_code_eff =
               (SELECT Max (sgbstdn_term_code_eff)
                FROM   sgbstdn
                WHERE  sgbstdn_pidm = a.sgbstdn_pidm)
               AND sgbstdn_pidm = a.sgbstdn_pidm
               AND sgbstdn_stst_code = stvstst_code
               AND stvstst_reg_ind = 'Y')
           AND ( EXISTS (SELECT 'x'
                         FROM   stvrsts,
                                sfrstcr
                         WHERE  stvrsts_code =
                                sfrstcr_rsts_code
                                AND sfrstcr_pidm =
                                    sgbstdn_pidm
                                AND sfrstcr_term_code
                                    >=
                                    '201808')
                  OR sgbstdn_term_code_admit >=
                     '201909' )
           AND sgbstdn_pidm NOT IN
               (SELECT DISTINCT sprhold_pidm
                FROM   sprhold
                WHERE  sprhold_hldd_code LIKE 'Z_'
                and sprhold_to_date > sysdate))
  --  and shrdgmr.shrdgmr_degc_code in (select stvdegc_code from stvdegc where stvdegc_dlev_code = 'BA')
     AND a.shrtckn_pidm = attr.shrtckn_pidm(+)
     AND a.shrtckn_term_code = attr.shrtckn_term_code(+)
     AND a.shrtckn_seq_no = attr.shrtckn_seq_no(+)
     AND attr.hidejs IS NULL 
       AND shrdgmr.shrdgmr_program NOT IN ( 'ST-AA', 'IP', 'FG-MA-PSY','FG-MS-SPMS','RE-RB2-KOL','RE-RB-RAB-I')
        AND shrdgmr.shrdgmr_program IN
                              (SELECT --+ INLINE
                              sgvacur_program
                               FROM sgvacur
                               WHERE     sgvacur_pidm = a.shrtckn_pidm
                                     AND sgvacur_levl_code = shrdgmr_levl_code
                                     AND sgvacur_stdn_term_code_eff =
                                         (SELECT MAX (sgbstdn_term_code_eff)
                                          FROM sgbstdn
                                          WHERE     sgbstdn_pidm = a.shrtckn_pidm
                                                AND sgbstdn_term_code_eff <=
                                                    baninst1.F_YU_RETURN_MIN_TERM)
                                     AND sgvacur_order > 0
                                     AND NVL (sgvacur_term_code_end,
                                              '999999'
                                             ) >
                                         sgvacur_stdn_term_code_eff
                                     AND sgvacur_current_cde = 'Y')
       AND shrdgmr_levl_code NOT IN( 'UT', 'HS' )
;