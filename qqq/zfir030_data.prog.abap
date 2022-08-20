*&---------------------------------------------------------------------*
*&  包含                ZFIR030_DATA
*&---------------------------------------------------------------------*
tables:but000,vdarl.

type-pools:slis.
data: gt_fieldcat      type table of slis_fieldcat_alv,
      wa_fieldcat      like line of  gt_fieldcat,
      it_sort          type table of slis_sortinfo_alv,
      gs_layout        type          slis_layout_alv,
      gs_list_top_page type          slis_t_listheader,
      gw_list_top_page type          slis_listheader.
data: it_but000  like table of but000 with header line,
      it_vdarl   like table of vdarl  with header line,
      it_vzzkopo like table of vzzkopo with header line,
      it_vzzkoko like table of vzzkoko with header line,
      it_but020  like table of but020 with header line.
data: begin of it_alv occurs 0,
        lid       type i,
        name_org1 like but000-name_org1,
        partner   like but000-partner,
        datef     like sy-datum,
        datet     like sy-datum,
        title_let like but000-title_let,
        sxamt     type p decimals 2,
        zwaers    like but000-zwaers,
        bpkind    like but000-bpkind,
        text40    like tb004t-text40, "授信类型
        ranl      like vdarl-ranl,
        bukrs     like vdarl-bukrs,
        butxt     like t001-butxt, "公司名称
        santwhr   like vdarl-santwhr,
        bzusage   like vzzkoko-bzusage,
        ukurs     like tcurr-ukurs,
        rmbamt    type wrbtr,
        dguel_kk  like vzzkoko-dguel_kk,
        dguel_kp  like vzzkopo-dguel_kp,
        litday    type i,
        xltext_qx like td09t-xltext, "贷款期限描述 add by  fanglei  20210309
        svzweck   like vdarl-svzweck,
        xtext     like td07t-xtext, "付息方式
        sdtyp     like vdarl-sdtyp,
        xkbez     like td02t-xkbez,  "贷款类别
        stitel    like vdarl-stitel,
        xkbez1    like td03t-xkbez,   "贷款类型
        ssicher   like vdarl-ssicher,
        xkbez2    like td01t-xkbez,   "贷款利率
        pkond     like vzzkopo-pkond, "实际利率
        skueart   like vzzkoko-skueart,
        xltext(100) TYPE c,         " like td27t-xltext,  "用途
        xallb     like vdarl-xallb,
        stitart   like vdarl-stitart,
        xkbez3    like td16t-xkbez,  "担保方式
        skuegl    like vzzkoko-skuegl,
        sgr1      like vdarl-sgr1,
        xltext2   like td28t-xltext,  "担保情况
        xlbez     like tddga-xlbez,   "担保情况 add by  fanglei  20210309
        zsftq   ,  "是否提前还款
        bcwhr     type wrbtr, "提前还本金额
        ddispo    like vdbepi-ddispo, "提前还本日期
        dqts      TYPE i ,      "货款到期天数
        sfdq      ,             "是否到期

      end of it_alv.

DATA:gt_ZFIT021 LIKE TABLE OF  ZFIT021 WITH HEADER LINE.

selection-screen begin of block ven with frame title text-001.
select-options: s_parn  for but000-partner,
                s_ranl     for vdarl-ranl.


selection-screen end of block ven.

PARAMETERS p_save type c AS CHECKBOX.
