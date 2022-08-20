*&---------------------------------------------------------------------*
*&  包含                ZFIR030_ALV
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_alv .
  define hout.
    WA_FIELDCAT-FIELDNAME = '&1'.
    WA_FIELDCAT-SELTEXT_L = &2.
    WA_FIELDCAT-KEY       = &3.
    wa_fieldcat-JUST      = &4.
*    wa_fieldcat-edit      = &4.
*    wa_fieldcat-outputlen = &5.
    APPEND WA_FIELDCAT TO GT_FIELDCAT.
    CLEAR WA_FIELDCAT.
  end-of-definition.
  HOUT: LID        '序号' 'X' '',
        NAME_ORG1  '业务合作伙伴' 'X' '',
        PARTNER    '授信合同号' 'X' '',
        DATEF      '签署是期从' '' '',
        DATET      '签署是期到' '' '',
        SXAMT      '授信额度' '' '',
        ZWAERS     '授信币种' '' '',
*        BPKIND     '授信类型' '' '',
        TEXT40     '授信类型' '' '',
        RANL       '贷款合同号' '' '',
        BUKRS      '贷款公司' '' '',
        BUTXT      '贷款公司名称' '' '',
        SANTWHR    '贷款币种' '' '',
        BZUSAGE    '贷款金额(原币)' '' '',
        UKURS      '汇率' '' '',
        RMBAMT     '贷款金额(人民币)' '' '',
        DGUEL_KK   '提款日期' '' '',
        DGUEL_KP   '到期日期' '' '',
        LITDAY     '贷款期限' '' '',
        XLTEXT_QX  '贷款期限描述' '' '' ,
        XTEXT      '付息方式' '' '',
        XKBEZ      '贷款类别' '' '',
        XKBEZ1     '贷款类型' '' '',
        XKBEZ2     '贷款利率' '' '',
        PKOND      '实际利率%' '' '',
        XLTEXT     '用途' '' '',
*        XALLB      '用款条件' '' '',
        XKBEZ3     '担保方式' '' '',
        SGR1       '担保情况' '' '',
        XLBEZ      '担保情况描述' '' '',
        ZSFTQ      '是否提前还本' '' '',
        BCWHR      '提前还本金额' '' '',
        DDISPO     '提前还本日期' '' '',
        DQTS       '贷款到期天数' '' '',
        SFDQ       '是否到期' '' ''.

endform.                    " BUILD_ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_alv .
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-detail_initial_lines = 'X'.
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET = 'PF_STATUS_SET'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat[]
      i_save             = 'A'
    tables
      t_outtab           = it_alv
    exceptions
      program_error      = 1
      others             = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
endform.                    " DISPLAY_ALV
