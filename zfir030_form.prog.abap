*&---------------------------------------------------------------------*
*&  包含                ZFIR030_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PARTNER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_partner_data .
  CLEAR: it_but000,it_but000[],it_but020,it_but020[],it_vdarl,it_vdarl[].

  SELECT a~* INTO CORRESPONDING FIELDS OF TABLE @it_but000
    FROM but000 AS a
    INNER JOIN but100 AS b ON a~partner = b~partner
    WHERE a~partner IN @s_parn
      AND b~rltyp = 'TR0100'.

  IF it_but000[] IS NOT INITIAL.
    SELECT * INTO TABLE it_but020
      FROM but020
       FOR ALL ENTRIES IN it_but000
     WHERE partner = it_but000-partner.
  ENDIF.
ENDFORM.                    " GET_PARTNER_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_RANL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ranl_data .
  CLEAR:it_vdarl,it_vdarl[],it_vzzkoko,it_vzzkoko[],it_vzzkopo,it_vzzkopo[].
  SELECT * INTO TABLE it_vdarl
    FROM vdarl
     FOR ALL ENTRIES IN it_but000
   WHERE rdarnehm = it_but000-partner
*     and ranl in s_ranl
     AND gsart = '33B'
     AND vvsloekz <> '1'. "  add 20210312
  IF it_vdarl[] IS NOT INITIAL.
    SELECT * INTO TABLE it_vzzkoko
      FROM vzzkoko
       FOR ALL ENTRIES IN it_vdarl
     WHERE bukrs = it_vdarl-bukrs
       AND rkey1 = it_vdarl-ranl.
    SELECT * INTO TABLE it_vzzkopo
      FROM vzzkopo
       FOR ALL ENTRIES IN it_vdarl
     WHERE bukrs = it_vdarl-bukrs
       AND rkey1 = it_vdarl-ranl.

  ENDIF.
ENDFORM.                    " GET_RANL_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ALV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_alv_data .
  DATA: tm_date TYPE c LENGTH 15.
  DATA: pp_date LIKE syst-datum,
        pp_curr LIKE tcurr-fcurr.
  DATA: sum_i TYPE i.
  CLEAR:it_alv,it_alv[].


  "是否提前还本
  IF  it_vdarl IS NOT INITIAL .

    SELECT a~bukrs,
            a~rbelkpfd,
            a~ranl,
            b~bcwhr,
            b~ddispo
      FROM vdbeki AS a
      INNER JOIN vdbepi AS b ON a~bukrs = b~bukrs AND a~rbelkpfd = b~rbelkpfd
      INTO TABLE  @DATA(lt_vdbepi)
      FOR ALL ENTRIES IN @it_vdarl
      WHERE a~bukrs = @it_vdarl-bukrs
        AND a~ranl  = @it_vdarl-ranl
        AND b~sbewart = '1135'.

  ENDIF.

  sum_i = 0.
  LOOP AT it_but000.

    IF  it_but000-name_org1 IS  INITIAL .
      it_but000-name_org1 = it_but000-name_grp1.
    ENDIF.


    MOVE-CORRESPONDING it_but000 TO it_alv.
    SELECT SINGLE text40 INTO it_alv-text40
       FROM tb004t
       WHERE spras   = 1
         AND bpkind  = it_alv-bpkind.

*     mod   by  fanglei  at  20210309
*    it_alv-sxamt = it_but000-title_let.
*    read table it_but020 with key partner = it_but000-partner.
*    if sy-subrc = 0.
*      tm_date = it_but020-addr_valid_from.
*      it_alv-datef = tm_date+0(8).
*      tm_date = it_but020-addr_valid_to.
*      it_alv-datet = tm_date+0(8).
*    endif.


    it_alv-sxamt = it_but000-zsxje.
    it_alv-datef = it_but000-zsxqj.
    it_alv-datet = it_but000-zsxqj2.

    "授信类型
    SELECT SINGLE text40 INTO it_alv-text40
      FROM tb004t
     WHERE spras  = 1
       AND bpkind = it_alv-bpkind.

    READ TABLE it_vdarl WITH KEY rdarnehm = it_but000-partner.
    IF sy-subrc = 0.
      SORT it_vdarl BY bukrs ranl.
      LOOP AT it_vdarl WHERE rdarnehm = it_but000-partner.
        MOVE-CORRESPONDING it_vdarl TO it_alv.

        SELECT SINGLE butxt
          FROM t001
          INTO it_alv-butxt
          WHERE bukrs = it_alv-bukrs. "   add by  fanglei at 20210309

        READ TABLE it_vzzkoko WITH KEY bukrs = it_vdarl-bukrs
                                       rkey1 = it_vdarl-ranl.
        IF sy-subrc = 0.
          it_alv-bzusage   = it_vzzkoko-bzusage.
          it_alv-dguel_kk  = it_vzzkoko-dguel_kk.
          it_alv-skueart   = it_vzzkoko-skueart.
          it_alv-skuegl    = it_vzzkoko-skuegl.
*          mod  by fanglei 20210309
*          "担保情况
*          it_alv-xltext2 = ''.
*          select single xltext into it_alv-xltext2
*            from td28t
*           where spras   = 1
*             and skuegl  = it_vzzkoko-skuegl.
          "用途
*          it_alv-xltext = ''.
*          select single xltext into it_alv-xltext
*            from td27t
*           where spras   = 1
*             and skueart = it_vzzkoko-skueart.

        ENDIF.


        "担保情况
        it_alv-xlbez = ''.
        SELECT SINGLE xlbez
          INTO it_alv-xlbez
          FROM tddga
          WHERE spras = sy-langu
          AND sgrp1 = it_vdarl-sgr1.

*        it_alv-xltext2 = it_vdarl-sgr1.
        "用途
        it_alv-xltext =  it_vdarl-xallb.
*        end  mod.

        READ TABLE it_vzzkopo WITH KEY bukrs  = it_vdarl-bukrs
                                       rkey1  = it_vdarl-ranl
                                       skoart = '0563'.
        IF sy-subrc = 0.
*          it_alv-dguel_kp = it_vzzkopo-dguel_kp.
          it_alv-dguel_kp = it_vzzkopo-dfaell. "mod  by  fanglei 20210309
        ENDIF.

*       货款到期天数
        it_alv-dqts  = sy-datum - it_alv-dguel_kp.
        IF  it_alv-dqts LE 0.
          it_alv-sfdq = '否'.
        ELSE .
          it_alv-sfdq = '是'.
        ENDIF.

        READ TABLE it_vzzkopo WITH KEY bukrs  = it_vdarl-bukrs
                                       rkey1  = it_vdarl-ranl
                                       skoart = '0501'.
        IF sy-subrc = 0.
*          it_alv-pkond    = it_vzzkopo-pkond.
*      mod  by  fanglei 20210309
          it_alv-xkbez2 = it_vzzkopo-pkond.
          it_alv-pkond  = it_vzzkoko-peffzins.
*      end  mod
        ENDIF.

        " 贷款类别
        it_alv-xkbez = ''.
        SELECT SINGLE xkbez INTO it_alv-xkbez
          FROM td02t
         WHERE spras = 1
           AND sdtyp = it_vdarl-sdtyp.

        "  贷款类型
        it_alv-xkbez1 = ''.
        SELECT SINGLE xkbez INTO it_alv-xkbez1
          FROM td03t
         WHERE spras  = 1
           AND stitel = it_vdarl-stitel.

*      mod  by  fanglei 20210309
        "贷款利率
*        it_alv-xkbez2 = ''.
*        select single xkbez into it_alv-xkbez2
*          from td01t
*         where spras  = 1
*           and ssich = it_vdarl-ssicher.
*      end  mod

        "担保方式
        it_alv-xkbez3 = ''.
        SELECT SINGLE xkbez INTO it_alv-xkbez3
          FROM td16t
         WHERE spras   = 1
           AND stitart = it_vdarl-stitart.

        "付息方式
        it_alv-xtext = ''.
        SELECT SINGLE xtext INTO it_alv-xtext
          FROM td07t
         WHERE spras  = 1
           AND gsart  = it_vdarl-gsart
           AND svzweck = it_vdarl-svzweck.

*  mod by fanglei 20210309
*        it_alv-litday  = it_alv-dguel_kp - it_alv-dguel_kk .
        SELECT SINGLE xltext
          INTO it_alv-xltext_qx
          FROM td09t
          WHERE spras = sy-langu
          AND sfrist = it_vdarl-sfrist.
        it_alv-litday = it_vdarl-sfrist.
*  mod end
        pp_date = it_alv-dguel_kk.
        pp_curr = it_alv-santwhr.

        CALL FUNCTION 'READ_EXCHANGE_RATE'
          EXPORTING
            client           = sy-mandt
            date             = pp_date
            foreign_currency = pp_curr
            local_currency   = 'CNY'
            type_of_rate     = 'M'
*           EXACT_DATE       = ' '
          IMPORTING
            exchange_rate    = it_alv-ukurs
          EXCEPTIONS
            no_rate_found    = 1
            no_factors_found = 2
            no_spread_found  = 3
            derived_2_times  = 4
            overflow         = 5
            zero_rate        = 6
            OTHERS           = 7.
        IF sy-subrc <> 0.
          it_alv-ukurs = 1.
        ENDIF.

*   mod by fanglei  20210309
*        it_alv-rmbamt = it_alv-bzusage * it_alv-ukurs.
*        if it_alv-santwhr = 'JPY'.
*          it_alv-rmbamt = it_alv-rmbamt / 1000.
*        endif.
        it_alv-rmbamt = it_alv-bzusage.
        it_alv-rmbamt = zcl_common2=>conversion_curr( iv_wrbtr = it_alv-rmbamt iv_waers = pp_curr ).
        it_alv-rmbamt = it_alv-rmbamt * it_alv-ukurs.
*   end mod

        LOOP AT lt_vdbepi INTO DATA(ls_vdbepi) WHERE bukrs = it_alv-bukrs
                AND ranl = it_alv-ranl.

          it_alv-bcwhr  = ls_vdbepi-bcwhr.
*          add by fanglei 20210309
          it_alv-bcwhr = zcl_common2=>conversion_curr( iv_wrbtr = it_alv-bcwhr iv_waers = pp_curr ).
*          end add

          it_alv-ddispo = ls_vdbepi-ddispo.
          it_alv-zsftq  = '是'.
          sum_i = sum_i + 1.
          it_alv-lid =  sum_i.
          APPEND it_alv.
        ENDLOOP.

        IF sy-subrc <> 0.
          it_alv-zsftq  = '否'.
          sum_i = sum_i + 1.
          it_alv-lid =  sum_i.
          APPEND it_alv.
        ENDIF.

      ENDLOOP.
      CLEAR it_alv.
    ELSE.
      sum_i = sum_i + 1.
      it_alv-lid =  sum_i.
      APPEND it_alv.
      CLEAR it_alv.
    ENDIF.
  ENDLOOP.

  IF  s_ranl  IS NOT INITIAL .
    DELETE it_alv WHERE ranl  NOT IN s_ranl.
  ENDIF.

  IF p_save IS NOT INITIAL.
    DELETE FROM zfit021.
    LOOP AT it_alv.

      MOVE-CORRESPONDING it_alv TO gt_zfit021.
      APPEND gt_zfit021.
      CLEAR gt_zfit021.
    ENDLOOP.

    IF gt_zfit021[] IS NOT INITIAL.
      MODIFY zfit021 FROM TABLE gt_zfit021.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT .
        MESSAGE '下发BI数据库成功' TYPE 'S'.
      ELSE.
        ROLLBACK WORK.
        MESSAGE '下发BI数据库失败' TYPE 'I'.
      ENDIF.
    ENDIF.


  ENDIF.
ENDFORM.                    " GET_ALV_DATA
