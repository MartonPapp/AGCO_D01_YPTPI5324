CLASS yptpi5324cl_cu60_dao DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60_DAO
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Database Access Object
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
  PUBLIC SECTION.
    INTERFACES yptpi5324i_cu60_dao .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS yptpi5324cl_cu60_dao IMPLEMENTATION.
  METHOD yptpi5324i_cu60_dao~sel_chars_for_char_ids.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60_DAO
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Database Access Object
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    SELECT cabn~atinn, cabn~atnam, cabn~atfor,
           cabnt~atbez
    FROM cabn
    LEFT OUTER JOIN cabnt
    ON cabnt~atinn = cabn~atinn
    AND cabnt~spras EQ @sy-langu
        INTO CORRESPONDING FIELDS OF TABLE @rt_chars
        WHERE cabn~atinn IN @it_char_id_rng.
  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~sel_chars_for_char_names.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60_DAO
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Database Access Object
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    SELECT cabn~atinn, cabn~atnam, cabn~atfor,
           cabnt~atbez
    FROM cabn
    LEFT OUTER JOIN cabnt
    ON cabn~atinn = cabnt~atinn
        INTO CORRESPONDING FIELDS OF TABLE @rt_chars
        WHERE cabn~atnam IN @it_char_name_rng
        AND   cabnt~spras EQ @sy-langu.
    IF sy-subrc NE 0.
      MESSAGE e002 INTO DATA(lv_msg) ##NEEDED.
      RAISE EXCEPTION TYPE ycx_agco_globcore_exception.
    ENDIF.
  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~sel_vals_for_vartab.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60_DAO
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Database Access Object
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    SELECT * FROM cuvtab_valc
        INTO CORRESPONDING FIELDS OF TABLE @rt_vals_for_vartab
        WHERE vtint EQ @iv_vartab_id
          AND atinn IN @it_char_id_rng.
  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~sel_numvals_for_vartab.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60_DAO
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Database Access Object
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    select * from cuvtab_valn
        into corresponding fields of table @rt_numvals_for_vartab
        where vtint eq @iv_vartab_id
          and atinn in @it_char_id_rng.

  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~sel_vartab.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60_DAO
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Database Access Object
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    SELECT SINGLE *
    FROM cuvtab
    INTO CORRESPONDING FIELDS OF @rs_vartab
    WHERE vtnam EQ @iv_vartab.
    IF sy-subrc NE 0.
      MESSAGE e003 INTO DATA(lv_msg) ##NEEDED.
      RAISE EXCEPTION TYPE ycx_agco_globcore_exception.
    ENDIF.
  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~complex_selections_dialog.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60_DAO
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Database Access Object
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    rv_canceled = abap_false.
    CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
      EXPORTING
        title             = iv_title    " Dialog Box Title
        excluded_options  = is_excluded_options    " List of Options Not Allowed
      TABLES
        range             = ct_range    " Contents Table
      EXCEPTIONS
        no_range_tab      = 1
        cancelled         = 2
        internal_error    = 3
        invalid_fieldname = 4
        OTHERS            = 5.
    IF sy-subrc EQ 2.
      rv_canceled = abap_true.
    ELSEIF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msg).
      RAISE EXCEPTION TYPE ycx_agco_globcore_exception.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
