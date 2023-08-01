CLASS yptpi5324cl_cu60 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Implements the logic for creating the dynamic
*                        table type for the variant table display
*                        and the table transpose of the cuvtab_valc to
*                        fill up the dynamic table.
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
  PUBLIC SECTION.

    METHODS constructor IMPORTING iv_vartab            TYPE vtnam
                                  iv_restrict_comb_ids TYPE i OPTIONAL
                        RAISING   ycx_agco_globcore_exception.
    METHODS get_chars_for_selscreen RETURNING VALUE(rt_chars) TYPE yptpi5324tt_chars
                                    RAISING   ycx_agco_globcore_exception.
    METHODS get_possible_charvals IMPORTING iv_charid          TYPE atinn
                                  RETURNING VALUE(rs_charvals) TYPE yptpi5324s_vals_for_chars
                                  RAISING   ycx_agco_globcore_exception.
    METHODS: multiple_selection IMPORTING iv_title            TYPE sy-title
                                          is_excluded_options TYPE rsoptions OPTIONAL
                                CHANGING  ct_range            TYPE ysmkr5304tt_atwrt
                                RETURNING VALUE(rv_canceled)  TYPE abap_bool
                                RAISING   ycx_agco_globcore_exception.
    METHODS vartab_vs_dbtab RETURNING VALUE(rv_dbtab_name) TYPE tabname16.

    METHODS get_transposed_vartab_vals IMPORTING it_vals_for_chars           TYPE yptpi5324tt_vals_for_chars
                                                 iv_restrict_comb_ids        TYPE i OPTIONAL
                                       RETURNING VALUE(ro_transposed_vartab) TYPE REF TO data
                                       RAISING   ycx_agco_globcore_exception.
    METHODS get_nr_of_slnid  IMPORTING it_vals_for_chars      TYPE yptpi5324tt_vals_for_chars
                             RETURNING VALUE(rv_nr_of_result) TYPE i
                             RAISING   ycx_agco_globcore_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_cu60_dao TYPE REF TO yptpi5324i_cu60_dao.

    DATA: ms_vartab TYPE cuvtab.
    DATA: mst_vals_for_vartab TYPE yptpi5324tt_cuvtab_valn_valc.
    DATA: mt_char_names TYPE yptpi5324tt_chars.
    DATA: mt_char_vals_per_id_and_name TYPE tty_char_vals_per_id_and_name.
    DATA: mo_transposed_tab       TYPE REF TO data,
          mo_transposed_tab_struc TYPE REF TO data.

    METHODS create_trnspsd_table IMPORTING it_vals_for_chars              TYPE yptpi5324tt_vals_for_chars
                                           iv_restrict_comb_ids           TYPE i OPTIONAL
                                 RETURNING VALUE(ro_transposed_char_vals) TYPE REF TO data
                                 RAISING   ycx_agco_globcore_exception.
    METHODS: get_type_for_vartab_chars RETURNING VALUE(rr_new_type) TYPE REF TO cl_abap_structdescr.
    METHODS: create_dynamic_table RETURNING VALUE(ro_new_tab) TYPE REF TO data.
    METHODS: create_val_tabs_per_char IMPORTING it_vals_for_chars                   TYPE yptpi5324tt_vals_for_chars
                                                ist_vals_for_vartab                 TYPE yptpi5324tt_cuvtab_valn_valc
                                      RETURNING VALUE(rt_char_vals_per_id_and_name) TYPE tty_char_vals_per_id_and_name
                                      RAISING   ycx_agco_globcore_exception.
    METHODS: fill_transptab_w_vartab_vals IMPORTING it_vals_for_vartab TYPE yptpi5324tt_cuvtab_valn_valc
                                                    it_vals_for_chars  TYPE yptpi5324tt_vals_for_chars
                                          RAISING   ycx_agco_globcore_exception.
    METHODS: collect_char_names RAISING   ycx_agco_globcore_exception.
    METHODS: filter_act_chars IMPORTING it_vals_for_chars TYPE yptpi5324tt_vals_for_chars
                              CHANGING  ct_char_names     TYPE yptpi5324tt_chars
                              RAISING   ycx_agco_globcore_exception.
    METHODS: values_in_selection IMPORTING it_char_vals_per_id_and_name TYPE tty_char_vals
                                           it_mltpl_vals                TYPE ysmkr5304tt_atwrt
                                 RETURNING VALUE(rv_res)                TYPE abap_bool.
    METHODS: filter_vals_for_vartab IMPORTING ist_vals_for_vartab        TYPE yptpi5324tt_cuvtab_valn_valc
                                              iv_restrict_comb_ids       TYPE i OPTIONAL
                                    RETURNING VALUE(rst_vals_for_vartab) TYPE  yptpi5324tt_cuvtab_valn_valc.
    METHODS: check_charvals IMPORTING it_vals_for_chars TYPE yptpi5324tt_vals_for_chars
                            RAISING   ycx_agco_globcore_exception.
    METHODS: clear_objects.
    METHODS: merge_valc_valn IMPORTING it_charvals_for_vartab TYPE yptpi5324tt_cuvtab_valc
                                       it_numvals_for_vartab  TYPE yptpi5324tt_cuvtab_valn
                             RAISING   ycx_agco_globcore_exception.
    METHODS:conv_packed IMPORTING iv_char_val_char_numval_from TYPE ty_char_vals-char_numval_from
                        RETURNING VALUE(rv_packed)             TYPE char30.
    METHODS: conv_date IMPORTING iv_char_val_char_numval_from TYPE ty_char_vals-char_numval_from
                       RETURNING VALUE(rv_date)               TYPE char30.
    METHODS: conv_num_values_to_char RAISING   ycx_agco_globcore_exception.
    METHODS: restrict_values IMPORTING iv_restrict_comb_ids       TYPE i
                             RETURNING VALUE(rst_vals_for_vartab) TYPE yptpi5324tt_cuvtab_valn_valc.
ENDCLASS.



CLASS yptpi5324cl_cu60 IMPLEMENTATION.


  METHOD check_charvals.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    "Are the input values for the characteristics relevant (for filtering)?
    LOOP AT it_vals_for_chars ASSIGNING FIELD-SYMBOL(<ls_vals_for_chars>).
      DATA(lt_act_vals) = VALUE tty_char_vals( FOR ls_vals_for_vartab IN mst_vals_for_vartab
                                                  WHERE ( atinn = <ls_vals_for_chars>-atinn )
                                                  ( char_val = ls_vals_for_vartab-valc ) ).
      LOOP AT <ls_vals_for_chars>-multpl_vals ASSIGNING FIELD-SYMBOL(<ls_selection_vals>).
        READ TABLE lt_act_vals WITH KEY char_val = <ls_selection_vals>-low TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          MESSAGE e004(yptpi5324) INTO DATA(lv_msg) ##NEEDED.
          RAISE EXCEPTION TYPE ycx_agco_globcore_exception.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD clear_objects.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    CLEAR mt_char_names.
    CLEAR mt_char_vals_per_id_and_name.
    CLEAR mo_transposed_tab.
    CLEAR mo_transposed_tab_struc.
  ENDMETHOD.


  METHOD collect_char_names.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    "Collect the actually selected Characteristic IDs
    DATA(lt_char_id_rng) = VALUE yptpi5324tt_atinn_rng( FOR GROUPS OF <char_id> IN mst_vals_for_vartab
                                                        GROUP BY <char_id>-atinn
                                                        ( sign = 'I'
                                                          option = 'EQ'
                                                          low = <char_id>-atinn ) ).

    mt_char_names = VALUE #( FOR ls_char IN mo_cu60_dao->sel_chars_for_char_ids( lt_char_id_rng )
                              ( CORRESPONDING #( ls_char )  ) ).

  ENDMETHOD.


  METHOD constructor.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Constructor
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    mo_cu60_dao = yptpi5324cl_dao_factory=>get_cu60_dao_instance( ).
    ms_vartab = mo_cu60_dao->sel_vartab( iv_vartab ).

    IF ms_vartab-dbcon_acti = abap_true.
      RETURN.
    ENDIF.

    DATA(lst_charvals_for_vartab) = mo_cu60_dao->sel_vals_for_vartab( iv_vartab_id = ms_vartab-vtint ).
    DATA(lst_numvals_for_vartab) = mo_cu60_dao->sel_numvals_for_vartab( iv_vartab_id = ms_vartab-vtint ).

    IF lst_charvals_for_vartab IS INITIAL
    AND lst_numvals_for_vartab IS INITIAL.
      MESSAGE e005(yptpi5324) WITH ms_vartab-vtint INTO DATA(lv_msg) ##NEEDED.
      RAISE EXCEPTION TYPE ycx_agco_globcore_exception.
    ENDIF.

    me->merge_valc_valn( it_charvals_for_vartab = lst_charvals_for_vartab
                         it_numvals_for_vartab  = lst_numvals_for_vartab ).

    me->collect_char_names( ).
    me->conv_num_values_to_char( ).
  ENDMETHOD.


  METHOD conv_date.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    DATA: lv_val_date   TYPE dats,
          lv_val_char14 TYPE char14.
    DATA(lv_val_int) = CONV i( iv_char_val_char_numval_from ).
    DATA(lv_val_char8) = CONV char8( lv_val_int ).
    lv_val_char14 = |{ lv_val_char8 }000000|.
    DATA(lv_val_tmstmp) = CONV timestamp( lv_val_char14 ).
    CONVERT TIME STAMP lv_val_tmstmp TIME ZONE ''
        INTO DATE lv_val_date.
    rv_date = |{ lv_val_date DATE = ENVIRONMENT }|.
  ENDMETHOD.


  METHOD conv_num_values_to_char.
*    IF mt_char_names IS INITIAL.
*      me->collect_char_names( ).
*    ENDIF.
    LOOP AT mt_char_names ASSIGNING FIELD-SYMBOL(<ls_char_name>) WHERE atfor NE 'CHAR'.
      LOOP AT  mst_vals_for_vartab ASSIGNING FIELD-SYMBOL(<ls_vals_for_vartab>) WHERE ( atinn = <ls_char_name>-atinn ).
        <ls_vals_for_vartab>-valc = COND char30( WHEN <ls_char_name>-atfor EQ 'NUM'
                                                        THEN me->conv_packed( <ls_vals_for_vartab>-val_from )
                                                 WHEN <ls_char_name>-atfor EQ 'DATE'
                                                        THEN |{ me->conv_date( <ls_vals_for_vartab>-val_from ) } - { me->conv_date( <ls_vals_for_vartab>-val_to ) }|
                                                 WHEN <ls_char_name>-atfor EQ 'TIME'
                                                        THEN |{ CONV tims( <ls_vals_for_vartab>-val_from ) } - { CONV tims( <ls_vals_for_vartab>-val_to ) }|
                                                 ELSE <ls_vals_for_vartab>-valc ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD conv_packed.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    DATA: lv_packed TYPE p LENGTH 8 DECIMALS 3.
    lv_packed = CONV #( iv_char_val_char_numval_from ).
    rv_packed = lv_packed.
    CONDENSE rv_packed.
  ENDMETHOD.


  METHOD create_dynamic_table.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    DATA: lo_new_tab  TYPE REF TO cl_abap_tabledescr.

    DATA(lo_new_type) = me->get_type_for_vartab_chars( ).

    lo_new_tab = cl_abap_tabledescr=>create( p_line_type  = lo_new_type
                                             p_table_kind = cl_abap_tabledescr=>tablekind_std
                                             p_unique     = abap_false ) .

    CREATE DATA ro_new_tab TYPE HANDLE lo_new_tab.
  ENDMETHOD.


  METHOD create_trnspsd_table.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Returns whether the variant table is linked to
*                        a DB table or not
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    me->clear_objects( ).

    me->check_charvals( it_vals_for_chars ).

    me->filter_act_chars( EXPORTING it_vals_for_chars = it_vals_for_chars
                          CHANGING  ct_char_names = mt_char_names ).

    mo_transposed_tab = me->create_dynamic_table( ).

    ASSIGN mst_vals_for_vartab TO FIELD-SYMBOL(<lst_vals_for_vartab>).

    mt_char_vals_per_id_and_name = me->create_val_tabs_per_char( it_vals_for_chars = it_vals_for_chars
                                                                 ist_vals_for_vartab = <lst_vals_for_vartab> ).

    me->fill_transptab_w_vartab_vals( EXPORTING it_vals_for_vartab = me->filter_vals_for_vartab( ist_vals_for_vartab = <lst_vals_for_vartab>
                                                                                                 iv_restrict_comb_ids = iv_restrict_comb_ids )
                                                it_vals_for_chars = it_vals_for_chars ).

    ro_transposed_char_vals = mo_transposed_tab.
  ENDMETHOD.


  METHOD create_val_tabs_per_char.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Build up the helper structure of characteristic
*                        values for each combination ID and characteristic
*                        filtered by the input values
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    "Collect the characteristic values under the variant table per combination Id + characteristic
    LOOP AT ist_vals_for_vartab INTO DATA(ls_vartab_val) GROUP BY ( slnid = ls_vartab_val-slnid )
                                                           ASSIGNING FIELD-SYMBOL(<ls_slnid_grp>).
      DATA(lv_not_valid_slnid) = VALUE abap_bool(  ).
      DATA(lt_char_vals_per_id_and_name) = VALUE tty_char_vals_per_id_and_name( ).
      LOOP AT GROUP <ls_slnid_grp> ASSIGNING FIELD-SYMBOL(<ls_slnid_itm>)
          GROUP BY ( atinn = <ls_slnid_itm>-atinn ) ASSIGNING FIELD-SYMBOL(<ls_char_grp>).
        DATA(lt_vals_per_id_and_char) = VALUE tty_char_vals( FOR ls_vals IN GROUP <ls_char_grp> ( char_val = ls_vals-valc
                                                                                                  char_numval_from = ls_vals-val_from
                                                                                                  char_numval_to   = ls_vals-val_to
                                                                                                  char_numval_unit_from = ls_vals-val_unit_from
                                                                                                  char_numval_unit_to   = ls_vals-val_unit_to ) ).
        DATA(ls_vals_for_char) = it_vals_for_chars[ atinn = <ls_char_grp>-atinn ].
        "Collect those Comb. Id-s and Char-s only, which contains values coming from the input list of char values (selection screen)
        IF me->values_in_selection( it_char_vals_per_id_and_name = lt_vals_per_id_and_char
                                    it_mltpl_vals                = ls_vals_for_char-multpl_vals ).
          INSERT VALUE ty_char_vals_per_id_and_name(
                  slnid = <ls_slnid_grp>-slnid
                  char_id = <ls_char_grp>-atinn
                  atfor = VALUE #( mt_char_names[ atinn = <ls_char_grp>-atinn ]-atfor DEFAULT '' )
                  active = ls_vals_for_char-active
                  char_vals = lt_vals_per_id_and_char ) INTO TABLE lt_char_vals_per_id_and_name.
        ELSE.
          lv_not_valid_slnid = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      "In case any of the characteristics is not fulfilling the requirement, the combination Id is not relevant any more
      IF lv_not_valid_slnid EQ abap_false.
        INSERT LINES OF lt_char_vals_per_id_and_name INTO TABLE rt_char_vals_per_id_and_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_transptab_w_vartab_vals.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    "Transpose the variant table (CUVTAB_VALC) and fill up the dynamic table with the values
    " SLNID | ATINN | ATWRT
    "-------|-------|----------
    "-------|-------|----------
    " 0001  | CHAR1 | VAL1_1
    " 0001  | CHAR2 | VAL2_1
    " 0001  | CHAR2 | VAL2_2
    " 0001  | CHAR2 | VAL2_3
    "--------------------------
    " 0002  | CHAR1 | VAL1_1
    " 0002  | CHAR1 | VAL1_2
    " 0002  | CHAR2 | VAL2_4

    "         ||
    "         ||
    "         \/

    " SLNID | CHAR1  | CHAR2
    "-------|--------|----------
    "-------|--------|----------
    " 0001  | VAL1_1 | VAL2_1
    " 0001  | VAL1_1 | VAL2_2
    " 0001  | VAL1_1 | VAL2_3
    "--------------------------
    " 0002  | VAL1_1 | val2_4
    " 0002  | VAL1_2 | val2_4
    IF mt_char_vals_per_id_and_name IS INITIAL.
      MESSAGE e006(yptpi5324) INTO DATA(lv_msg) ##NEEDED.
      RAISE EXCEPTION TYPE ycx_agco_globcore_exception.
    ENDIF.
    DATA(lst_atinn_comb) = VALUE tty_atinn_cnt( ).
    DATA(lv_struc_index) = VALUE i( ).
    FIELD-SYMBOLS: <ls_transpose_tab>  TYPE STANDARD TABLE,
                   <ls_transpose_line> TYPE any.
    "Reference for the dynamic table
    ASSIGN mo_transposed_tab->* TO <ls_transpose_tab>.

    CREATE DATA mo_transposed_tab_struc LIKE LINE OF <ls_transpose_tab>.
    ASSIGN mo_transposed_tab_struc->* TO <ls_transpose_line>.

    LOOP AT it_vals_for_vartab INTO DATA(ls_vartab_val) GROUP BY ( slnid = ls_vartab_val-slnid )
                                                         ASSIGNING FIELD-SYMBOL(<ls_slnid_grp>).
      CLEAR lst_atinn_comb.

      lv_struc_index  = 1.
      ASSIGN COMPONENT lv_struc_index OF STRUCTURE <ls_transpose_line> TO FIELD-SYMBOL(<ls_field>).
      <ls_field> = <ls_slnid_grp>-slnid.

      "Characteristics in the Combination group. Number of values for each Characteristics
      lst_atinn_comb = VALUE #( FOR ls_ausp_2 IN GROUP <ls_slnid_grp>
                                  LET lv_count = REDUCE i( INIT x = 0 FOR i
                                                           IN GROUP <ls_slnid_grp>
                                                           WHERE ( atinn = ls_ausp_2-atinn ) NEXT x = x + 1 )
                                  IN
                                  ( atinn = ls_ausp_2-atinn
                                    active = it_vals_for_chars[ atinn = ls_ausp_2-atinn ]-active
                                    valc  = ls_ausp_2-valc
                                    count = lv_count ) ).
      "Max number of values (for any active Characteristic) in the group.
      "That determines the number of appended lines in the Combination group
      DATA(lv_max_size) = REDUCE i( INIT y = 0 FOR GROUPS count OF j
                                    IN lst_atinn_comb WHERE ( active = 'X') GROUP BY ( count = j-count )
                                    NEXT y = COND #( WHEN y > count-count THEN y ELSE count-count ) ).

      DO lv_max_size TIMES.
        DATA(lv_comb_id_indx) = sy-index.
        "Iterate over the Characteristics in the Combination ID group, group by the Characteristics
        LOOP AT lst_atinn_comb ASSIGNING FIELD-SYMBOL(<ls_atinn>) WHERE ( active EQ 'X' )
                    GROUP BY ( atinn = <ls_atinn>-atinn
                               gs = GROUP SIZE
                               gi = GROUP INDEX ) ASSIGNING FIELD-SYMBOL(<ls_atinn_grp>).
          "Get the values for the Comb. Id and the Characteristic from the helper structure
          ASSIGN mt_char_vals_per_id_and_name[ slnid = <ls_slnid_grp>-slnid
                                               char_id = <ls_atinn_grp>-atinn
                                               active = 'X' ] TO FIELD-SYMBOL(<ls_char_val_per_id_and_name>).
          IF sy-subrc NE 0.
            CLEAR <ls_transpose_line>.
            EXIT.
          ENDIF.

          ASSIGN COMPONENT lv_struc_index + <ls_atinn_grp>-gi OF STRUCTURE <ls_transpose_line> TO FIELD-SYMBOL(<ls_field_l>).

          READ TABLE <ls_char_val_per_id_and_name>-char_vals INDEX lv_comb_id_indx ASSIGNING FIELD-SYMBOL(<ls_char_val>).
          IF sy-subrc NE 0.
            CLEAR <ls_field_l>.
            CONTINUE.
          ENDIF.
          <ls_field_l> = <ls_char_val>-char_val.
        ENDLOOP.
        IF <ls_transpose_line> IS NOT INITIAL.
          APPEND <ls_transpose_line> TO <ls_transpose_tab>.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.


  METHOD filter_act_chars.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Delete the not relevant characteristics
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    IF ct_char_names IS INITIAL.
      me->collect_char_names( ).
      ct_char_names = mt_char_names.
    ENDIF.
    DATA: lt_atinn_rng TYPE RANGE OF atinn.
    lt_atinn_rng = VALUE #( FOR ls_vals_for_chars IN it_vals_for_chars
                                        WHERE ( active = 'X' )
                                      ( sign = 'I'
                                        option = 'EQ'
                                        low = ls_vals_for_chars-atinn ) ).
    DELETE ct_char_names WHERE atinn NOT IN lt_atinn_rng.
  ENDMETHOD.


  METHOD filter_vals_for_vartab.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    DATA: lt_slnid_rng TYPE RANGE OF slnid.

    LOOP AT mt_char_vals_per_id_and_name ASSIGNING FIELD-SYMBOL(<ls_char_vals>)
                                           GROUP BY ( slnid = <ls_char_vals>-slnid
                                                      gi = GROUP INDEX ) ASSIGNING FIELD-SYMBOL(<lr_slnid_grp>).
      "Restrict the number of combination Ids to the Maximum number of hits
      IF <lr_slnid_grp>-gi LE iv_restrict_comb_ids.
        APPEND VALUE #( sign = 'I'
                        option = 'EQ'
                        low = <lr_slnid_grp>-slnid ) TO lt_slnid_rng.
      ENDIF.
    ENDLOOP.

    "Restrict the characteristic value lines with the filtered combination ids
    LOOP AT ist_vals_for_vartab ASSIGNING FIELD-SYMBOL(<ls_vals_for_vartab>).
      IF <ls_vals_for_vartab>-slnid IN lt_slnid_rng.
        INSERT <ls_vals_for_vartab> INTO TABLE rst_vals_for_vartab.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_chars_for_selscreen.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    rt_chars = mt_char_names.
  ENDMETHOD.

  METHOD get_nr_of_slnid.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    DATA(lt_char_vals_per_id_and_name) = me->create_val_tabs_per_char( it_vals_for_chars = it_vals_for_chars
                                                                       ist_vals_for_vartab = mst_vals_for_vartab ).
    DATA: lt_slnid_rng TYPE RANGE OF slnid.
    lt_slnid_rng = VALUE #( FOR GROUPS OF  ls_char_vals IN lt_char_vals_per_id_and_name
                            GROUP BY ls_char_vals-slnid
                            ( sign = 'I'
                              option = 'EQ'
                              low = ls_char_vals-slnid ) ).
    rv_nr_of_result = lines( lt_slnid_rng ).
  ENDMETHOD.

  METHOD get_possible_charvals.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    DATA(lt_charvals) = VALUE ysmkr5304tt_atwrt( FOR ls_vals IN mst_vals_for_vartab WHERE ( atinn = iv_charid )
                                                      ( sign = 'I'
                                                        option = 'EQ'
                                                        low = ls_vals-valc ) ).
    SORT lt_charvals BY low.
    DELETE ADJACENT DUPLICATES FROM lt_charvals COMPARING low.
    rs_charvals = VALUE #( atinn = iv_charid
                           multpl_vals = lt_charvals ).
  ENDMETHOD.


  METHOD get_transposed_vartab_vals.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : - prepares the data for evaluation
*                        - creates dynamic table type
*                        - creates helper data for the table transpose
*                        - table transpose
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*

    ro_transposed_vartab = me->create_trnspsd_table( it_vals_for_chars = it_vals_for_chars
                                                     iv_restrict_comb_ids = iv_restrict_comb_ids ).

  ENDMETHOD.


  METHOD get_type_for_vartab_chars.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    DATA: lt_comp TYPE cl_abap_structdescr=>component_table,
          ls_comp LIKE LINE OF lt_comp.

    "First attribute always the key for value combination in the variant table
    ls_comp-name =  'SLNID' .
    ls_comp-type = cl_abap_elemdescr=>get_n( p_length = 5 ).

    APPEND ls_comp TO lt_comp.
    "Next attributes are the Characteristic names
    LOOP AT mt_char_names ASSIGNING FIELD-SYMBOL(<ls_char_name>).
      ls_comp-name = <ls_char_name>-atnam.
      ls_comp-type = cl_abap_elemdescr=>get_c( p_length = 30 ).
      APPEND ls_comp TO lt_comp.
      CLEAR ls_comp.
    ENDLOOP.

    rr_new_type = cl_abap_structdescr=>create( lt_comp ).
  ENDMETHOD.


  METHOD merge_valc_valn.
    mst_vals_for_vartab = CORRESPONDING #( it_charvals_for_vartab ).
    mst_vals_for_vartab = CORRESPONDING #( BASE ( mst_vals_for_vartab )  it_numvals_for_vartab ).

  ENDMETHOD.


  METHOD multiple_selection.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    rv_canceled = mo_cu60_dao->complex_selections_dialog( EXPORTING iv_title = iv_title
                                                                    is_excluded_options = is_excluded_options
                                                          CHANGING  ct_range = ct_range ).
  ENDMETHOD.


  METHOD values_in_selection.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          :
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    LOOP AT it_char_vals_per_id_and_name ASSIGNING FIELD-SYMBOL(<ls_char_vals>).
      IF <ls_char_vals>-char_val IN it_mltpl_vals.
        rv_res = abap_true.
        TRY.
            IF it_mltpl_vals[ low = <ls_char_vals>-char_val ]-sign EQ 'I'.
              RETURN.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.
      ELSE.
        rv_res = abap_false.
        TRY.
            IF it_mltpl_vals[ low = <ls_char_vals>-char_val ]-sign EQ 'E'.
              RETURN.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD vartab_vs_dbtab.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Returns whether the variant table is linked to
*                        a DB table or not
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    IF ms_vartab-dbcon_acti = abap_true.
      rv_dbtab_name = ms_vartab-dbtab_name.
    ELSE.
      rv_dbtab_name = ''.
    ENDIF.
  ENDMETHOD.


  METHOD restrict_values.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_CU60
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Returns whether the variant table is linked to
*                        a DB table or not
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
    rst_vals_for_vartab = mst_vals_for_vartab.
    CHECK iv_restrict_comb_ids IS NOT INITIAL.
    DELETE rst_vals_for_vartab WHERE slnid GT iv_restrict_comb_ids.
  ENDMETHOD.

ENDCLASS.
