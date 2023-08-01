*&---------------------------------------------------------------------*
*& Report  YPTPI5324R_CU60_DISPLAY
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Variant table display function without
*                        authorization.
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
REPORT yptpi5324r_cu60_display.

INCLUDE yptpi5324r_cu60_displaytop.

CLASS lcl_cu60_display DEFINITION DEFERRED.


CLASS lcl_cu60_display DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_inst RETURNING VALUE(ro_inst) TYPE REF TO lcl_cu60_display.

    METHODS: constructor.

    METHODS call_screen_1100.
    METHODS: startup_vartab_proc IMPORTING iv_vartab TYPE vtnam
                                 RAISING   ycx_agco_globcore_exception.
    METHODS: get_vals_from_selscreen.
    METHODS: change_sel_screen.
    METHODS: get_nr_of_combs RAISING ycx_agco_globcore_exception.
    METHODS: set_values IMPORTING iv_restrict_comb_ids TYPE i.
    METHODS: pbo0100,
      pai0100 IMPORTING iv_ok_code TYPE sy-ucomm,
      pbo1100,
      pai1100 IMPORTING iv_ok_code TYPE sy-ucomm,
      back IMPORTING iv_ok_code TYPE sy-ucomm.
  PRIVATE SECTION.

    CLASS-DATA: go_instance TYPE REF TO lcl_cu60_display.
    DATA: mv_vartab            TYPE vtnam,
          mv_restrict_comb_ids TYPE i.
    DATA: mo_cu60 TYPE REF TO yptpi5324cl_cu60.
    DATA: ms_exclude_options TYPE rsoptions.
    DATA: mt_mltpl_vals_for_chars TYPE yptpi5324tt_vals_for_chars.
    DATA: mr_variant_tab TYPE REF TO data.
    DATA: mv_nr_of_combinations TYPE i.
    DATA: mo_dock     TYPE REF TO cl_gui_docking_container,
          mo_cont     TYPE REF TO cl_gui_container,
          mo_alv      TYPE REF TO cl_gui_alv_grid,
          mt_fieldcat TYPE lvc_t_fcat.

    METHODS: set_selcrit_tab RAISING   ycx_agco_globcore_exception.
    METHODS: process RAISING ycx_agco_globcore_exception.

    METHODS: call_screen_0100.
    METHODS: multiple_selection RAISING   ycx_agco_globcore_exception.
    METHODS: input_help RAISING   ycx_agco_globcore_exception.
    METHODS: call_se16n IMPORTING iv_tab_name TYPE tabname16.
    METHODS: fcat_from_itab IMPORTING it_itab        TYPE ANY TABLE
                            RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat
                            RAISING   ycx_agco_globcore_exception.
    METHODS: icon_create IMPORTING iv_icon TYPE iconname
                         CHANGING  cv_btn  TYPE char50.
    METHODS: display_alv CHANGING ct_vartab TYPE ANY TABLE.
    METHODS: display_dock_alv CHANGING ct_vartab TYPE ANY TABLE.

ENDCLASS.

CLASS lcl_cu60_display IMPLEMENTATION.
  METHOD get_inst.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_inst = go_instance.
  ENDMETHOD.

  METHOD constructor.
    ms_exclude_options = VALUE #( bt = 'X'
                                  cp = 'X'
                                  eq = ''
                                  ge = 'X'
                                  gt = 'X'
                                  le = 'X'
                                  lt = 'X'
                                  nb = 'X'
                                  ne = 'X'
                                  np = 'X' ).
  ENDMETHOD.

  METHOD pbo0100.
    TRY.
        gv_nr_of_combinations = mv_nr_of_combinations.

        FIELD-SYMBOLS: <lt_variant_tab> TYPE STANDARD TABLE.
        ASSIGN mr_variant_tab->* TO <lt_variant_tab>.
        mt_fieldcat = me->fcat_from_itab( <lt_variant_tab> ).

*        me->display_alv( CHANGING ct_vartab = <lt_variant_tab> ).
        me->display_dock_alv( CHANGING ct_vartab = <lt_variant_tab> ).


      CATCH ycx_agco_globcore_exception INTO DATA(lx_agco).
        MESSAGE ID lx_agco->msgid TYPE lx_agco->msgty NUMBER lx_agco->msgno
                  WITH lx_agco->msgv1 lx_agco->msgv2 lx_agco->msgv3 lx_agco->msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD display_alv.

    IF mo_alv IS NOT BOUND.
      mo_alv = NEW #( i_parent = cl_gui_custom_container=>default_screen
                      i_name = CONV string( 'Variant Table Display'(001) ) ).

      mo_alv->set_table_for_first_display( CHANGING it_outtab = ct_vartab
                                                    it_fieldcatalog = mt_fieldcat
                                           EXCEPTIONS invalid_parameter_combination = 1
                                                      program_error                 = 2
                                                      too_many_lines                = 3
                                                      OTHERS                        = 4 ).
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      mo_alv->set_gridtitle( i_gridtitle = |{ 'Variant Table Display'(001) }: { mv_vartab } | ).
    ENDIF.
  ENDMETHOD.

  METHOD display_dock_alv.
    IF mo_dock IS NOT BOUND.
      mo_dock = NEW #( dynnr = '0100'
                       side = cl_gui_docking_container=>dock_at_bottom
                       ratio = 90
                       name  = 'DOCK_CONT' ).
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
    IF mo_alv IS NOT BOUND.
      CLEAR mo_cont.
      mo_cont ?= mo_dock.
      mo_alv = NEW #( i_parent = mo_cont
                      i_name = CONV string( 'Variant Table Display'(001) ) ).

      mo_alv->set_table_for_first_display( CHANGING it_outtab = ct_vartab
                                                    it_fieldcatalog = mt_fieldcat
                                           EXCEPTIONS invalid_parameter_combination = 1
                                                      program_error                 = 2
                                                      too_many_lines                = 3
                                                      OTHERS                        = 4 ).
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      mo_alv->set_gridtitle( i_gridtitle = |{ 'Variant Table Display'(001) }: { mv_vartab } | ).
    ENDIF.
  ENDMETHOD.

  METHOD pbo1100.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GR1'.
        IF gv_tabname IS NOT INITIAL.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD pai0100.
    CASE iv_ok_code.
      WHEN 'BACK' OR 'EXIT' OR 'STOP'.
        IF mo_alv IS BOUND.
          mo_alv->free( ).
          CLEAR mo_alv.
        ENDIF.
        cl_gui_cfw=>flush( ).
        LEAVE TO SCREEN 0.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD pai1100.
    TRY.
        CASE iv_ok_code.
          WHEN 'MORE'.
            me->multiple_selection( ).
          WHEN 'INHLP'.
            me->input_help( ).
          WHEN 'EXEC'.
            me->process( ).
        ENDCASE.
      CATCH ycx_agco_globcore_exception INTO DATA(lx_agco).
        IF lx_agco->msgty EQ 'E'.
          MESSAGE ID lx_agco->msgid TYPE 'S' NUMBER lx_agco->msgno
                    WITH lx_agco->msgv1 lx_agco->msgv2 lx_agco->msgv3 lx_agco->msgv4 DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ELSE.
          MESSAGE ID lx_agco->msgid TYPE lx_agco->msgty NUMBER lx_agco->msgno
                    WITH lx_agco->msgv1 lx_agco->msgv2 lx_agco->msgv3 lx_agco->msgv4.
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD back.
    CASE iv_ok_code.
      WHEN 'BACK' OR 'EXIT' OR 'STOP'.
        LEAVE TO SCREEN 0.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD call_screen_0100.
    CALL SCREEN '0100'.
  ENDMETHOD.

  METHOD call_screen_1100.
    CALL SCREEN '1100'.
  ENDMETHOD.

  METHOD call_se16n.
    DATA(lt_bdcdata) = VALUE bdcdata_tab( ( program = 'SAPLSE16N' dynpro = '0100' dynbegin = 'X' )
                                          ( fnam = 'BDC_CURSOR' fval = 'GD-TAB' )
                                          ( fnam = 'GD-TAB' fval = iv_tab_name )
                                          ( fnam = 'BDC_OKCODE' fval = '=EXEC' ) ).
    DATA(ls_opt) = VALUE ctu_params( dismode = 'E' ).
    TRY.
        CALL TRANSACTION 'SE16N' WITH AUTHORITY-CHECK
                                 USING lt_bdcdata OPTIONS FROM ls_opt.
      CATCH cx_sy_authorization_error.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.
  ENDMETHOD.

  METHOD process.
    mr_variant_tab = mo_cu60->get_transposed_vartab_vals( it_vals_for_chars = mt_mltpl_vals_for_chars
                                                          iv_restrict_comb_ids = mv_restrict_comb_ids ).
    mv_nr_of_combinations = mo_cu60->get_nr_of_slnid( mt_mltpl_vals_for_chars ).
    me->call_screen_0100( ).
  ENDMETHOD.

  METHOD get_nr_of_combs.
    TRY.
        gv_nr_of_combinations = mo_cu60->get_nr_of_slnid( mt_mltpl_vals_for_chars ).
      CATCH ycx_agco_globcore_exception INTO DATA(lx_agco).
        MESSAGE ID lx_agco->msgid TYPE lx_agco->msgty NUMBER lx_agco->msgno
                      WITH lx_agco->msgv1 lx_agco->msgv2 lx_agco->msgv3 lx_agco->msgv4.
    ENDTRY.
  ENDMETHOD.

  METHOD set_values.
    mv_restrict_comb_ids = iv_restrict_comb_ids.
  ENDMETHOD.

  METHOD fcat_from_itab.
    DATA: lt_table TYPE REF TO data.
    CREATE DATA lt_table LIKE it_itab.
    ASSIGN lt_table->* TO FIELD-SYMBOL(<ls_table>).
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv_table)
                                CHANGING t_table = <ls_table>  ).
        rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = salv_table->get_columns( )
                                                                     r_aggregations = salv_table->get_aggregations( ) ).
        LOOP AT rt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
          IF <ls_fcat>-fieldname EQ 'SLNID'.
            <ls_fcat>-coltext = 'ID'.
            <ls_fcat>-scrtext_l = 'ID'.
            <ls_fcat>-scrtext_m = 'ID'.
            <ls_fcat>-scrtext_s = 'ID'.
          ELSE.
            <ls_fcat>-coltext = <ls_fcat>-fieldname.
            <ls_fcat>-scrtext_l = <ls_fcat>-fieldname.
            <ls_fcat>-scrtext_m = <ls_fcat>-fieldname.
            <ls_fcat>-scrtext_s = <ls_fcat>-fieldname.
            <ls_fcat>-inttype = 'F'.
            <ls_fcat>-decimals = 3.
            <ls_fcat>-decimals_o = 3.
            <ls_fcat>-exponent = 0.
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        RAISE EXCEPTION TYPE ycx_agco_globcore_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD startup_vartab_proc.
    TRY.
        IF iv_vartab NE mv_vartab.

          IF mo_cu60 IS BOUND.
            CLEAR mo_cu60.
            CLEAR gs_selfields_wa.
            CLEAR gt_selfields[].
            CLEAR gs_selfields.
            CLEAR mt_mltpl_vals_for_chars.
          ENDIF.
          mv_vartab = iv_vartab.
          mo_cu60 = NEW #( iv_vartab = mv_vartab ).
          DATA(lv_tab_name) = mo_cu60->vartab_vs_dbtab( ).
          IF lv_tab_name NE ''.
            me->call_se16n( lv_tab_name ).
          ENDIF.
          me->set_selcrit_tab( ).
          CLEAR gv_nr_of_combinations.
        ENDIF.
      CATCH ycx_agco_globcore_exception INTO DATA(lx_agco).
        MESSAGE ID lx_agco->msgid TYPE lx_agco->msgty NUMBER lx_agco->msgno
                      WITH lx_agco->msgv1 lx_agco->msgv2 lx_agco->msgv3 lx_agco->msgv4.
    ENDTRY.
  ENDMETHOD.

  METHOD set_selcrit_tab.
    mt_mltpl_vals_for_chars = CORRESPONDING #( mo_cu60->get_chars_for_selscreen( ) ).
    LOOP AT mt_mltpl_vals_for_chars ASSIGNING FIELD-SYMBOL(<ls_vals>).
      <ls_vals>-active = 'X'.
    ENDLOOP.

    gt_selfields[] = VALUE #( FOR ls_chars_in_vartab IN mt_mltpl_vals_for_chars ( atinn = ls_chars_in_vartab-atinn
                                                                                fieldname = |{ ls_chars_in_vartab-atbez CASE = UPPER }|
                                                                                active = 'X'
                                                                                 ) ).
  ENDMETHOD.

  METHOD get_vals_from_selscreen.
    ASSIGN mt_mltpl_vals_for_chars[ atinn = gs_selfields_wa-atinn ] TO FIELD-SYMBOL(<ls_char_vals>).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    <ls_char_vals>-active = gs_selfields-active.
    "Handle the Value field, and the multiple value internal table
    IF <ls_char_vals>-multpl_vals IS NOT INITIAL.
      ASSIGN <ls_char_vals>-multpl_vals[ 1 ] TO FIELD-SYMBOL(<ls_mltpl_val_line>).
      <ls_mltpl_val_line>-low = gs_selfields-value.
      IF gs_selfields-value IS INITIAL.
        DELETE <ls_char_vals>-multpl_vals INDEX 1.
        READ TABLE <ls_char_vals>-multpl_vals INDEX 1 INTO DATA(ls_val).
        IF sy-subrc EQ 0.
          gs_selfields-value = ls_val-low.
        ENDIF.
      ENDIF.
    ELSEIF gs_selfields-value IS NOT INITIAL.
      APPEND VALUE #( sign = 'I'
                      option = 'EQ'
                      low = gs_selfields-value ) TO <ls_char_vals>-multpl_vals.
    ENDIF.

    IF <ls_char_vals>-multpl_vals IS NOT INITIAL.
      gs_selfields-push = abap_true.
    ELSE.
      gs_selfields-push = abap_false.
    ENDIF.

    "Push back the data to the internal table of the Table Control
    ASSIGN gt_selfields[ selfields_tc-current_line ] TO FIELD-SYMBOL(<ls_selfield>).
    <ls_selfield>-value = gs_selfields-value.
    <ls_selfield>-active = gs_selfields-active.
    <ls_selfield>-push = gs_selfields-push.
  ENDMETHOD.

  METHOD multiple_selection.
    DATA: lv_line TYPE sy-tabix.
    GET CURSOR LINE lv_line.
    TRY.
        ASSIGN gt_selfields[ lv_line ] TO FIELD-SYMBOL(<ls_selfields>).
        ASSIGN mt_mltpl_vals_for_chars[ lv_line ] TO FIELD-SYMBOL(<ls_char_vals>).
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
        mo_cu60->multiple_selection( EXPORTING iv_title = CONV syst_title( <ls_char_vals>-atbez )
                                               is_excluded_options = ms_exclude_options
                                     CHANGING ct_range = <ls_char_vals>-multpl_vals ) .
        IF <ls_char_vals>-multpl_vals IS NOT INITIAL.
          <ls_selfields>-push = abap_true.
          <ls_selfields>-value = <ls_char_vals>-multpl_vals[ 1 ]-low.
        ELSE.
          CLEAR <ls_selfields>-value.
          <ls_selfields>-push = abap_false.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD input_help.
    DATA: lv_line TYPE sy-tabix.
    GET CURSOR LINE lv_line.
    TRY.
        ASSIGN gt_selfields[ lv_line ] TO FIELD-SYMBOL(<ls_selfields>).
        ASSIGN mt_mltpl_vals_for_chars[ lv_line ] TO FIELD-SYMBOL(<ls_char_vals>).
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
        DATA(ls_possible_values) = mo_cu60->get_possible_charvals( <ls_char_vals>-atinn ).

        DATA(lv_canceled) = mo_cu60->multiple_selection( EXPORTING iv_title = 'Possible Values'(002)
                                                                   is_excluded_options = ms_exclude_options
                                                         CHANGING  ct_range = ls_possible_values-multpl_vals ).

        IF lv_canceled EQ abap_false.
          <ls_char_vals>-multpl_vals = ls_possible_values-multpl_vals.
        ENDIF.

        IF <ls_char_vals>-multpl_vals IS NOT INITIAL.
          <ls_selfields>-push = abap_true.
          <ls_selfields>-value = <ls_char_vals>-multpl_vals[ 1 ]-low.
        ELSE.
          CLEAR <ls_selfields>-value.
          <ls_selfields>-push = abap_false.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD change_sel_screen.
    READ TABLE gt_selfields INTO DATA(ls_selfields) INDEX selfields_tc-current_line.
    IF ls_selfields-push EQ abap_true.
      me->icon_create( EXPORTING iv_icon = 'ICON_DISPLAY_MORE'
                       CHANGING  cv_btn  = more_btn ).
    ELSE.
      me->icon_create( EXPORTING iv_icon = 'ICON_ENTER_MORE'
                       CHANGING  cv_btn  = more_btn ).
    ENDIF.

    IF selfields_tc-current_line > gv_linecount.
      LOOP AT SCREEN.
        screen-input = 0.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD icon_create.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = iv_icon
      IMPORTING
        result                = cv_btn
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0.
      CLEAR cv_btn.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  lcl_cu60_display=>get_inst( )->call_screen_1100( ).


  INCLUDE yptpi5324r_cu60_displayo01.
  INCLUDE yptpi5324r_cu60_displayi01.
