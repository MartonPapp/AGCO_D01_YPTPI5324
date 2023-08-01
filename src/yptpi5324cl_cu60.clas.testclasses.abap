*"* use this source file for your ABAP unit test classes
CLASS lcl_dao_injector DEFINITION DEFERRED.

CLASS ltd_cu60_dao DEFINITION FOR TESTING FRIENDS lcl_dao_injector.
  PUBLIC SECTION.
    INTERFACES yptpi5324i_cu60_dao PARTIALLY IMPLEMENTED.
  PRIVATE SECTION.
    CLASS-DATA: mt_cabn TYPE yptpi5324tt_chars.
    CLASS-DATA: mt_cuvtab TYPE TABLE OF cuvtab.
    CLASS-DATA: mt_cuvtab_valc TYPE TABLE OF cuvtab_valc.
    CLASS-DATA: mt_cuvtab_valn TYPE TABLE OF cuvtab_valn.
ENDCLASS.

CLASS ltd_cu60_dao IMPLEMENTATION.
  METHOD yptpi5324i_cu60_dao~sel_vartab.
    rs_vartab = mt_cuvtab[ vtnam = iv_vartab ].
  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~sel_chars_for_char_names.
    rt_chars = VALUE #( FOR ls_cabn IN mt_cabn WHERE ( atnam IN it_char_name_rng ) ( ls_cabn ) ).
  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~sel_vals_for_vartab.
    rt_vals_for_vartab = VALUE #( FOR ls_cuvtab_valc IN mt_cuvtab_valc WHERE ( vtint EQ iv_vartab_id
                                                                           AND atinn IN it_char_id_rng ) ( ls_cuvtab_valc ) ).
  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~sel_numvals_for_vartab.
    rt_numvals_for_vartab = VALUE #( FOR ls_cuvtab_valn IN mt_cuvtab_valn WHERE ( vtint EQ iv_vartab_id
                                                                            AND   atinn IN it_char_id_rng ) ( ls_cuvtab_valn ) ).
  ENDMETHOD.

  METHOD yptpi5324i_cu60_dao~sel_chars_for_char_ids.
    rt_chars = VALUE #( FOR ls_cabn IN mt_cabn WHERE ( atinn IN it_char_id_rng ) ( ls_cabn ) ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dao_injector DEFINITION FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS set_cabn IMPORTING it_cabn TYPE yptpi5324tt_chars.
    CLASS-METHODS set_cuvtab IMPORTING it_cuvtab TYPE expo_cuvtab_t.
    CLASS-METHODS set_cuvtab_valc IMPORTING it_cuvtab_valc TYPE ysmke10084tt_cuvtab_valc.
    CLASS-METHODS set_cuvtab_valn
      IMPORTING
        it_cuvtab_valn TYPE yptpi5324tt_cuvtab_valn.
ENDCLASS.

CLASS lcl_dao_injector IMPLEMENTATION.
  METHOD set_cabn.
    ltd_cu60_dao=>mt_cabn = it_cabn.
  ENDMETHOD.

  METHOD set_cuvtab.
    ltd_cu60_dao=>mt_cuvtab = it_cuvtab.
  ENDMETHOD.

  METHOD set_cuvtab_valc.
    ltd_cu60_dao=>mt_cuvtab_valc = it_cuvtab_valc.
  ENDMETHOD.


  METHOD set_cuvtab_valn.
    ltd_cu60_dao=>mt_cuvtab_valn = it_cuvtab_valn.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_cu60 DEFINITION FOR TESTING
    RISK LEVEL HARMLESS
    DURATION SHORT.
  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO yptpi5324cl_cu60.

    METHODS setup.
    METHODS assert_data CHANGING ct_act TYPE STANDARD  TABLE
                                 ct_exp TYPE STANDARD  TABLE.


    METHODS ok_for_1_val_per_char_2_chars FOR TESTING.
    METHODS ok_mltpl_vals_per_char_2chars FOR TESTING.
    METHODS allcharsact_1charvalfltrd FOR TESTING.
    METHODS allcharsact_1charvalfltrd_2 FOR TESTING.
    METHODS charsfltrd_allvals FOR TESTING.
    METHODS charsfltrd_valsfltrd FOR TESTING.
    METHODS charsfltrd_valsfltrd_2 FOR TESTING.
    METHODS not_valid_char_val FOR TESTING.
    METHODS excl_single FOR TESTING.
    METHODS excl_mltpl_single FOR TESTING.
    METHODS excl_mltpl_single_2 FOR TESTING.
    METHODS excl_mltpl_from_sameid FOR TESTING.

    METHODS ok_for_valn_and_valc FOR TESTING.
    METHODS ok_for_fltrdvaln_and_valc FOR TESTING.

    METHODS nr_of_slnid FOR TESTING.

    methods restrict_nr_of_combids for testing.
    methods rstrct_nr_of_ids_rstr_vals for testing.
ENDCLASS.

CLASS lcl_cu60 IMPLEMENTATION.
  METHOD setup.
    "given
    yptpi5324cl_dao_factory=>set_cu60_dao_instance( NEW ltd_cu60_dao( ) ).
  ENDMETHOD.

  METHOD assert_data.
    cl_abap_unit_assert=>assert_equals( act = ct_act
                                        exp = ct_exp ).
  ENDMETHOD.

  METHOD ok_for_1_val_per_char_2_chars.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE '1_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) ) ).

    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                              ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = '1_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                            ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' ) ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_exp) = VALUE tty_2chars( ( slnid = '1'
                                       test_cstic_1 = 'Y'
                                       test_hunbak_model = '02' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.



  METHOD ok_mltpl_vals_per_char_2chars.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) ) ).

    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                   ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_exp) = VALUE tty_2chars( ( slnid = '1'
                                       test_cstic_1 = 'Y'
                                       test_hunbak_model = '02' )
                                      ( slnid = '1'
                                       test_cstic_1 = ''
                                       test_hunbak_model = '04' )
                                      ( slnid = '1'
                                       test_cstic_1 = ''
                                       test_hunbak_model = '05' )
                                      ( slnid = '2'
                                       test_cstic_1 = 'Y'
                                       test_hunbak_model = '00' )
                                      ( slnid = '2'
                                       test_cstic_1 = 'Z'
                                       test_hunbak_model = '' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD allcharsact_1charvalfltrd.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ( sign = 'I'
                                                                                          option = 'EQ'
                                                                                          low = 'Z' ) ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                   ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_exp) = VALUE tty_2chars( ( slnid = '2'
                                       test_cstic_1 = 'Y'
                                       test_hunbak_model = '00' )
                                    ( slnid = '2'
                                      test_cstic_1 = 'Z'
                                      test_hunbak_model = '' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD allcharsact_1charvalfltrd_2.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #( ( sign = 'I'
                                                                                          option = 'EQ'
                                                                                          low = '02' ) ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #(  ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                   ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_exp) = VALUE tty_2chars( ( slnid = '1'
                                       test_cstic_1 = 'Y'
                                       test_hunbak_model = '02' )
                                     ( slnid = '1'
                                       test_cstic_1 = ''
                                       test_hunbak_model = '04' )
                                     ( slnid = '1'
                                       test_cstic_1 = ''
                                       test_hunbak_model = '05' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD charsfltrd_allvals.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = ''
                                                                 multpl_vals = VALUE #(  ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                   ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_exp) = VALUE tty_1char( ( slnid = '1'
                                      test_hunbak_model = '02' )
                                    ( slnid = '1'
                                      test_hunbak_model = '04' )
                                    ( slnid = '1'
                                      test_hunbak_model = '05' )
                                    ( slnid = '2'
                                      test_hunbak_model = '00' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD charsfltrd_valsfltrd.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = ''
                                                                 multpl_vals = VALUE #( ( sign = 'I'
                                                                                          option = 'EQ'
                                                                                          low = 'Z' ) ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                   ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_exp) = VALUE tty_1char( ( slnid = '2'
                                      test_hunbak_model = '00' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD charsfltrd_valsfltrd_2.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = ''
                                                                  multpl_vals = VALUE #( ( sign = 'I'
                                                                                          option = 'EQ'
                                                                                          low = '02' ) ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #(  ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                             ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_exp) = VALUE tty_1char_cstics( ( slnid = '1'
                                             test_cstic_1 = 'Y' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD not_valid_char_val.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = ''
                                                                 multpl_vals = VALUE #( ( sign = 'I'
                                                                                          option = 'EQ'
                                                                                          low = 'X' ) ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                   ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS'  ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA: lv_exp_msg TYPE string VALUE 'Not valid characteristic values for variant table'.

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception INTO DATA(lx_agco).
        DATA(lv_msg) = lx_agco->get_text( ).
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_equals( act = lv_msg
                                        exp = lv_exp_msg ).
  ENDMETHOD.

  METHOD ok_for_valn_and_valc.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'CR18P_WEIGHT'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029350'
                                                                  atnam = 'YMFG_EFFECTIVITY_DATE'
                                                                  atfor = 'DATE'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000029353'
                                                                 atnam = 'CR18P_VCGROUPS'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) )
                                                               ( atinn = '0000029354'
                                                                 atnam = 'CR18PVCS_REARWEIGHT'
                                                                 atfor = 'NUM'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) ) ).

    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029350' atnam = 'YMFG_EFFECTIVITY_DATE' atfor = 'DATE' )
                                             ( atinn = '0000029353' atnam = 'CR18P_VCGROUPS' atfor = 'CHAR' )
                                             ( atinn = '0000029354' atnam = 'CR18PVCS_REARWEIGHT' atfor = 'NUM' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '4182' vtnam = 'CR18P_WEIGHT' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '4182' slnid = '1' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '2' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '3' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B02' )
                                                           ( vtint = '4182' slnid = '4' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A01' )
                                                           ( vtint = '4182' slnid = '5' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B01' ) ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_cuvtab_valn) = VALUE yptpi5324tt_cuvtab_valn( ( vtint = '4182' slnid = '1' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '1' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '44.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '20221231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '34.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '124.86199999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '213.57499999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '111.50500000000000' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' ) ).
    lcl_dao_injector=>set_cuvtab_valn( lt_cuvtab_valn ).

    DATA(lt_exp) = VALUE tty_3chars( ( slnid = '1'
                                       cr18p_vcgroups = 'CR22PC510A02'
                                       ymfg_effectivity_date = '01.01.2023 - 31.12.9999'
                                       cr18pvcs_rearweight = '44.876'
                                        )
                                      ( slnid = '2'
                                       cr18p_vcgroups = 'CR22PC510A02'
                                       ymfg_effectivity_date = '01.01.2000 - 31.12.2022'
                                       cr18pvcs_rearweight = '34.876'
                                       )
                                      ( slnid = '3'
                                       cr18p_vcgroups = 'CR22PC463B02'
                                       ymfg_effectivity_date = '01.01.2023 - 31.12.9999'
                                       cr18pvcs_rearweight = '124.862' )
                                      ( slnid = '4'
                                       cr18p_vcgroups = 'CR22PC510A01'
                                       ymfg_effectivity_date = '01.01.2023 - 31.12.9999'
                                       cr18pvcs_rearweight = '213.575' )
                                      ( slnid = '5'
                                       cr18p_vcgroups = 'CR22PC463B01'
                                       ymfg_effectivity_date = '01.01.2000 - 31.12.9999'
                                       cr18pvcs_rearweight = '111.505' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD ok_for_fltrdvaln_and_valc.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'CR18P_WEIGHT'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029350'
                                                                  atnam = 'YMFG_EFFECTIVITY_DATE'
                                                                  atfor = 'DATE'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #( ( sign = 'I'
                                                                                          option = 'EQ'
                                                                                          low = '01.01.2000 - 31.12.2022' ) ) )
                                                               ( atinn = '0000029353'
                                                                 atnam = 'CR18P_VCGROUPS'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) )
                                                               ( atinn = '0000029354'
                                                                 atnam = 'CR18PVCS_REARWEIGHT'
                                                                 atfor = 'NUM'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) ) ).

    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029350' atnam = 'YMFG_EFFECTIVITY_DATE' atfor = 'DATE' )
                                             ( atinn = '0000029353' atnam = 'CR18P_VCGROUPS' atfor = 'CHAR' )
                                             ( atinn = '0000029354' atnam = 'CR18PVCS_REARWEIGHT' atfor = 'NUM' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '4182' vtnam = 'CR18P_WEIGHT' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '4182' slnid = '1' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '2' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '3' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B02' )
                                                           ( vtint = '4182' slnid = '4' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A01' )
                                                           ( vtint = '4182' slnid = '5' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B01' ) ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_cuvtab_valn) = VALUE yptpi5324tt_cuvtab_valn( ( vtint = '4182' slnid = '1' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '1' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '44.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '20221231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '34.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '124.86199999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '213.57499999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '111.50500000000000' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' ) ).
    lcl_dao_injector=>set_cuvtab_valn( lt_cuvtab_valn ).

    DATA(lt_exp) = VALUE tty_3chars( ( slnid = '2'
                                       cr18p_vcgroups = 'CR22PC510A02'
                                       ymfg_effectivity_date = '01.01.2000 - 31.12.2022'
                                       cr18pvcs_rearweight = '34.876'
                                       ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD excl_single.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #( ( sign = 'E'
                                                                                          option = 'EQ'
                                                                                          low = '02' ) ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #(  ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                             ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_exp) = VALUE tty_2chars( ( slnid = '2'
                                       test_cstic_1 = 'Y'
                                       test_hunbak_model = '00' )
                                      ( slnid = '2'
                                        test_cstic_1 = 'Z'
                                        test_hunbak_model = '' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD excl_mltpl_single.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'CR18P_WEIGHT'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029350'
                                                                  atnam = 'YMFG_EFFECTIVITY_DATE'
                                                                  atfor = 'DATE'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #( ( sign = 'E'
                                                                                          option = 'EQ'
                                                                                          low = '01.01.2023 - 31.12.9999' ) ) )
                                                               ( atinn = '0000029353'
                                                                 atnam = 'CR18P_VCGROUPS'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ( sign = 'E'
                                                                                          option = 'EQ'
                                                                                          low = 'CR22PC510A02' ) ) )
                                                               ( atinn = '0000029354'
                                                                 atnam = 'CR18PVCS_REARWEIGHT'
                                                                 atfor = 'NUM'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ( sign = 'I'
                                                                                          option = 'EQ'
                                                                                          low = '111.505' ) ) ) ).

    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029350' atnam = 'YMFG_EFFECTIVITY_DATE' atfor = 'DATE' )
                                             ( atinn = '0000029353' atnam = 'CR18P_VCGROUPS' atfor = 'CHAR' )
                                             ( atinn = '0000029354' atnam = 'CR18PVCS_REARWEIGHT' atfor = 'NUM' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '4182' vtnam = 'CR18P_WEIGHT' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '4182' slnid = '1' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '2' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '3' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B02' )
                                                           ( vtint = '4182' slnid = '4' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A01' )
                                                           ( vtint = '4182' slnid = '5' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B01' ) ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_cuvtab_valn) = VALUE yptpi5324tt_cuvtab_valn( ( vtint = '4182' slnid = '1' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '1' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '44.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '20221231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '34.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '124.86199999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '213.57499999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '111.50500000000000' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' ) ).
    lcl_dao_injector=>set_cuvtab_valn( lt_cuvtab_valn ).

    DATA(lt_exp) = VALUE tty_3chars( ( slnid = '5'
                                       cr18p_vcgroups = 'CR22PC463B01'
                                       ymfg_effectivity_date = '01.01.2000 - 31.12.9999'
                                       cr18pvcs_rearweight = '111.505'
                                       ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD excl_mltpl_single_2.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #( ( sign = 'E'
                                                                                          option = 'EQ'
                                                                                          low = '02' ) ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ( sign = 'E'
                                                                                          option = 'EQ'
                                                                                          low = 'Y' ) ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                             ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA: lv_exp_msg TYPE string VALUE 'Result is empty.'.

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception INTO DATA(lx_agco).
        DATA(lv_msg) = lx_agco->get_text( ).
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_equals( act = lv_msg
                                        exp = lv_exp_msg ).
  ENDMETHOD.

  METHOD excl_mltpl_from_sameid.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'CR18P_WEIGHT'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029350'
                                                                  atnam = 'YMFG_EFFECTIVITY_DATE'
                                                                  atfor = 'DATE'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #( ( sign = 'E'
                                                                                          option = 'EQ'
                                                                                          low = '01.01.2023 - 31.12.9999' ) ) )
                                                               ( atinn = '0000029353'
                                                                 atnam = 'CR18P_VCGROUPS'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ( sign = 'E'
                                                                                          option = 'EQ'
                                                                                          low = 'CR22PC510A02' ) ) )
                                                               ( atinn = '0000029354'
                                                                 atnam = 'CR18PVCS_REARWEIGHT'
                                                                 atfor = 'NUM'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #(  ) ) ).

    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029350' atnam = 'YMFG_EFFECTIVITY_DATE' atfor = 'DATE' )
                                             ( atinn = '0000029353' atnam = 'CR18P_VCGROUPS' atfor = 'CHAR' )
                                             ( atinn = '0000029354' atnam = 'CR18PVCS_REARWEIGHT' atfor = 'NUM' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '4182' vtnam = 'CR18P_WEIGHT' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '4182' slnid = '1' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '2' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '3' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B02' )
                                                           ( vtint = '4182' slnid = '4' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A01' )
                                                           ( vtint = '4182' slnid = '5' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B01' ) ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_cuvtab_valn) = VALUE yptpi5324tt_cuvtab_valn( ( vtint = '4182' slnid = '1' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '1' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '44.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '20221231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '34.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '124.86199999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '213.57499999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '111.50500000000000' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' ) ).
    lcl_dao_injector=>set_cuvtab_valn( lt_cuvtab_valn ).

    DATA(lt_exp) = VALUE tty_3chars( ( slnid = '5'
                                       cr18p_vcgroups = 'CR22PC463B01'
                                       ymfg_effectivity_date = '01.01.2000 - 31.12.9999'
                                       cr18pvcs_rearweight = '111.505'
                                       ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( lt_vals_for_chars ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD nr_of_slnid.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'MLTPL_VAL_2_CHARS'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029463'
                                                                  atnam = 'TEST_HUNBAK_MODEL'
                                                                  atfor = 'CHAR'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000012716'
                                                                 atnam = 'TEST_CSTIC_1'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #(  ) ) ).
    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029463' atnam = 'TEST_HUNBAK_MODEL' atfor = 'CHAR' )
                                   ( atinn = '0000012716' atnam = 'TEST_CSTIC_1' atfor = 'CHAR' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '1010' vtnam = 'MLTPL_VAL_2_CHARS' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '1010' slnid = '1' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '1' valc = '02' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '2' valc = '04' )
                                                           ( vtint = '1010' slnid = '1' atinn = '0000029463' vlcnt = '3' valc = '05' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '1' valc = 'Y' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000012716' vlcnt = '2' valc = 'Z' )
                                                           ( vtint = '1010' slnid = '2' atinn = '0000029463' vlcnt = '1' valc = '00' )
                                                             ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA: lv_exp TYPE i VALUE 2.

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lv_act) = mo_cut->get_nr_of_slnid( it_vals_for_chars = lt_vals_for_chars ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    cl_abap_unit_assert=>assert_equals( act = lv_act
                                        exp = lv_exp ).
  ENDMETHOD.

  METHOD restrict_nr_of_combids.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'CR18P_WEIGHT'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029350'
                                                                  atnam = 'YMFG_EFFECTIVITY_DATE'
                                                                  atfor = 'DATE'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #(  ) )
                                                               ( atinn = '0000029353'
                                                                 atnam = 'CR18P_VCGROUPS'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) )
                                                               ( atinn = '0000029354'
                                                                 atnam = 'CR18PVCS_REARWEIGHT'
                                                                 atfor = 'NUM'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) ) ).

    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029350' atnam = 'YMFG_EFFECTIVITY_DATE' atfor = 'DATE' )
                                             ( atinn = '0000029353' atnam = 'CR18P_VCGROUPS' atfor = 'CHAR' )
                                             ( atinn = '0000029354' atnam = 'CR18PVCS_REARWEIGHT' atfor = 'NUM' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '4182' vtnam = 'CR18P_WEIGHT' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '4182' slnid = '1' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '2' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '3' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B02' )
                                                           ( vtint = '4182' slnid = '4' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A01' )
                                                           ( vtint = '4182' slnid = '5' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B01' ) ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_cuvtab_valn) = VALUE yptpi5324tt_cuvtab_valn( ( vtint = '4182' slnid = '1' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '1' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '44.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '20221231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '34.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '124.86199999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '213.57499999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '111.50500000000000' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' ) ).
    lcl_dao_injector=>set_cuvtab_valn( lt_cuvtab_valn ).

    DATA(lt_exp) = VALUE tty_3chars( ( slnid = '1'
                                       cr18p_vcgroups = 'CR22PC510A02'
                                       ymfg_effectivity_date = '01.01.2023 - 31.12.9999'
                                       cr18pvcs_rearweight = '44.876'
                                        )
                                      ( slnid = '2'
                                       cr18p_vcgroups = 'CR22PC510A02'
                                       ymfg_effectivity_date = '01.01.2000 - 31.12.2022'
                                       cr18pvcs_rearweight = '34.876'
                                       )
                                      ( slnid = '3'
                                       cr18p_vcgroups = 'CR22PC463B02'
                                       ymfg_effectivity_date = '01.01.2023 - 31.12.9999'
                                       cr18pvcs_rearweight = '124.862' ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars
                                                           iv_restrict_comb_ids = 3  ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

  METHOD rstrct_nr_of_ids_rstr_vals.
    "given
    DATA: lv_vartab TYPE vtnam  VALUE 'CR18P_WEIGHT'.
    DATA(lt_vals_for_chars) = VALUE yptpi5324tt_vals_for_chars( ( atinn = '0000029350'
                                                                  atnam = 'YMFG_EFFECTIVITY_DATE'
                                                                  atfor = 'DATE'
                                                                  active = 'X'
                                                                  multpl_vals = VALUE #( ( sign = 'E'
                                                                                          option = 'EQ'
                                                                                          low = '01.01.2023 - 31.12.9999' ) ) )
                                                               ( atinn = '0000029353'
                                                                 atnam = 'CR18P_VCGROUPS'
                                                                 atfor = 'CHAR'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) )
                                                               ( atinn = '0000029354'
                                                                 atnam = 'CR18PVCS_REARWEIGHT'
                                                                 atfor = 'NUM'
                                                                 active = 'X'
                                                                 multpl_vals = VALUE #( ) ) ).

    DATA(lt_cabn) = VALUE yptpi5324tt_chars( ( atinn = '0000029350' atnam = 'YMFG_EFFECTIVITY_DATE' atfor = 'DATE' )
                                             ( atinn = '0000029353' atnam = 'CR18P_VCGROUPS' atfor = 'CHAR' )
                                             ( atinn = '0000029354' atnam = 'CR18PVCS_REARWEIGHT' atfor = 'NUM' ) ).
    lcl_dao_injector=>set_cabn( lt_cabn ).

    DATA(lt_cuvtab) = VALUE expo_cuvtab_t( ( vtint = '4182' vtnam = 'CR18P_WEIGHT' ) ).
    lcl_dao_injector=>set_cuvtab( lt_cuvtab ).

    DATA(lt_cuvtab_valc) = VALUE ysmke10084tt_cuvtab_valc( ( vtint = '4182' slnid = '1' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '2' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A02' )
                                                           ( vtint = '4182' slnid = '3' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B02' )
                                                           ( vtint = '4182' slnid = '4' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC510A01' )
                                                           ( vtint = '4182' slnid = '5' atinn = '0000029353' vlcnt = '1' valc = 'CR22PC463B01' ) ).
    lcl_dao_injector=>set_cuvtab_valc( lt_cuvtab_valc ).

    DATA(lt_cuvtab_valn) = VALUE yptpi5324tt_cuvtab_valn( ( vtint = '4182' slnid = '1' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '1' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '44.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '20221231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '2' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '34.875999999999998' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '3' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '124.86199999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20230101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '4' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '213.57499999999999' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029350' vlcnt = '1' val_from = CONV f( '20000101.000000000' ) val_to = CONV f( '99991231.000000000' ) val_code = '3' )
                                                          ( vtint = '4182' slnid = '5' atinn = '0000029354' vlcnt = '1' val_from = CONV f( '111.50500000000000' ) val_to = CONV f( '0.0000000000000000' ) val_code = '1' ) ).
    lcl_dao_injector=>set_cuvtab_valn( lt_cuvtab_valn ).

    DATA(lt_exp) = VALUE tty_3chars(
                                      ( slnid = '2'
                                       cr18p_vcgroups = 'CR22PC510A02'
                                       ymfg_effectivity_date = '01.01.2000 - 31.12.2022'
                                       cr18pvcs_rearweight = '34.876'
                                       ) ).

    FIELD-SYMBOLS: <ls_tab> TYPE STANDARD TABLE.

    TRY.
        mo_cut = NEW #( iv_vartab = lv_vartab ).
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Constructor failed for MO_CUT' TYPE 'E'.
    ENDTRY.
    "when
    TRY.
        DATA(lr_tab) = mo_cut->get_transposed_vartab_vals( it_vals_for_chars = lt_vals_for_chars
                                                           iv_restrict_comb_ids = 1  ).
        ASSIGN lr_tab->* TO <ls_tab>.
      CATCH ycx_agco_globcore_exception.
        MESSAGE 'Method under test failed' TYPE 'E'.
    ENDTRY.
    "then
    me->assert_data( CHANGING ct_exp = lt_exp
                              ct_act = <ls_tab> ).
  ENDMETHOD.

ENDCLASS.
