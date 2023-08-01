INTERFACE yptpi5324i_cu60_dao
  PUBLIC .
  METHODS sel_vartab IMPORTING iv_vartab        TYPE vtnam
                     RETURNING VALUE(rs_vartab) TYPE cuvtab
                     RAISING   ycx_agco_globcore_exception.
  METHODS sel_chars_for_char_names IMPORTING it_char_name_rng TYPE yvc_tt_range_atnam
                                   RETURNING VALUE(rt_chars)  TYPE yptpi5324tt_chars
                                   RAISING   ycx_agco_globcore_exception.
  METHODS sel_chars_for_char_ids IMPORTING it_char_id_rng  TYPE yptpi5324tt_atinn_rng
                                 RETURNING VALUE(rt_chars) TYPE yptpi5324tt_chars.
  METHODS sel_vals_for_vartab IMPORTING iv_vartab_id              TYPE vtint
                                        it_char_id_rng            TYPE yptpi5324tt_atinn_rng OPTIONAL
                              RETURNING VALUE(rt_vals_for_vartab) TYPE  yptpi5324tt_cuvtab_valc
                              RAISING   ycx_agco_globcore_exception.
  methods sel_numvals_for_vartab importing iv_vartab_id type vtint
                                           it_char_id_rng            TYPE yptpi5324tt_atinn_rng OPTIONAL
                                 returning value(rt_numvals_for_vartab) type yptpi5324tt_cuvtab_valn
                                 raising ycx_agco_globcore_exception.
  METHODS complex_selections_dialog IMPORTING iv_title            TYPE sy-title
                                              is_excluded_options TYPE rsoptions OPTIONAL
                                    CHANGING  ct_range            TYPE ysmkr5304tt_atwrt
                                    RETURNING VALUE(rv_canceled)  TYPE abap_bool
                                    RAISING   ycx_agco_globcore_exception.


ENDINTERFACE.
