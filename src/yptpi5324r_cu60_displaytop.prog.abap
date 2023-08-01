*&---------------------------------------------------------------------*
*& Report  YPTPI5324R_CU60_DISPLAYTOP
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
DATA more_btn(50).
DATA: gv_linecount TYPE sy-tabix ##NEEDED.

DATA: ok_code_0100 TYPE sy-ucomm,
      ok_code_1100 TYPE sy-ucomm.
CONTROLS: selfields_tc TYPE TABLEVIEW USING SCREEN 1100.
DATA: gs_selfields TYPE yptpi5324s_sel_screen.    "Structure for the screen elements on screen '1100'
DATA: gt_selfields TYPE TABLE OF yptpi5324s_sel_screen.
DATA: gs_selfields_wa TYPE yptpi5324s_sel_screen.  "structure for looping the gt_selfields
DATA: gv_tabname            TYPE yptpi5324e_vtnam,
      gv_nr_of_combinations TYPE numc10,
      gv_restrict_comb_ids  TYPE i VALUE 50.
