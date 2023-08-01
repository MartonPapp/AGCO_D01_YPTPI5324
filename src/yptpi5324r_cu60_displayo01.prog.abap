*&---------------------------------------------------------------------*
*& Report  YPTPI5324R_CU60_DISPLAYO01
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
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'GUI0100'.
  SET TITLEBAR 'TITLE0100'.
  lcl_cu60_display=>get_inst( )->pbo0100( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1100 OUTPUT.
  SET PF-STATUS 'GUI1100'.
  set TITLEBAR 'TITLE1100'.
  lcl_cu60_display=>get_inst( )->pbo1100( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHANGE_SCREEN_SEL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_screen_sel OUTPUT.
  lcl_cu60_display=>get_inst( )->change_sel_screen( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_LINECOUNT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_linecount OUTPUT.
  gv_linecount = lines( gt_selfields ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SHOW_LINES_SEL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE show_lines_sel OUTPUT.
  MOVE-CORRESPONDING gs_selfields_wa TO GS_SELFIELDS.
ENDMODULE.
