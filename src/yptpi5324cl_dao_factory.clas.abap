CLASS yptpi5324cl_dao_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_DAO_FACTORY
*&---------------------------------------------------------------------*
* AUTHOR               : Marton Papp / HUNMCP
* DATE                 : 2023.01.23.
* RICEFW OBJECT        : PTP.I.5324
*
* DESCRIPTION          : Database Access Object Factory Class
*
*&---------------------------------------------------------------------*
* Changes                                                              *
* Index   Name           Date     Short description                    *
*----------------------------------------------------------------------*
  PUBLIC SECTION.
    CLASS-METHODS: set_cu60_dao_instance IMPORTING io_cu60_dao_instance TYPE REF TO yptpi5324i_cu60_dao.
    CLASS-METHODS: get_cu60_dao_instance RETURNING VALUE(ro_cu60_dao_instance) TYPE REF TO yptpi5324i_cu60_dao.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: go_cu60_dao TYPE REF TO yptpi5324i_cu60_dao.
ENDCLASS.



CLASS yptpi5324cl_dao_factory IMPLEMENTATION.
  METHOD set_cu60_dao_instance.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_DAO_FACTORY
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
    go_cu60_dao = io_cu60_dao_instance.
  ENDMETHOD.

  METHOD get_cu60_dao_instance.
*&---------------------------------------------------------------------*
*& Class  YPTPI5324CL_DAO_FACTORY
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
    IF go_cu60_dao IS NOT BOUND.
      go_cu60_dao = NEW yptpi5324cl_cu60_dao( ).
    ENDIF.
    ro_cu60_dao_instance = go_cu60_dao.
  ENDMETHOD.

ENDCLASS.
