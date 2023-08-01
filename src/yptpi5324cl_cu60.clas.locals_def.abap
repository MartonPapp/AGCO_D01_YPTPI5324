*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES: BEGIN OF ty_atinn_cnt,
         atinn  TYPE atinn,
         active TYPE abap_bool,
         valc   TYPE atwrt,
         count  TYPE i,
       END OF ty_atinn_cnt.
TYPES: tty_atinn_cnt TYPE SORTED TABLE OF ty_atinn_cnt WITH NON-UNIQUE KEY atinn.
TYPES: BEGIN OF ty_char_vals,
         char_val TYPE atwrt,
         char_numval_from type ATFLV,
         char_numval_to type atflv,
         char_numval_unit_from type MSEHI,
         char_numval_unit_to type msehi,
       END OF ty_char_vals.
TYPES: tty_char_vals TYPE STANDARD TABLE OF ty_char_vals WITH DEFAULT KEY.
TYPES: BEGIN OF ty_char_vals_per_id_and_name,
         slnid     TYPE slnid,
         char_id   TYPE atinn,
         atfor     type atfor,
         active    TYPE abap_bool,
         char_vals TYPE tty_char_vals,
       END OF ty_char_vals_per_id_and_name.
TYPES: tty_char_vals_per_id_and_name TYPE HASHED TABLE OF ty_char_vals_per_id_and_name WITH UNIQUE KEY slnid char_id.
