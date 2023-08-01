*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


TYPES: BEGIN OF ty_2chars,
         slnid             TYPE slnid,
         test_cstic_1      TYPE char30,
         test_hunbak_model TYPE char30,
       END OF ty_2chars.
TYPES: tty_2chars TYPE STANDARD TABLE OF ty_2chars WITH DEFAULT KEY.

TYPES: BEGIN OF ty_1char,
         slnid             TYPE slnid,
         test_hunbak_model TYPE char30,
       END OF ty_1char.
TYPES: tty_1char TYPE STANDARD TABLE OF ty_1char WITH DEFAULT KEY.

TYPES: BEGIN OF ty_1char_cstics,
         slnid        TYPE slnid,
         test_cstic_1 TYPE char30,
       END OF ty_1char_cstics.
TYPES: tty_1char_cstics TYPE STANDARD TABLE OF ty_1char_cstics WITH DEFAULT KEY.

TYPES: BEGIN OF ty_3chars,
         slnid                 TYPE slnid,
         ymfg_effectivity_date TYPE char30,
         cr18p_vcgroups        TYPE char30,
         cr18pvcs_rearweight   TYPE char30,
       END OF ty_3chars.
TYPES: tty_3chars TYPE STANDARD TABLE OF ty_3chars WITH DEFAULT KEY.
