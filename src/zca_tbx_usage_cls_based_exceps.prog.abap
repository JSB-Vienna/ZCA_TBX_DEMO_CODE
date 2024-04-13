"! <p class="shorttext synchronized" lang="en">Example for the usage of class based exceptions</p>
REPORT zca_tbx_usage_cls_based_exceps.


CLASS lcx_this_application DEFINITION CREATE PUBLIC
                                      INHERITING FROM zcx_test. "cx_static_check.
  PUBLIC SECTION.
    ALIASES:
      msgty FOR if_t100_dyn_msg~msgty,
      msgv1 FOR if_t100_dyn_msg~msgv1,
      msgv2 FOR if_t100_dyn_msg~msgv2,
      msgv3 FOR if_t100_dyn_msg~msgv3,
      msgv4 FOR if_t100_dyn_msg~msgv4.

    CONSTANTS:
      BEGIN OF lcx_this_application,
        msgid TYPE symsgid VALUE '38',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF lcx_this_application.

    CONSTANTS:
      BEGIN OF creation_failed,
        msgid TYPE symsgid VALUE '38',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF creation_failed.

    CONSTANTS:
      BEGIN OF no_data_found,
        msgid TYPE symsgid VALUE '38',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF no_data_found.

    CONSTANTS:
      BEGIN OF save_logging_failed,
        msgid TYPE symsgid VALUE '38',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE 'MSGV4',
      END OF save_logging_failed.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL
        msgty    TYPE symsgty DEFAULT 'E'
        msgv1    TYPE symsgv OPTIONAL
        msgv2    TYPE symsgv OPTIONAL
        msgv3    TYPE symsgv OPTIONAL
        msgv4    TYPE symsgv OPTIONAL.
ENDCLASS.

CLASS lcx_this_application IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    me->msgty = msgty.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = lcx_this_application .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcx_this_went_wrong_01 DEFINITION CREATE PUBLIC INHERITING FROM lcx_this_application FINAL.
ENDCLASS.


CLASS lcx_this_went_wrong_02 DEFINITION CREATE PUBLIC INHERITING FROM lcx_this_application FINAL.
ENDCLASS.


CLASS lcx_this_went_wrong_03 DEFINITION CREATE PUBLIC INHERITING FROM lcx_this_application FINAL.
ENDCLASS.



"! <p class="shorttext synchronized" lang="en">Demo class with classic exception</p>
CLASS demo_class_based_exceps DEFINITION CREATE PUBLIC
                                         FINAL.
  PUBLIC SECTION.
    INTERFACES:
      if_xo_const_message.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Main method, that controls the entire processing</p>
      main.


  PRIVATE SECTION.
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

    "! <p class="shorttext synchronized" lang="en">Create BAL</p>
    "!
    "! @raising   lcx_this_application | <p class="shorttext synchronized" lang="en">xx</p>
    METHODS create_logging
      RAISING
        lcx_this_application.

    "! <p class="shorttext synchronized" lang="en">Read QM Notifications</p>
    "!
    "! @parameter result               | <p class="shorttext synchronized" lang="en">Found quality notifications</p>
    "! @raising   lcx_this_application | <p class="shorttext synchronized" lang="en">xx</p>
    METHODS read_qm_notifications
      RETURNING
        VALUE(result) TYPE qmel_essence_tab
      RAISING
        lcx_this_application.

    "! <p class="shorttext synchronized" lang="en">Do something first, but right and not left</p>
    "!
    "! @parameter qm_notification        | <p class="shorttext synchronized" lang="en">Quality notification</p>
    "! @raising   lcx_this_went_wrong_01 | <p class="shorttext synchronized" lang="en">The first thing that goes wrong</p>
    "! @raising   lcx_this_went_wrong_02 | <p class="shorttext synchronized" lang="en">The second thing that goes wrong</p>
    METHODS do_something_01
      IMPORTING
        qm_notification TYPE REF TO qmel_essence
      RAISING
        lcx_this_went_wrong_01
        lcx_this_went_wrong_02.


    "! <p class="shorttext synchronized" lang="en">Do something second</p>
    "!
    "! @parameter qm_notification        | <p class="shorttext synchronized" lang="en">Quality notification</p>
    "! @raising   lcx_this_went_wrong_02 | <p class="shorttext synchronized" lang="en">The second thing that goes wrong</p>
    METHODS do_somethinrg_02
      IMPORTING
        qm_notification TYPE REF TO qmel_essence
      RETURNING
        VALUE(result)   TYPE REF TO qmel_essence
      RAISING
        lcx_this_went_wrong_02.

    METHODS post_changes
      IMPORTING
        qm_notification TYPE REF TO qmel_essence
      RAISING
        lcx_this_went_wrong_03.

    METHODS log_exception
      IMPORTING
        exception TYPE REF TO cx_root.

    METHODS save_logging
      RAISING
        cx_bal_exception.

ENDCLASS.                     "demo_classic_exceptions  DEFINITION


CLASS demo_class_based_exceps IMPLEMENTATION.

  METHOD main.
    "-----------------------------------------------------------------*
    "   Main method, that controls the entire processing
    "-----------------------------------------------------------------*
    TRY.
        create_logging( ).

        DATA(_qm_notifications) = read_qm_notifications( ).

        LOOP AT _qm_notifications REFERENCE INTO DATA(_qm_notification).
          TRY.
              do_something_01( _qm_notification ).

              _qm_notification = do_somethinrg_02( _qm_notification ).

              post_changes( _qm_notification ).

            CATCH lcx_this_went_wrong_02.
              DELETE _qm_notifications.

            CATCH lcx_this_application INTO DATA(lx_catched_in_loop).
              log_exception( lx_catched_in_loop ).
          ENDTRY.
        ENDLOOP.

        save_logging( ).

      CATCH lcx_this_application                      "Catching different exceptions in one statement
            cx_bal_exception INTO DATA(lx_catched).   "One of them has no parameter MSGTY, so use constants
        MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE c_msgty_e.   "Use S to avoid abort of program
    ENDTRY.
  ENDMETHOD.                    "main


  METHOD create_logging.
*    RAISE EXCEPTION NEW lcx_this_application( textid = lcx_this_application=>creation_failed ).
  ENDMETHOD.


  METHOD read_qm_notifications.
    APPEND INITIAL LINE TO result.
*    RAISE EXCEPTION NEW lcx_this_application( textid = lcx_this_application=>no_data_found ).
  ENDMETHOD.


  METHOD do_something_01.
*    RAISE EXCEPTION NEW lcx_this_went_wrong_01( ).
*    RAISE EXCEPTION NEW lcx_this_went_wrong_02( ).
  ENDMETHOD.


  METHOD do_somethinrg_02.
*    RAISE EXCEPTION NEW lcx_this_went_wrong_02( ).
  ENDMETHOD.


  METHOD post_changes.
*    RAISE EXCEPTION NEW lcx_this_went_wrong_03( ).
  ENDMETHOD.


  METHOD log_exception.

  ENDMETHOD.


  METHOD save_logging.
*    RAISE EXCEPTION NEW cx_bal_exception( ).
  ENDMETHOD.

ENDCLASS.                     "demo_classic_exceptions  IMPLEMENTATION


*---------------------------------------------------------------------*
*     s t a r t - o f - s e l e c t i o n
*---------------------------------------------------------------------*
START-OF-SELECTION.
  NEW demo_class_based_exceps( )->main( ).
