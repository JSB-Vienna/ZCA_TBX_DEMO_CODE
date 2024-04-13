"! <p class="shorttext synchronized" lang="en">Example for the usage of classic exceptions</p>
REPORT zca_tbx_usage_classic_exceps.

"! <p class="shorttext synchronized" lang="en">Demo class with classic exception</p>
CLASS demo_classic_exceptions DEFINITION CREATE PUBLIC
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
    METHODS create_logging
      EXCEPTIONS
        creation_failed.

    "! <p class="shorttext synchronized" lang="en">Read QM Notifications</p>
    "!
    "! @parameter result        | <p class="shorttext synchronized" lang="en">Found quality notifications</p>
    "! @exception no_data_found | <p class="shorttext synchronized" lang="en">No data found</p>
    METHODS read_qm_notifications
      RETURNING
        VALUE(result) TYPE qmel_essence_tab
      EXCEPTIONS
        no_data_found.

    "! <p class="shorttext synchronized" lang="en">Do something first, but right and not left</p>
    "!
    "! @parameter qm_notification    | <p class="shorttext synchronized" lang="en">Quality notification</p>
    "! @exception this_went_wrong_01 | <p class="shorttext synchronized" lang="en">The first thing that goes wrong</p>
    "! @exception this_went_wrong_02 | <p class="shorttext synchronized" lang="en">The second thing that goes wrong</p>
    METHODS do_something_01
      IMPORTING
        qm_notification TYPE REF TO qmel_essence
      EXCEPTIONS
        this_went_wrong_01
        this_went_wrong_02.

    "! <p class="shorttext synchronized" lang="en">Do something second</p>
    "!
    "! @parameter qm_notification    | <p class="shorttext synchronized" lang="en">Quality notification</p>
    "! @exception this_went_wrong_02 | <p class="shorttext synchronized" lang="en">The second thing that goes wrong</p>
    METHODS do_somethinrg_02
      IMPORTING
        qm_notification TYPE REF TO qmel_essence
      RETURNING
        VALUE(result)   TYPE REF TO qmel_essence
      EXCEPTIONS
        this_went_wrong_02.

    METHODS post_changes
      IMPORTING
        qm_notification TYPE REF TO qmel_essence
      EXCEPTIONS
        this_went_wrong_03.

    METHODS log_exception.

    METHODS save_logging
      EXCEPTIONS
        saving_log_failed.

    METHODS save_n_close_bal
      RAISING
        cx_bal_exception.

ENDCLASS.                     "demo_classic_exceptions  DEFINITION


CLASS demo_classic_exceptions IMPLEMENTATION.

  METHOD main.
    "-----------------------------------------------------------------*
    "   Main method, that controls the entire processing
    "-----------------------------------------------------------------*
    create_logging(
              EXCEPTIONS
                creation_failed = 1
                OTHERS          = 2 ).
    IF sy-subrc NE 0.
      "Message type E leads to an abort of the program
*      MESSAGE ID sy-msgid  TYPE sy-msgty  NUMBER sy-msgno     "What contains SY-MSGTY?
*            WITH sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4.
      MESSAGE ID sy-msgid  TYPE c_msgty_s  NUMBER sy-msgno
            WITH sy-msgv1  sy-msgv2  sy-msgv3  sy-msgv4 DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    "Note: With classic excep. you can NOT use of functional method calls
    read_qm_notifications(
                      RECEIVING
                        result        = DATA(_qm_notifications)
                      EXCEPTIONS
                        no_data_found = 1
                        OTHERS        = 2 ).
    IF sy-subrc NE 0.
      "Message type E leads to an abort of the program
*      MESSAGE e001(38) WITH 'No data found to selection criteria' ##no_text.
      MESSAGE i001(38) WITH 'No data found to selection criteria' DISPLAY LIKE c_msgty_e ##no_text.
      RETURN.
    ENDIF.

    LOOP AT _qm_notifications REFERENCE INTO DATA(_qm_notification).
      do_something_01(
                 EXPORTING
                   qm_notification    = _qm_notification
                 EXCEPTIONS
                   this_went_wrong_01 = 1
                   this_went_wrong_02 = 2
                   OTHERS             = 3 ).
      CASE sy-subrc.
        WHEN 0.
        WHEN 2.
          DELETE _qm_notifications.
          CONTINUE.
        WHEN OTHERS.
          log_exception( ).
          CONTINUE.
      ENDCASE.

      do_somethinrg_02(
                 EXPORTING
                   qm_notification    = _qm_notification
                 RECEIVING
                   result             = _qm_notification
                 EXCEPTIONS
                   this_went_wrong_02 = 1
                   OTHERS             = 2 ).
      CASE sy-subrc.
        WHEN 0.
        WHEN 1.
          DELETE _qm_notifications.
          CONTINUE.
        WHEN OTHERS.
          log_exception( ).
          CONTINUE.
      ENDCASE.

      post_changes(
               EXPORTING
                qm_notification     = _qm_notification
               EXCEPTIONS
                 this_went_wrong_03 = 1
                 OTHERS             = 2 ).
      IF sy-subrc NE 0.
        log_exception( ).
      ENDIF.
    ENDLOOP.

    save_logging(
             EXCEPTIONS
               saving_log_failed = 1
               OTHERS            = 2 ).
  ENDMETHOD.                    "main


  METHOD create_logging.
*    MESSAGE e001(38) WITH 'Logging creation failed' RAISING creation_failed ##no_text.
  ENDMETHOD.


  METHOD read_qm_notifications.
    APPEND INITIAL LINE TO result.
*    RAISE no_data_found.
  ENDMETHOD.


  METHOD do_something_01.
*    MESSAGE e001(38) WITH 'First thing went wrong here' RAISING this_went_wrong_01 ##no_text.
*    MESSAGE e001(38) WITH 'Second thing went wrong here' RAISING this_went_wrong_02 ##no_text.
  ENDMETHOD.


  METHOD do_somethinrg_02.
*    MESSAGE e001(38) WITH 'Second thing went wrong here' RAISING this_went_wrong_02 ##no_text.
  ENDMETHOD.


  METHOD post_changes.
*    MESSAGE e001(38) WITH 'Third thing went wrong here' RAISING this_went_wrong_03 ##no_text.
  ENDMETHOD.


  METHOD log_exception.

  ENDMETHOD.


  METHOD save_logging.
    TRY.
*        RAISE EXCEPTION NEW cx_bal_exception( ).   "Using exception classes in a routine that has classic
        save_n_close_bal( ).                        "exceptions defined is not possible. Only this way is possible.

      CATCH cx_bal_exception INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->msgty RAISING saving_log_failed.
    ENDTRY.
  ENDMETHOD.


  METHOD save_n_close_bal.
*    RAISE EXCEPTION NEW cx_bal_exception( ).
  ENDMETHOD.

ENDCLASS.                     "demo_classic_exceptions  IMPLEMENTATION


*---------------------------------------------------------------------*
*     s t a r t - o f - s e l e c t i o n
*---------------------------------------------------------------------*
START-OF-SELECTION.
  NEW demo_classic_exceptions( )->main( ).
