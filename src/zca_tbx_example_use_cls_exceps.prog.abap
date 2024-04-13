*&---------------------------------------------------------------------*
*& Report zzjsb_example_usage_exceptions
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zca_tbx_example_use_cls_exceps.

"! <p class="shorttext synchronized" lang="en">Class using only classic exceptions</p>
CLASS lcl_classic_exception DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      raise_without_message EXCEPTIONS no_message,
      clear_sy_flds_n_raise_wo_msg EXCEPTIONS no_message_no_sy_flds,
      raise_with_message EXCEPTIONS with_message.
ENDCLASS.                     "lcl_classic_exception  DEFINITION


CLASS lcl_classic_exception IMPLEMENTATION.
  METHOD raise_without_message.
    RAISE no_message.
  ENDMETHOD.

  METHOD clear_sy_flds_n_raise_wo_msg.
    CLEAR: sy-msgid, sy-msgno, sy-msgty.
    RAISE no_message_no_sy_flds.
  ENDMETHOD.

  METHOD raise_with_message.
    MESSAGE e001(38) WITH 'I want to' 'appear as' 'an exception' 'at the screen :)' RAISING with_message.
  ENDMETHOD.
ENDCLASS.                     "lcl_classic_exception  IMPLEMENTATION



"! <p class="shorttext synchronized" lang="en">Class using class-based exception to handle classic exceptns.</p>
CLASS lcl_class_based_exceptions DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      main.

  PRIVATE SECTION.
    DATA:
      mo_output TYPE REF TO if_demo_output.

    METHODS:
      using_exceptions RAISING zcx_ca_error,
      call_raising_with_message RAISING zcx_ca_dbacc,
      call_raising_without_message RAISING zcx_ca_param,
      call_raising_cleared_sy_flds RAISING zcx_ca_ui.
ENDCLASS.                     "lcl_class_based_exceptions  DEFINITION


CLASS lcl_class_based_exceptions IMPLEMENTATION.

  METHOD call_raising_with_message.
    NEW lcl_classic_exception( )->raise_with_message( EXCEPTIONS with_message = 1
                                                                 OTHERS       = 2 ).
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ca_dbacc USING MESSAGE.
    ENDIF.
  ENDMETHOD.                    "call_raising_with_message


  METHOD call_raising_without_message.
    NEW lcl_classic_exception( )->raise_without_message( EXCEPTIONS no_message = 1
                                                                    OTHERS     = 2 ).
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ca_param USING MESSAGE.
    ENDIF.
  ENDMETHOD.                    "call_raising_without_message


  METHOD call_raising_cleared_sy_flds.
    NEW lcl_classic_exception( )->clear_sy_flds_n_raise_wo_msg( EXCEPTIONS no_message_no_sy_flds = 1
                                                                           OTHERS                = 2 ).
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ca_ui USING MESSAGE.
    ENDIF.
  ENDMETHOD.                    "call_raising_cleared_sy_flds


  METHOD using_exceptions.
    DATA: lo_classic_excep TYPE REF TO lcl_classic_exception,
          lx_dbacc         TYPE REF TO zcx_ca_dbacc,
          lx_param         TYPE REF TO zcx_ca_param.

    TRY.
        IF lx_dbacc IS NOT BOUND.
          call_raising_with_message( ).
        ENDIF.

        IF lx_param IS NOT BOUND.
          call_raising_without_message( ).
        ENDIF.

        call_raising_cleared_sy_flds( ).

      CATCH zcx_ca_dbacc INTO lx_dbacc.
        mo_output->begin_section( 'Raising with message' ).
        mo_output->write_text( lx_dbacc->get_text( ) ).
        RETRY.

      CATCH zcx_ca_param INTO lx_param.
        mo_output->next_section( 'Raising without message,no clear on SY-MSG fields -> uses last message!!!' ).
        mo_output->write_text( lx_param->get_text( ) ).
        RETRY.

      CATCH zcx_ca_ui
            cx_sy_no_handler INTO DATA(lx_ui).
        mo_output->next_section( 'Raising without message again, but clearing SY-MSG fields before -> System exception!!!!' ).
        mo_output->write_text( lx_ui->get_text( ) ).
    ENDTRY.

    DATA(lx_error) = CAST zcx_ca_param( zcx_ca_error=>create_exception(
                                                           iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                           iv_class    = 'LCL_CLASS_BASED_EXCEPTIONS'
                                                           iv_method   = 'MAIN'
                                                           ix_error    = lx_ui ) ) ##no_text.
    IF lx_error IS BOUND.
      RAISE EXCEPTION lx_error.
    ENDIF.
  ENDMETHOD.                    "using_exceptions


  METHOD main.
    mo_output = cl_demo_output=>new( mode = cl_demo_output=>text_mode ).

    TRY.
        using_exceptions( ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        TRY.
            DATA(lo_type_descr) = NEW zcl_ca_ddic( io_object = lx_catched->previous ).
            MESSAGE i001(38) WITH 'Last exception type was not ZCX_CA_UI, but'
                                  lo_type_descr->mo_type_desc->get_relative_name( )
                                  'because no exception class with interface'
                                  'IF_T100_DYN_MSG respectively no message values.'.
            MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.

          CATCH zcx_ca_param INTO DATA(lx_type_descr).
            MESSAGE lx_type_descr TYPE lx_type_descr->c_msgty_s DISPLAY LIKE lx_type_descr->mv_msgty.
        ENDTRY.

    ENDTRY.

    mo_output->display( ).
  ENDMETHOD.                    "main

ENDCLASS.                     "lcl_class_based_exceptions  IMPLEMENTATION



START-OF-SELECTION.
  NEW lcl_class_based_exceptions( )->main( ).
