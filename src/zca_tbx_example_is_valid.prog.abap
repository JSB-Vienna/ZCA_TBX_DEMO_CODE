"! <p class="shorttext synchronized" lang="en">Example of a validation method -> see comment to method MAIN</p>
REPORT zca_tbx_example_is_valid.

"! <p class="shorttext synchronized" lang="en">Test check method</p>
CLASS lcl_check DEFINITION FINAL
                           CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Main method, that controls the entire processing</p>
      "!
      "! <p>Tha MAIN method executes a validation method in two different ways. At first used as a functional
      "! method in an IF statement (could also be a case where the result is immediately be used) and in a
      "! second try without using the validation result which turns into an exception.</p>
      "!
      "! <p>What do you think about this approach?</p>
      main.


  PRIVATE SECTION.
    METHODS:
      is_parameter_valid
        IMPORTING
          i_parameter   TYPE char1
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_param.
ENDCLASS.                     "lcl_check  DEFINITION


CLASS lcl_check IMPLEMENTATION.

  METHOD main.
    TRY.
        IF NOT is_parameter_valid( 'P' ).
          is_parameter_valid( 'P' ).
        ENDIF.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "main


  METHOD is_parameter_valid.
    result = xsdbool( i_parameter CO 'AB' ).

    IF result IS NOT SUPPLIED AND
       result EQ abap_false.
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>param_invalid
          mv_msgty = zcx_ca_param=>c_msgty_e
          mv_msgv1 = 'I_PARAMETER'
          mv_msgv2 = CONV #( i_parameter ).
    ENDIF.
  ENDMETHOD.                    "main

ENDCLASS.                     "lcl_check  IMPLEMENTATION



*---------------------------------------------------------------------*
*     s t a r t - o f - s e l e c t i o n
*---------------------------------------------------------------------*
START-OF-SELECTION.
  NEW lcl_check( )->main( ).
