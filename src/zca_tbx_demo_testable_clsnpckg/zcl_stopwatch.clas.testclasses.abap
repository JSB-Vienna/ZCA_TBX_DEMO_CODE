CLASS ltc_stopwatch_testbase DEFINITION ABSTRACT
                                        FOR TESTING
                                        INHERITING FROM zth_stopwatch_testbase
                                        DURATION SHORT
                                        RISK LEVEL HARMLESS.
  PROTECTED SECTION.
    DATA:
      cut TYPE REF TO zif_stopwatch.


  PRIVATE SECTION.
    METHODS:
      setup.

ENDCLASS.


CLASS ltc_stopwatch_testbase IMPLEMENTATION.

  METHOD setup.
    cut = zcl_stopwatch_factory=>get_singleton( )->create_stopwatch( ).
  ENDMETHOD.                 "setup

ENDCLASS.



CLASS ltc_stopwatch DEFINITION FOR TESTING
                               INHERITING FROM ltc_stopwatch_testbase
                               DURATION SHORT
                               RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      create_ok FOR TESTING.

ENDCLASS.


CLASS ltc_stopwatch IMPLEMENTATION.

  METHOD create_ok.
    cl_abap_unit_assert=>assert_bound( cut ).
  ENDMETHOD.                 "create_ok

ENDCLASS.
