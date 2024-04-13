CLASS zth_stopwatch_testbase DEFINITION PUBLIC
                                        ABSTRACT
                                        FOR TESTING
                                        DURATION SHORT
                                        RISK LEVEL HARMLESS.
  PROTECTED SECTION.
    DATA:
      factory_double TYPE REF TO ztd_stopwatch_factory.

  PRIVATE SECTION.
    METHODS:
      setup.

ENDCLASS.



CLASS zth_stopwatch_testbase IMPLEMENTATION.

  METHOD setup.
    factory_double = ztd_stopwatch_factory=>get_factory_double( ).
    zth_stopwatch_injector=>inject_factory( factory_double ).
  ENDMETHOD.                 "setup

ENDCLASS.
