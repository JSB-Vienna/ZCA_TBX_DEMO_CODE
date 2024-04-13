CLASS zcl_stopwatch_factory DEFINITION PUBLIC
                                       CREATE PROTECTED
                                       GLOBAL FRIENDS zth_stopwatch_injector.
  PUBLIC SECTION.
    INTERFACES:
      zif_stopwatch_factory.

    CLASS-METHODS:
      get_singleton
        RETURNING
          VALUE(result) TYPE REF TO zif_stopwatch_factory.


  PRIVATE SECTION.
    CLASS-DATA:
      factory  TYPE REF TO zif_stopwatch_factory.

ENDCLASS.



CLASS zcl_stopwatch_factory IMPLEMENTATION.

  METHOD get_singleton.
    IF factory IS NOT BOUND.
      factory ?= NEW zcl_stopwatch_factory( ).
    ENDIF.

    result = factory.
  ENDMETHOD.                 "get_singleton

  METHOD zif_stopwatch_factory~create_stopwatch.
    result = zcl_stopwatch=>get_instance( ).
  ENDMETHOD.                 "if_stopwatch~create_stopwatch

ENDCLASS.
