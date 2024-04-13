CLASS zcl_stopwatch DEFINITION PUBLIC
                               CREATE PRIVATE
                               FINAL
                               GLOBAL FRIENDS zcl_stopwatch_factory.
  PUBLIC SECTION.
    INTERFACES:
      zif_stopwatch.

    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zif_stopwatch.


  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_stopwatch IMPLEMENTATION.

  METHOD get_instance.
    result ?= NEW zcl_stopwatch( ).
  ENDMETHOD.                 "get_instance

ENDCLASS.
