CLASS ztd_stopwatch_factory DEFINITION PUBLIC
                                       FOR TESTING
                                       FINAL
                                       CREATE PRIVATE
                                       INHERITING FROM zcl_stopwatch_factory.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_factory_double
        RETURNING
          VALUE(result) TYPE REF TO ztd_stopwatch_factory.

    METHODS:
      zif_stopwatch_factory~create_stopwatch REDEFINITION,

      inject_stopwatch
        IMPORTING
          stopwatch TYPE REF TO zif_stopwatch.

  PRIVATE SECTION.
    DATA:
      stopwatch_collection TYPE STANDARD TABLE OF REF TO zif_stopwatch
                                                    WITH EMPTY KEY.
ENDCLASS.



CLASS ztd_stopwatch_factory IMPLEMENTATION.

  METHOD get_factory_double.
    result = NEW ztd_stopwatch_factory( ).
  ENDMETHOD.                 "get_instance


  METHOD zif_stopwatch_factory~create_stopwatch.
    IF stopwatch_collection IS INITIAL.
      result = super->zif_stopwatch_factory~create_stopwatch( ).

    ELSE.
      result = stopwatch_collection[ 1 ].
      DELETE stopwatch_collection INDEX 1.
    ENDIF.
  ENDMETHOD.                 "zif_stopwatch_factory~create_stopwatch


  METHOD inject_stopwatch.
    APPEND stopwatch TO stopwatch_collection.
  ENDMETHOD.                 "inject_stopwatch

ENDCLASS.
