CLASS zth_stopwatch_injector DEFINITION PUBLIC
                                        CREATE PUBLIC
                                        ABSTRACT
                                        FINAL
                                        FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS:
      inject_factory
        IMPORTING
          factory TYPE REF TO zif_stopwatch_factory.


  PRIVATE SECTION.


ENDCLASS.



CLASS zth_stopwatch_injector IMPLEMENTATION.

  METHOD inject_factory.
    zcl_stopwatch_factory=>factory = factory.
  ENDMETHOD.                 "inject_factory

ENDCLASS.
