INTERFACE zif_stopwatch_factory PUBLIC.
  METHODS:
    create_stopwatch
      RETURNING
        VALUE(result) TYPE REF TO zif_stopwatch.
ENDINTERFACE.
