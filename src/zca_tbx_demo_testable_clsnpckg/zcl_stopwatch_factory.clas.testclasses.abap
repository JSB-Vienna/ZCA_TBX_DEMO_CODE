class ltc_factory definition for testing
                             final
                             duration short
                             risk level harmless.
  private section.
    data:
      factory type ref to zif_stopwatch_factory.

    methods:
      setup,
      get_factory_ok for testing,
      create_stopwatch_ok for testing.

endclass.


class ltc_factory implementation.

  method setup.
    factory = zcl_stopwatch_factory=>get_singleton( ).
  endmethod.                 "setup

  method get_factory_ok.
    cl_abap_unit_assert=>assert_bound( factory ).
    cl_abap_unit_assert=>assert_equals( exp = factory
                                        act = zcl_stopwatch_factory=>get_singleton( ) ).
  endmethod.                 "get_factory_ok


  method create_stopwatch_ok.
    data(_stopwatch_a) = factory->create_stopwatch( ).
    cl_abap_unit_assert=>assert_bound( _stopwatch_a ).
    data(_stopwatch_b) = factory->create_stopwatch( ).
    cl_abap_unit_assert=>assert_bound( _stopwatch_b ).
    cl_abap_unit_assert=>assert_false( xsdbool( _stopwatch_a eq _stopwatch_b ) ).
  endmethod.                 "create_stopwatch_ok

endclass.
