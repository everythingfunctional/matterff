program test
    implicit none

    call run()
contains
    subroutine run()
        use chemical_symbol_component_test, only: &
            chemical_symbol_component_chemical_symbol_component => test_chemical_symbol_component
        use chemical_test, only: &
            chemical_chemical => test_chemical
        use element_symbol_test, only: &
            element_symbol_hello => test_hello
        use element_test, only: &
            element_element => test_element
        use isotope_symbol_test, only: &
            isotope_symbol_isotope_symbol => test_isotope_symbol
        use isotope_test, only: &
            isotope_isotope => test_isotope
        use material_test, only: &
            material_material => test_material
        use iso_varying_string
        use Vegetables_m, only: TestItem_t, testThat, runTests

        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(7)

        individual_tests(1) = chemical_symbol_component_chemical_symbol_component()
        individual_tests(2) = chemical_chemical()
        individual_tests(3) = element_symbol_hello()
        individual_tests(4) = element_element()
        individual_tests(5) = isotope_symbol_isotope_symbol()
        individual_tests(6) = isotope_isotope()
        individual_tests(7) = material_material()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program test
