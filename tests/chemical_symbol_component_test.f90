module chemical_symbol_component_test
    use Chemical_symbol_component_m, only: ChemicalSymbolComponent
    use Element_symbol_m, only: H, He
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertNot, assertThat, Describe, It

    implicit none
    private

    public :: test_chemical_symbol_component
contains
    function test_chemical_symbol_component() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It("Identical components are equal", checkEqual)
        individual_tests(2) = It( &
                "Components with different elements aren't equal", &
                checkDiffElements)
        individual_tests(3) = It( &
                "Components with different multiples aren't equal", &
                checkDiffMultiples)
        tests = Describe("ChemicalSymbolComponent_t", individual_tests)
    end function test_chemical_symbol_component

    pure function checkEqual() result(result_)
        type(Result_t) :: result_

        result_ = assertThat( &
                ChemicalSymbolComponent(H, 1) == ChemicalSymbolComponent(H, 1), &
                "H_1 == H_1")
    end function checkEqual

    pure function checkDiffElements() result(result_)
        type(Result_t) :: result_

        result_ = assertNot( &
                ChemicalSymbolComponent(H, 1) == ChemicalSymbolComponent(He, 1), &
                "H_1 == He_1")
    end function checkDiffElements

    pure function checkDiffMultiples() result(result_)
        type(Result_t) :: result_

        result_ = assertNot( &
                ChemicalSymbolComponent(H, 1) == ChemicalSymbolComponent(H, 2), &
                "H_1 == H_2")
    end function checkDiffMultiples
end module chemical_symbol_component_test
