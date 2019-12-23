module element_test
    use Element_m, only: Element_t, fromAtomFractions
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: H
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: He_3
    use Isotope_symbol_m, only: H_1_SYM
    use Utilities_m, only: MISMATCH_TYPE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, Describe, It

    implicit none
    private

    public :: test_element
contains
    function test_element() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "Doesn't contain any isotopes when it's empty", checkEmpty)
        individual_tests(2) = It( &
                "Creating an element with isotopes of a different element is an error", &
                checkDiffIsotopes)
        tests = Describe("Element_t", individual_tests)
    end function test_element

    pure function checkEmpty() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: empty

        result_ = &
                assertEquals(0.0d0, empty%atomFraction(H_1_SYM), "atom fraction") &
                .and.assertEquals(0.0d0, empty%weightFraction(H_1_SYM), "weight fraction")
    end function checkEmpty

    pure function checkDiffIsotopes() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, ElementComponent([He_3], [1.0d0]), messages, errors, element)

        result_ = assertThat( &
                errors.hasType.MISMATCH_TYPE, errors%toString())
    end function checkDiffIsotopes
end module element_test
