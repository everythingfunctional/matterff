module element_test
    use Element_m, only: Element_t
    use Isotope_symbol_m, only: H_1_SYM
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, Describe, It

    implicit none
    private

    public :: test_element
contains
    function test_element() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "Doesn't contain any isotopes when it's empty", checkEmpty)
        tests = Describe("Element_t", individual_tests)
    end function test_element

    pure function checkEmpty() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: empty

        result_ = &
                assertEquals(0.0d0, empty%atomFraction(H_1_SYM), "atom fraction") &
                .and.assertEquals(0.0d0, empty%weightFraction(H_1_SYM), "weight fraction")
    end function checkEmpty
end module element_test
