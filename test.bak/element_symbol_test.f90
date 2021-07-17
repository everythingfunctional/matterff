module element_symbol_test
    use Element_symbol_m, only: H, He
    use iso_varying_string, only: operator(//)
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertNot, assertThat, describe, it

    implicit none
    private

    public :: test_hello
contains
    function test_hello() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It("Equals itself", checkEqualsItself)
        individual_tests(2) = It( &
                "Doesn't equal a different element", &
                checkNotEqualDifferentElement)
        tests = describe("ElementSymbol_t", individual_tests)
    end function test_hello

    pure function checkEqualsItself() result(result_)
        type(Result_t) :: result_

        result_ = assertThat(H == H, H%toString() // " == " // H%toString())
    end function checkEqualsItself

    pure function checkNotEqualDifferentElement() result(result_)
        type(Result_t) :: result_

        result_ = assertNot(H == He, H%toString() // " == " // He%toString())
    end function checkNotEqualDifferentElement
end module element_symbol_test
