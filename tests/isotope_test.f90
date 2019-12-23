module isotope_test
    use Element_symbol_m, only: H, He
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: H_1
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertNot, assertThat, Describe, It

    implicit none
    private

    public :: test_isotope
contains
    function test_isotope() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It("Is it's element", checkIsElement)
        individual_tests(2) = It( &
                "Is not a different element", checkNotDifferentElement)
        tests = Describe("Isotope_t", individual_tests)
    end function test_isotope

    pure function checkIsElement() result(result_)
        type(Result_t) :: result_

        result_ = assertThat( &
                H_1%is(H), H_1%toString() // "%is(" // H%toString() // ")")
    end function checkIsElement

    pure function checkNotDifferentElement() result(result_)
        type(Result_t) :: result_

        result_ = assertNot( &
                H_1%is(He), H_1%toString() // "%is(" // He%toString() // ")")
    end function checkNotDifferentElement
end module isotope_test
