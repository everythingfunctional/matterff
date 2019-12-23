module isotope_symbol_test
    use Element_symbol_m, only: H, He
    use iso_varying_string, only: operator(//)
    use Isotope_symbol_m, only: H_1_SYM, H_2_SYM, H_3_SYM, He_3_SYM
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertNot, assertThat, Describe, It

    implicit none
    private

    public :: test_isotope_symbol
contains
    function test_isotope_symbol() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(5)

        individual_tests(1) = It("Equals itself", checkEqualsItself)
        individual_tests(2) = It( &
                "Doesn't equal a different element with the same mass number", &
                checkNotEqualDifferentElement)
        individual_tests(3) = It( &
                "Doesn't equal a different isotope of the same element", &
                checkNotEqualDifferentIsotope)
        individual_tests(4) = It("Is its element", checkIsItsElement)
        individual_tests(5) = It( &
                "Is not a different element", checkIsNotDifferentElement)
        tests = Describe("IsotopeSymbol_t", individual_tests)
    end function test_isotope_symbol

    pure function checkEqualsItself() result(result_)
        type(Result_t) :: result_

        result_ = assertThat( &
                H_1_SYM == H_1_SYM, &
                H_1_SYM%toString() // " == " // H_1_SYM%toString())
    end function checkEqualsItself

    pure function checkNotEqualDifferentElement() result(result_)
        type(Result_t) :: result_

        result_ = assertNot( &
                H_3_SYM == He_3_SYM, &
                H_3_SYM%toString() // " == " // He_3_SYM%toString())
    end function checkNotEqualDifferentElement

    pure function checkNotEqualDifferentIsotope() result(result_)
        type(Result_t) :: result_

        result_ = assertNot( &
                H_1_SYM == H_2_SYM, &
                H_1_SYM%toString() // " == " // H_2_SYM%toString())
    end function checkNotEqualDifferentIsotope

    pure function checkIsItsElement() result(result_)
        type(Result_t) :: result_

        result_ = assertThat( &
                H_1_SYM%is(H), &
                H_1_SYM%toString() // "%is(" // H%toString() // ")")
    end function checkIsItsElement

    pure function checkIsNotDifferentElement() result(result_)
        type(Result_t) :: result_

        result_ = assertNot( &
                H_1_SYM%is(He), &
                H_1_SYM%toString() // "%is(" // He%toString() // ")")
    end function checkIsNotDifferentElement
end module isotope_symbol_test
