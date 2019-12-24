module isotope_test
    use Element_symbol_m, only: H, He
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: Isotope_t, find, H_1, H_2, He_3, He_4
    use Isotope_symbol_m, only: H_1_SYM, H_2_SYM
    use Vegetables_m, only: &
            Result_t, &
            TestItem_t, &
            assertEquals, &
            assertNot, &
            assertThat, &
            Describe, &
            It

    implicit none
    private

    public :: test_isotope
contains
    function test_isotope() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = It("Is it's element", checkIsElement)
        individual_tests(2) = It( &
                "Is not a different element", checkNotDifferentElement)
        individual_tests(3) = It( &
                "Has a position of 0 if it's not in a list", checkNotFound)
        individual_tests(4) = It( &
                "Can be found in a list", checkFind)
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

    pure function checkNotFound() result(result_)
        type(Result_t) :: result_

        type(Isotope_t), parameter :: isotopes(*) = [H_2, He_3, He_4]

        result_ = assertEquals(0, find(H_1_SYM, isotopes))
    end function checkNotFound

    pure function checkFind() result(result_)
        type(Result_t) :: result_

        type(Isotope_t), parameter :: isotopes(*) = [H_1, H_2, He_3, He_4]

        result_ = assertEquals(2, find(H_2_SYM, isotopes))
    end function checkFind
end module isotope_test
