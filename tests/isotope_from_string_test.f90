module isotope_from_string_test
    use erloff, only: ErrorList_t
    use Isotope_m, only: Isotope_t, fromString, H_1, He_4
    use Utilities_m, only: INVALID_ARGUMENT_TYPE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, Describe, fail, It

    implicit none
    private

    public :: test_isotope_from_string
contains
    function test_isotope_from_string() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = It( &
                "Works for known, valid isotopes", checkValidString)
        individual_tests(2) = It( &
                "Fails if the string doesn't have the '-'", checkMissingDash)
        individual_tests(3) = It( &
                "Fails if it can't interpret the mass number", checkNotANumber)
        individual_tests(4) = It( &
                "Fails for an unknown isotope", checkUnknown)
        tests = Describe("fromString to Isotope", individual_tests)
    end function test_isotope_from_string

    pure function checkValidString() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Isotope_t) :: helium
        type(Isotope_t) :: hydrogen

        call fromString("H-1", errors, hydrogen)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromString("He-4", errors, helium)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ = &
                        assertEquals(H_1%toString(), hydrogen%toString()) &
                        .and.assertEquals(He_4%toString(), helium%toString())
            end if
        end if
    end function checkValidString

    pure function checkMissingDash() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Isotope_t) :: isotope

        call fromString("H1", errors, isotope)
        result_ = assertThat( &
                errors.hasType.INVALID_ARGUMENT_TYPE, errors%toString())
    end function checkMissingDash

    pure function checkNotANumber() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Isotope_t) :: isotope

        call fromString("H-#", errors, isotope)
        result_ = assertThat( &
                errors.hasType.INVALID_ARGUMENT_TYPE, errors%toString())
    end function checkNotANumber

    pure function checkUnknown() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Isotope_t) :: isotope

        call fromString("H-123", errors, isotope)
        result_ = assertThat( &
                errors.hasType.INVALID_ARGUMENT_TYPE, errors%toString())
    end function checkUnknown
end module isotope_from_string_test
