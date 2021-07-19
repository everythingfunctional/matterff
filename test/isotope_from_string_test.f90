module isotope_from_string_test
    use erloff, only: error_list_t
    use matterff, only: fallible_isotope_t, isotope_t, H_1, He_4
    use matterff_utilities_m, only: INVALID_ARGUMENT
    use vegetables, only: &
            result_t, test_item_t, assert_equals, assert_that, describe, fail, it

    implicit none
    private
    public :: test_isotope_from_string
contains
    function test_isotope_from_string() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "fallible_isotope_t.from_string", &
                [ it("Works for known, valid isotopes", check_valid_string) &
                , it("Fails if the string doesn't have the '-'", check_missing_dash) &
                , it("Fails if it can't interpret the mass number", check_not_a_number) &
                , it("Fails for an unknown isotope", check_unknown) &
                ])
    end function

    function check_valid_string() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(isotope_t) :: helium
        type(isotope_t) :: hydrogen
        type(fallible_isotope_t) :: maybe_helium
        type(fallible_isotope_t) :: maybe_hydrogen

        maybe_hydrogen = fallible_isotope_t("H-1")
        if (maybe_hydrogen%failed()) then
            errors = maybe_hydrogen%errors()
            result_ = fail(errors%to_string())
        else
            hydrogen = maybe_hydrogen%isotope()
            result_ = assert_equals(H_1%to_string(), hydrogen%to_string())
        end if

        maybe_helium = fallible_isotope_t("He-4")
        if (maybe_helium%failed()) then
            errors = maybe_helium%errors()
            result_ = result_.and.fail(errors%to_string())
        else
            helium = maybe_helium%isotope()
            result_ = result_.and.assert_equals(He_4%to_string(), helium%to_string())
        end if
    end function

    function check_missing_dash() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_isotope_t) :: maybe_isotope

        maybe_isotope = fallible_isotope_t("H1")
        errors = maybe_isotope%errors()
        result_ = assert_that( &
                errors.hasType.INVALID_ARGUMENT, errors%to_string())
    end function

    function check_not_a_number() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_isotope_t) :: maybe_isotope

        maybe_isotope = fallible_isotope_t("H-#")
        errors = maybe_isotope%errors()
        result_ = assert_that( &
                errors.hasType.INVALID_ARGUMENT, errors%to_string())
    end function

    function check_unknown() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_isotope_t) :: maybe_isotope

        maybe_isotope = fallible_isotope_t("H-123")
        errors = maybe_isotope%errors()
        result_ = assert_that( &
                errors.hasType.INVALID_ARGUMENT, errors%to_string())
    end function
end module
