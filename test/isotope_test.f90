module isotope_test
    use iso_varying_string, only: operator(//)
    use matterff, only: &
            isotope_t, find, H, H_1, H_1_SYM, H_2, H_2_SYM, He, He_3, He_4
    use vegetables, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_not, &
            assert_that, &
            describe, &
            it

    implicit none
    private
    public :: test_isotope
contains
    function test_isotope() result(tests)
        type(test_item_t) :: tests

        tests = describe(&
                "An isotope_t", &
                [ it("Is it's element", check_is_element) &
                , it("Is not a different element", check_not_different_element) &
                , it("Has a position of 0 if it's not in a list", check_not_found) &
                , it("Can be found in a list", check_find) &
                ])
    end function

    pure function check_is_element() result(result_)
        type(result_t) :: result_

        result_ = assert_that( &
                H_1%is(H), H_1%to_string() // "%is(" // H%to_string() // ")")
    end function

    pure function check_not_different_element() result(result_)
        type(result_t) :: result_

        result_ = assert_not( &
                H_1%is(He), H_1%to_string() // "%is(" // He%to_string() // ")")
    end function

    pure function check_not_found() result(result_)
        type(result_t) :: result_

        type(isotope_t), parameter :: isotopes(*) = [H_2, He_3, He_4]

        result_ = assert_equals(0, find(H_1_SYM, isotopes))
    end function

    pure function check_find() result(result_)
        type(result_t) :: result_

        type(isotope_t), parameter :: isotopes(*) = [H_1, H_2, He_3, He_4]

        result_ = assert_equals(2, find(H_2_SYM, isotopes))
    end function
end module
