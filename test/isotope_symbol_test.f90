module isotope_symbol_test
    use iso_varying_string, only: operator(//)
    use matterff, only: H, H_1_SYM, H_2_SYM, H_3_SYM, He, He_3_SYM
    use vegetables, only: &
            result_t, test_item_t, assert_not, assert_that, describe, it

    implicit none
    private
    public :: test_isotope_symbol
contains
    function test_isotope_symbol() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "An isotope_symbol_t", &
                [ it("Equals itself", check_equals_itself) &
                , it( &
                        "Doesn't equal a different element with the same mass number", &
                        check_not_equal_different_element) &
                , it( &
                        "Doesn't equal a different isotope of the same element", &
                        check_not_equal_different_isotope) &
                , it("Is its element", check_is_its_element) &
                , it("Is not a different element", check_is_not_different_element) &
                ])
    end function

    pure function check_equals_itself() result(result_)
        type(result_t) :: result_

        result_ = assert_that( &
                H_1_SYM == H_1_SYM, &
                H_1_SYM%to_string() // " == " // H_1_SYM%to_string())
    end function

    pure function check_not_equal_different_element() result(result_)
        type(result_t) :: result_

        result_ = assert_not( &
                H_3_SYM == He_3_SYM, &
                H_3_SYM%to_string() // " == " // He_3_SYM%to_string())
    end function

    pure function check_not_equal_different_isotope() result(result_)
        type(result_t) :: result_

        result_ = assert_not( &
                H_1_SYM == H_2_SYM, &
                H_1_SYM%to_string() // " == " // H_2_SYM%to_string())
    end function 

    pure function check_is_its_element() result(result_)
        type(result_t) :: result_

        result_ = assert_that( &
                H_1_SYM%is(H), &
                H_1_SYM%to_string() // "%is(" // H%to_string() // ")")
    end function

    pure function check_is_not_different_element() result(result_)
        type(result_t) :: result_

        result_ = assert_not( &
                H_1_SYM%is(He), &
                H_1_SYM%to_string() // "%is(" // He%to_string() // ")")
    end function
end module
