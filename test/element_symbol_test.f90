module element_symbol_test
    use matterff, only: H, He
    use iso_varying_string, only: operator(//)
    use vegetables, only: &
            result_t, test_item_t, assert_not, assert_that, describe, it

    implicit none
    private
    public :: test_hello
contains
    function test_hello() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "An element_symbol_t", &
                [ it("Equals itself", check_equals_itself) &
                , it( &
                        "Doesn't equal a different element", &
                        check_not_equal_different_element) &
                ])
    end function

    pure function check_equals_itself() result(result_)
        type(result_t) :: result_

        result_ = assert_that(H == H, H%to_string() // " == " // H%to_string())
    end function

    pure function check_not_equal_different_element() result(result_)
        type(result_t) :: result_

        result_ = assert_not(H == He, H%to_string() // " == " // He%to_string())
    end function
end module
