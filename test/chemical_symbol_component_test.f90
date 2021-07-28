module chemical_symbol_component_test
    use matterff, only: &
            chemical_symbol_component_t, H, He
    use vegetables, only: &
            result_t, test_item_t, assert_not, assert_that, describe, it

    implicit none
    private
    public :: test_chemical_symbol_component
contains
    function test_chemical_symbol_component() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "chemical_symbol_component_t", &
                [ it("Identical components are equal", check_equal) &
                , it( &
                        "Components with different elements aren't equal", &
                        check_diff_elements) &
                , it( &
                        "Components with different multiples aren't equal", &
                        check_diff_multiples) &
                ])
    end function

    pure function check_equal() result(result_)
        type(result_t) :: result_

        result_ = assert_that( &
                chemical_symbol_component_t(H, 1) == chemical_symbol_component_t(H, 1), &
                "H_1 == H_1")
    end function

    pure function check_diff_elements() result(result_)
        type(result_t) :: result_

        result_ = assert_not( &
                chemical_symbol_component_t(H, 1) == chemical_symbol_component_t(He, 1), &
                "H_1 == He_1")
    end function

    pure function check_diff_multiples() result(result_)
        type(result_t) :: result_

        result_ = assert_not( &
                chemical_symbol_component_t(H, 1) == chemical_symbol_component_t(H, 2), &
                "H_1 == H_2")
    end function
end module
