module chemical_symbol_json_test
    use erloff, only: error_list_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: fallible_json_value_t, json_array_t, parse_json
    use matterff, only: &
            chemical_symbol_t, fallible_chemical_symbol_t, hydrogen_gas_symbol
    use strff, only: NEWLINE
    use vegetables, only: &
            result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_chemical_symbol_json
contains
    function test_chemical_symbol_json() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "A chemical_symbol_t", &
                [ it("can be converted to JSON", check_convert_to_json) &
                , it("can be extracted from JSON", check_extract_from_json) &
                ])
    end function

    function check_convert_to_json() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '[' // NEWLINE &
// '    {' // NEWLINE &
// '        "element" : "H",' // NEWLINE &
// '        "multiple" : 2' // NEWLINE &
// '    }' // NEWLINE &
// ']'
        type(json_array_t) :: json
        type(chemical_symbol_t) :: symbol

        symbol = hydrogen_gas_symbol()
        json = symbol%to_json()
        result_ = assert_equals(EXPECTED, json%to_expanded_string())
    end function

    function check_extract_from_json() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: JSON_STRING = &
   '[' // NEWLINE &
// '    {' // NEWLINE &
// '        "element" : "H",' // NEWLINE &
// '        "multiple" : 2' // NEWLINE &
// '    },' // NEWLINE &
// '    {' // NEWLINE &
// '        "element" : "O",' // NEWLINE &
// '        "multiple" : 1' // NEWLINE &
// '    }' // NEWLINE &
// ']'
        type(error_list_t) :: errors
        type(fallible_chemical_symbol_t) :: maybe_symbol
        type(fallible_json_value_t) :: maybe_json
        type(json_array_t) :: new_json
        type(chemical_symbol_t) :: symbol

        maybe_json = parse_json(JSON_STRING)
        if (maybe_json%failed()) then
            errors = maybe_json%errors()
            result_ = fail(errors%to_string())
        else
            select type (json => maybe_json%value_())
            type is (json_array_t)
                maybe_symbol = fallible_chemical_symbol_t(json)
                if (maybe_symbol%failed()) then
                    errors = maybe_symbol%errors()
                    result_ = fail(errors%to_string())
                else
                    symbol = maybe_symbol%chemical_symbol()
                    new_json = symbol%to_json()
                    result_ = assert_equals(JSON_STRING, new_json%to_expanded_string())
                end if
            class default
                result_ = fail("Didn't get an array: " // json%to_compact_string())
            end select
        end if
    end function
end module
