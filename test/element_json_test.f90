module element_json_test
    use erloff, only: error_list_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: fallible_json_value_t, json_object_t, parse_json
    use matterff, only: fallible_element_t, element_t, natural_hydrogen
    use strff, only: NEWLINE
    use vegetables, only: &
            result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_element_json
contains
    function test_element_json() result(tests)
        type(test_item_t) :: tests

        tests = describe(&
                "An element_t", &
                [ it("can be converted to JSON", check_convert_to_json) &
                , it("can be extracted from JSON", check_extract_from_json) &
                ])
    end function

    function check_convert_to_json() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '{' // NEwLINE &
// '    "multiplier" : 1.0,' // NEWLINE &
// '    "element" : "H",' // NEWLINE &
// '    "atom fractions" : [' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 0.99988500000000002,' // NEWLINE &
// '            "isotope" : "H-1",' // NEWLINE &
// '            "atomic mass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '        },' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '            "isotope" : "H-2",' // NEWLINE &
// '            "atomic mass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '        }' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(element_t) :: hydrogen
        type(json_object_t) :: json

        hydrogen = natural_hydrogen()
        json = hydrogen%to_json_with_multiplier(1.0d0)
        result_ = assert_equals(EXPECTED, json%to_expanded_string())
    end function

    function check_extract_from_json() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: JSON_STRING = &
   '{' // NEwLINE &
// '    "multiplier" : 1.0,' // NEWLINE &
// '    "element" : "H",' // NEWLINE &
// '    "atom fractions" : [' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 0.99988500000000002,' // NEWLINE &
// '            "isotope" : "H-1",' // NEWLINE &
// '            "atomic mass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '        },' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '            "isotope" : "H-2",' // NEWLINE &
// '            "atomic mass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '        }' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(element_t) :: element
        type(error_list_t) :: errors
        type(fallible_element_t) :: maybe_element
        type(fallible_json_value_t) :: maybe_json
        type(json_object_t) :: new_json

        maybe_json = parse_json(JSON_STRING)
        if (maybe_json%failed()) then
            errors = maybe_json%errors()
            result_ = fail(errors%to_string())
        else
            select type (json => maybe_json%value_())
            type is (json_object_t)
                maybe_element = fallible_element_t(json)
                if (maybe_element%failed()) then
                    errors = maybe_element%errors()
                    result_ = fail(errors%to_string())
                else
                    element = maybe_element%element()
                    new_json = element%to_json_with_multiplier(1.0d0)
                    result_ = assert_equals(JSON_STRING, new_json%to_expanded_string())
                end if
            class default
                result_ = fail("Didn't get an object: " // json%to_compact_string())
            end select
        end if
    end function
end module
