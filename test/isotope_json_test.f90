module isotope_json_test
    use erloff, only: error_list_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: fallible_json_value_t, json_object_t, parse_json
    use matterff, only: fallible_isotope_t, isotope_t, H_1
    use strff, only: NEWLINE
    use vegetables, only: &
            result_t, test_item_t, assert_equals, describe, fail, it

    implicit none
    private
    public :: test_isotope_json
contains
    function test_isotope_json() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "an isotope", &
                [ it("can be converted to JSON", check_convert_to_json) &
                , it("can be extracted from JSON", check_extract_from_json) &
                ])
    end function

    function check_convert_to_json() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '{' // NEWLINE &
// '    "fraction" : 1.0,' // NEWLINE &
// '    "isotope" : "H-1",' // NEWLINE &
// '    "atomic mass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '}'
        type(json_object_t) :: json

        json = H_1%to_json_with_fraction(1.0d0)
        result_ = assert_equals(EXPECTED, json%to_expanded_string())
    end function

    function check_extract_from_json() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: JSON_STRING = &
   '{' // NEWLINE &
// '    "fraction" : 1.0,' // NEWLINE &
// '    "isotope" : "H-1",' // NEWLINE &
// '    "atomic mass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '}'
        type(error_list_t) :: errors
        type(isotope_t) :: isotope
        type(fallible_isotope_t) :: maybe_isotope
        type(fallible_json_value_t) :: maybe_json

        maybe_json = parse_json(JSON_STRING)
        if (maybe_json%failed()) then
            errors = maybe_json%errors()
            result_ = fail(errors%to_string())
        else
            select type (object => maybe_json%value_())
            type is (json_object_t)
                maybe_isotope = fallible_isotope_t(object)
                if (maybe_isotope%failed()) then
                    errors = maybe_isotope%errors()
                    result_ = fail(errors%to_string())
                else
                    isotope = maybe_isotope%isotope()
                    result_ = assert_equals(H_1%to_string(), isotope%to_string())
                end if
            class default
                result_ = fail("Didn't get an object: " // object%to_compact_string())
            end select
        end if
    end function
end module
