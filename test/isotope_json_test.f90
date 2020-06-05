module isotope_json_test
    use erloff, only: ErrorList_t
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: Isotope_t, fromJson, H_1
    use jsonff, only: JsonElement_t, JsonObject_t, parseJson
    use strff, only: NEWLINE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, Describe, fail, It

    implicit none
    private

    public :: test_isotope_json
contains
    function test_isotope_json() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "can be converted to JSON", checkConvertToJson)
        individual_tests(2) = It( &
                "can be extracted from JSON", checkExtractFromJson)
        tests = Describe("an isotope", individual_tests)
    end function

    pure function checkConvertToJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '{' // NEWLINE &
// '    "fraction" : 1.0,' // NEWLINE &
// '    "isotope" : "H-1",' // NEWLINE &
// '    "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '}'
        type(JsonObject_t) :: json

        json = H_1%toJsonWithFraction(1.0d0)
        result_ = assertEquals(EXPECTED, json%toExpandedString())
    end function checkConvertToJson

    pure function checkExtractFromJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: JSON_STRING = &
   '{' // NEWLINE &
// '    "fraction" : 1.0,' // NEWLINE &
// '    "isotope" : "H-1",' // NEWLINE &
// '    "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '}'
        type(ErrorList_t) :: errors
        type(Isotope_t) :: isotope
        type(JsonElement_t) :: json

        call parseJson(JSON_STRING, errors, json)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            select type (object => json%element)
            type is (JsonObject_t)
                call fromJson(object, errors, isotope)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    result_ = assertEquals(H_1%toString(), isotope%toString())
                end if
            class default
                result_ = fail("Didn't get an object: " // object%toCompactString())
            end select
        end if
    end function checkExtractFromJson
end module isotope_json_test
