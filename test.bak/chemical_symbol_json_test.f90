module chemical_symbol_json_test
    use Chemical_symbol_m, only: ChemicalSymbol_t, fromJson, hydrogenGasSymbol
    use erloff, only: ErrorList_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: JsonArray_t, JsonElement_t, parseJson
    use strff, only: NEWLINE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, Describe, fail, It

    implicit none
    private

    public :: test_chemical_symbol_json
contains
    function test_chemical_symbol_json() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "can be converted to JSON", checkConvertToJson)
        individual_tests(2) = It( &
                "can be extracted from JSON", checkExtractFromJson)
        tests = Describe("A chemical symbol", individual_tests)
    end function test_chemical_symbol_json

    pure function checkConvertToJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '[' // NEWLINE &
// '    {' // NEWLINE &
// '        "element" : "H",' // NEWLINE &
// '        "multiple" : 2.0' // NEWLINE &
// '    }' // NEWLINE &
// ']'
        type(JsonArray_t) :: json
        type(ChemicalSymbol_t) :: symbol

        symbol = hydrogenGasSymbol()
        json = symbol%toJson()
        result_ = assertEquals(EXPECTED, json%toExpandedString())
    end function checkConvertToJson

    pure function checkExtractFromJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: JSON_STRING = &
   '[' // NEWLINE &
// '    {' // NEWLINE &
// '        "element" : "H",' // NEWLINE &
// '        "multiple" : 2.0' // NEWLINE &
// '    },' // NEWLINE &
// '    {' // NEWLINE &
// '        "element" : "O",' // NEWLINE &
// '        "multiple" : 1.0' // NEWLINE &
// '    }' // NEWLINE &
// ']'
        type(ErrorList_t) :: errors
        type(JsonElement_t) :: json
        type(JsonArray_t) :: new_json
        type(ChemicalSymbol_t) :: symbol

        call parseJson(JSON_STRING, errors, json)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            select type (array => json%element)
            type is (JsonArray_t)
                call fromJson(array, errors, symbol)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    new_json = symbol%toJson()
                    result_ = assertEquals(JSON_STRING, new_json%toExpandedString())
                end if
            class default
                result_ = fail("Didn't get an array: " // array%toCompactString())
            end select
        end if
    end function checkExtractFromJson
end module chemical_symbol_json_test
