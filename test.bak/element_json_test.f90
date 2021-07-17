module element_json_test
    use Element_m, only: Element_t, fromJson, naturalHydrogen
    use erloff, only: ErrorList_t, MessageList_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: JsonElement_t, JsonObject_t, parseJson
    use strff, only: NEWLINE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, Describe, fail, It

    implicit none
    private

    public :: test_element_json
contains
    function test_element_json() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "can be converted to JSON", checkConvertToJson)
        individual_tests(2) = It( &
                "can be extracted from JSON", checkExtractFromJson)
        tests = Describe("an element", individual_tests)
    end function test_element_json

    pure function checkConvertToJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '{' // NEwLINE &
// '    "multiplier" : 1.0,' // NEWLINE &
// '    "element" : "H",' // NEWLINE &
// '    "atomFractions" : [' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 0.99988500000000002,' // NEWLINE &
// '            "isotope" : "H-1",' // NEWLINE &
// '            "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '        },' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '            "isotope" : "H-2",' // NEWLINE &
// '            "atomicMass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '        }' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(Element_t) :: hydrogen
        type(JsonObject_t) :: json

        hydrogen = naturalHydrogen()
        json = hydrogen%toJsonWithMultiplier(1.0d0)
        result_ = assertEquals(EXPECTED, json%toExpandedString())
    end function checkConvertToJson

    pure function checkExtractFromJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: JSON_STRING = &
   '{' // NEwLINE &
// '    "multiplier" : 1.0,' // NEWLINE &
// '    "element" : "H",' // NEWLINE &
// '    "atomFractions" : [' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 0.99988500000000002,' // NEWLINE &
// '            "isotope" : "H-1",' // NEWLINE &
// '            "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '        },' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '            "isotope" : "H-2",' // NEWLINE &
// '            "atomicMass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '        }' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(JsonElement_t) :: json
        type(MessageList_t) :: messages
        type(JsonObject_t) :: new_json

        call parseJson(JSON_STRING, errors, json)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            select type (object => json%element)
            type is (JsonObject_t)
                call fromJson(object, messages, errors, element)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    new_json = element%toJsonWithMultiplier(1.0d0)
                    result_ = assertEquals(JSON_STRING, new_json%toExpandedString())
                end if
            class default
                result_ = fail("Didn't get an object: " // object%toCompactString())
            end select
        end if
    end function checkExtractFromJson
end module element_json_test
