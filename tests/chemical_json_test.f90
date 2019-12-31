module chemical_json_test
    use Chemical_m, only: Chemical_t, fromJson, naturalHydrogenGas
    use erloff, only: ErrorList_t, MessageList_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: JsonElement_t, JsonObject_t, parseJson
    use strff, only: NEWLINE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, Describe, fail, It

    implicit none
    private

    public :: test_chemical_json
contains
    function test_chemical_json() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "can be converted to JSON", checkConvertToJson)
        individual_tests(2) = It( &
                "can be extracted from JSON", checkExtractFromJson)
        tests = Describe("a chemical", individual_tests)
    end function test_chemical_json

    pure function checkConvertToJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '{' // NEWLINE &
// '    "fraction" : 1.0,' // NEWLINE &
// '    "chemical" : [' // NEWLINE &
// '        {' // NEWLINE &
// '            "element" : "H",' // NEWLINE &
// '            "multiple" : 2.0' // NEWLINE &
// '        }' // NEWLINE &
// '    ],' // NEWLINE &
// '    "elements" : [' // NEWLINE &
// '        {' // NEwLINE &
// '            "multiplier" : 2.0,' // NEWLINE &
// '            "element" : "H",' // NEWLINE &
// '            "atomFractions" : [' // NEWLINE &
// '                {' // NEWLINE &
// '                    "fraction" : 0.99988500000000002,' // NEWLINE &
// '                    "isotope" : "H-1",' // NEWLINE &
// '                    "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '                },' // NEWLINE &
// '                {' // NEWLINE &
// '                    "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '                    "isotope" : "H-2",' // NEWLINE &
// '                    "atomicMass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '                }' // NEWLINE &
// '            ]' // NEWLINE &
// '        }' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(Chemical_t) :: hydrogen
        type(JsonObject_t) :: json

        hydrogen = naturalHydrogenGas()
        json = hydrogen%toJsonWithFraction(1.0d0)
        result_ = assertEquals(EXPECTED, json%toExpandedString())
    end function checkConvertToJson

    pure function checkExtractFromJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: JSON_STRING = &
   '{' // NEWLINE &
// '    "fraction" : 1.0,' // NEWLINE &
// '    "chemical" : [' // NEWLINE &
// '        {' // NEWLINE &
// '            "element" : "H",' // NEWLINE &
// '            "multiple" : 2.0' // NEWLINE &
// '        }' // NEWLINE &
// '    ],' // NEWLINE &
// '    "elements" : [' // NEWLINE &
// '        {' // NEwLINE &
// '            "multiplier" : 2.0,' // NEWLINE &
// '            "element" : "H",' // NEWLINE &
// '            "atomFractions" : [' // NEWLINE &
// '                {' // NEWLINE &
// '                    "fraction" : 0.99988500000000002,' // NEWLINE &
// '                    "isotope" : "H-1",' // NEWLINE &
// '                    "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '                },' // NEWLINE &
// '                {' // NEWLINE &
// '                    "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '                    "isotope" : "H-2",' // NEWLINE &
// '                    "atomicMass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '                }' // NEWLINE &
// '            ]' // NEWLINE &
// '        }' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(Chemical_t) :: chemical
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
                call fromJson(object, messages, errors, chemical)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    new_json = chemical%toJsonWithFraction(1.0d0)
                    result_ = assertEquals(JSON_STRING, new_json%toExpandedString())
                end if
            class default
                result_ = fail("Didn't get an object: " // object%toCompactString())
            end select
        end if
    end function checkExtractFromJson
end module chemical_json_test
