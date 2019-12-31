module material_json_test
    use erloff, only: ErrorList_t, MessageList_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: JsonElement_t, JsonObject_t, parseJson
    use Material_m, only: Material_t, fromJson, pureNaturalHydrogenGas
    use strff, only: NEWLINE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, Describe, fail, It

    implicit none
    private

    public :: test_material_json
contains
    function test_material_json() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "can be converted to JSON", checkConvertToJson)
        individual_tests(2) = It( &
                "can be extracted from JSON", checkExtractFromJson)
        tests = Describe("a material", individual_tests)
    end function test_material_json

    pure function checkConvertToJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '{' // NEWLINE &
// '    "atomFractions" : [' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 1.0,' // NEWLINE &
// '            "chemical" : [' // NEWLINE &
// '                {' // NEWLINE &
// '                    "element" : "H",' // NEWLINE &
// '                    "multiple" : 2.0' // NEWLINE &
// '                }' // NEWLINE &
// '            ],' // NEWLINE &
// '            "elements" : [' // NEWLINE &
// '                {' // NEWLINE &
// '                    "multiplier" : 2.0,' // NEWLINE &
// '                    "element" : "H",' // NEWLINE &
// '                    "atomFractions" : [' // NEWLINE &
// '                        {' // NEWLINE &
// '                            "fraction" : 0.99988500000000002,' // NEWLINE &
// '                            "isotope" : "H-1",' // NEWLINE &
// '                            "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '                        },' // NEWLINE &
// '                        {' // NEWLINE &
// '                            "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '                            "isotope" : "H-2",' // NEWLINE &
// '                            "atomicMass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '                        }' // NEWLINE &
// '                    ]' // NEWLINE &
// '                }' // NEWLINE &
// '            ]' // NEWLINE &
// '        }' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(Material_t) :: hydrogen
        type(JsonObject_t) :: json

        hydrogen = pureNaturalHydrogenGas()
        json = hydrogen%toJson()
        result_ = assertEquals(EXPECTED, json%toExpandedString())
    end function checkConvertToJson

    pure function checkExtractFromJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: JSON_STRING = &
   '{' // NEWLINE &
// '    "atomFractions" : [' // NEWLINE &
// '        {' // NEWLINE &
// '            "fraction" : 1.0,' // NEWLINE &
// '            "chemical" : [' // NEWLINE &
// '                {' // NEWLINE &
// '                    "element" : "H",' // NEWLINE &
// '                    "multiple" : 2.0' // NEWLINE &
// '                }' // NEWLINE &
// '            ],' // NEWLINE &
// '            "elements" : [' // NEWLINE &
// '                {' // NEWLINE &
// '                    "multiplier" : 2.0,' // NEWLINE &
// '                    "element" : "H",' // NEWLINE &
// '                    "atomFractions" : [' // NEWLINE &
// '                        {' // NEWLINE &
// '                            "fraction" : 0.99988500000000002,' // NEWLINE &
// '                            "isotope" : "H-1",' // NEWLINE &
// '                            "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '                        },' // NEWLINE &
// '                        {' // NEWLINE &
// '                            "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '                            "isotope" : "H-2",' // NEWLINE &
// '                            "atomicMass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '                        }' // NEWLINE &
// '                    ]' // NEWLINE &
// '                }' // NEWLINE &
// '            ]' // NEWLINE &
// '        }' // NEWLINE &
// '    ]' // NEWLINE &
// '}'
        type(ErrorList_t) :: errors
        type(JsonElement_t) :: json
        type(Material_t) :: material
        type(MessageList_t) :: messages
        type(JsonObject_t) :: new_json

        call parseJson(JSON_STRING, errors, json)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            select type (object => json%element)
            type is (JsonObject_t)
                call fromJson(object, messages, errors, material)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    new_json = material%toJson()
                    result_ = assertEquals(JSON_STRING, new_json%toExpandedString())
                end if
            class default
                result_ = fail("Didn't get an object: " // object%toCompactString())
            end select
        end if
    end function checkExtractFromJson
end module material_json_test
