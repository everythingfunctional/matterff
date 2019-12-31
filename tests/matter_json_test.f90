module matter_json_test
    use erloff, only: ErrorList_t
    use jsonff, only: JsonObject_t
    use Material_m, only: pureNaturalHydrogenGas
    use Matter_m, only: Matter_t, createMatter
    use quaff, only: operator(.unit.), MOLS
    use strff, only: NEWLINE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, Describe, fail, It

    implicit none
    private

    public :: test_matter_json
contains
    function test_matter_json() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "can be converted to JSON", checkConvertToJson)
        tests = Describe("some matter", individual_tests)
    end function test_matter_json

    pure function checkConvertToJson() result(result_)
        type(Result_t) :: result_

        character(len=*), parameter :: EXPECTED = &
   '{' // NEWLINE &
// '    "amount" : "1.0 mol",' // NEWLINE &
// '    "material" : {' // NEWLINE &
// '        "atomFractions" : [' // NEWLINE &
// '            {' // NEWLINE &
// '                "fraction" : 1.0,' // NEWLINE &
// '                "chemical" : [' // NEWLINE &
// '                    {' // NEWLINE &
// '                        "element" : "H",' // NEWLINE &
// '                        "multiple" : 2.0' // NEWLINE &
// '                    }' // NEWLINE &
// '                ],' // NEWLINE &
// '                "elements" : [' // NEWLINE &
// '                    {' // NEWLINE &
// '                        "multiplier" : 2.0,' // NEWLINE &
// '                        "element" : "H",' // NEWLINE &
// '                        "atomFractions" : [' // NEWLINE &
// '                            {' // NEWLINE &
// '                                "fraction" : 0.99988500000000002,' // NEWLINE &
// '                                "isotope" : "H-1",' // NEWLINE &
// '                                "atomicMass" : "1.0078250321e-3 kg/mol"' // NEWLINE &
// '                            },' // NEWLINE &
// '                            {' // NEWLINE &
// '                                "fraction" : 1.1500000000000001e-4,' // NEWLINE &
// '                                "isotope" : "H-2",' // NEWLINE &
// '                                "atomicMass" : "2.0141017779000001e-3 kg/mol"' // NEWLINE &
// '                            }' // NEWLINE &
// '                        ]' // NEWLINE &
// '                    }' // NEWLINE &
// '                ]' // NEWLINE &
// '            }' // NEWLINE &
// '        ]' // NEWLINE &
// '    }' // NEWLINE &
// '}'
        type(ErrorList_t) :: errors
        type(JsonObject_t) :: json
        type(Matter_t) :: matter

        call createMatter(1.0d0.unit.MOLS, pureNaturalHydrogenGas(), errors, matter)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            json = matter%toJson()
            result_ = assertEquals(EXPECTED, json%toExpandedString())
        end if
    end function checkConvertToJson
end module matter_json_test
