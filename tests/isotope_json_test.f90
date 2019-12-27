module isotope_json_test
    use Isotope_m, only: H_1
    use jsonff, only: JsonObject_t
    use strff, only: NEWLINE
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, Describe, It

    implicit none
    private

    public :: test_isotope_json
contains
    function test_isotope_json() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "can be converted to JSON", checkConvertToJson)
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
end module isotope_json_test
