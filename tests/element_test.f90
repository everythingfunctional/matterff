module element_test
    use Element_m, only: Element_t, fromAtomFractions
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: H
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: H_1, He_3
    use Utilities_m, only: INVALID_ARGUMENT, MISMATCH_TYPE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, Describe, fail, It

    implicit none
    private

    public :: test_element
contains
    function test_element() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = It( &
                "Doesn't contain any isotopes when it's empty", checkEmpty)
        individual_tests(2) = It( &
                "Creating an element with isotopes of a different element is an error", &
                checkDiffIsotopes)
        individual_tests(3) = It( &
                "Creating an element with negative fractions is an error", &
                checkNegativeFractions)
        individual_tests(4) = It( &
                "A single isotope element is all that isotope", &
                checkSingleIsotope)
        tests = Describe("Element_t", individual_tests)
    end function test_element

    pure function checkEmpty() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: empty

        result_ = &
                assertEquals( &
                        0.0d0, empty%atomFraction(H_1), "atom fraction") &
                .and.assertEquals( &
                        0.0d0, empty%weightFraction(H_1), "weight fraction")
    end function checkEmpty

    pure function checkDiffIsotopes() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, [ElementComponent(He_3, 1.0d0)], messages, errors, element)

        result_ = assertThat( &
                errors.hasType.MISMATCH_TYPE, errors%toString())
    end function checkDiffIsotopes

    pure function checkNegativeFractions() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, [ElementComponent(H_1, -1.0d0)], messages, errors, element)

        result_ = assertThat( &
                errors.hasType.INVALID_ARGUMENT, errors%toString())
    end function checkNegativeFractions

    pure function checkSingleIsotope() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, [ElementComponent(H_1, 1.0d0)], messages, errors, element)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            result_ = &
                    assertEquals( &
                            1.0d0, &
                            element%atomFraction(H_1), &
                            "atom fraction from atom fraction") &
                    .and.assertEquals( &
                            1.0d0, &
                            element%weightFraction(H_1), &
                            "weight fraction from atom fraction")
        end if
    end function checkSingleIsotope
end module element_test
