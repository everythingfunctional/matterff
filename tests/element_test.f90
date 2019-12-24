module element_test
    use Element_m, only: Element_t, fromAtomFractions
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: H
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: H_1, H_2, He_3
    use Utilities_m, only: &
            INVALID_ARGUMENT_TYPE, MISMATCH_TYPE, NORMALIZED_FRACTIONS_TYPE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, Describe, fail, It

    implicit none
    private

    public :: test_element
contains
    function test_element() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(7)

        individual_tests(1) = It( &
                "Creating an element with isotopes of a different element is an error", &
                checkDiffIsotopes)
        individual_tests(2) = It( &
                "Creating an element with negative fractions is an error", &
                checkNegativeFractions)
        individual_tests(3) = It( &
                "A single isotope element is all that isotope", &
                checkSingleIsotope)
        individual_tests(4) = It( &
                "Keeps track of its components", checkKeepsTrack)
        individual_tests(5) = It( &
                "Has normalized fractions of its components", &
                checkNormalizedFractions)
        individual_tests(6) = It( &
                "Normalizing fractions of isotopes produces a message", &
                checkNormalizedMessage)
        individual_tests(7) = It( &
                "Created with duplicate isotopes has sum of duplicates", &
                checkDuplicates)
        tests = Describe("Element_t", individual_tests)
    end function test_element

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
                errors.hasType.INVALID_ARGUMENT_TYPE, errors%toString())
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

    pure function checkKeepsTrack() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 0.6d0), ElementComponent(H_2, 0.4d0)], &
                messages, &
                errors, &
                element)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            result_ = &
                    assertEquals(0.6d0, element%atomFraction(H_1)) &
                    .and.assertEquals(0.4d0, element%atomFraction(H_2))
        end if
    end function checkKeepsTrack

    pure function checkNormalizedFractions() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 0.06d0), ElementComponent(H_2, 0.04d0)], &
                messages, &
                errors, &
                element)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            result_ = &
                    assertEquals(0.6d0, element%atomFraction(H_1)) &
                    .and.assertEquals(0.4d0, element%atomFraction(H_2))
        end if
    end function checkNormalizedFractions

    pure function checkNormalizedMessage() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 0.06d0), ElementComponent(H_2, 0.04d0)], &
                messages, &
                errors, &
                element)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            result_ = assertThat( &
                    messages.hasType.NORMALIZED_FRACTIONS_TYPE, &
                    messages%toString())
        end if
    end function checkNormalizedMessage

    pure function checkDuplicates() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 0.6d0), ElementComponent(H_1, 0.4d0)], &
                messages, &
                errors, &
                element)
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
    end function checkDuplicates
end module element_test
