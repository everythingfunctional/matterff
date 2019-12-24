module element_test
    use Element_m, only: &
            Element_t, &
            combineByAtomFactors, &
            combineByWeightFactors, &
            fromAtomFractions, &
            fromWeightFractions, &
            naturalHydrogen, &
            naturalHelium, &
            naturalLithium, &
            naturalBeryllium
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: H
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: Isotope_t, H_1, H_2, He_3
    use quaff, only: operator(.unit.), GRAMS_PER_MOL
    use quaff_asserts_m, only: assertEqualsWithinAbsolute
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

        type(TestItem_t) :: individual_tests(10)

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
        individual_tests(8) = It( &
                "Combining elements of a different type is an error", &
                checkCombineDifferentElementsIsError)
        individual_tests(9) = It( &
                "Combining elements results in correct fractions", checkCombine)
        individual_tests(10) = It( &
                "Natural compositions have their atomic mass from the Periodic Table", &
                checkNaturalElements)
        tests = Describe("Element_t", individual_tests)
    end function test_element

    pure function checkDiffIsotopes() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors_from_atom_fractions
        type(ErrorList_t) :: errors_from_weight_fractions
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(He_3, 1.0d0)], &
                messages, &
                errors_from_atom_fractions, &
                element)
        call fromWeightFractions( &
                H, &
                [ElementComponent(He_3, 1.0d0)], &
                messages, &
                errors_from_weight_fractions, &
                element)

        result_ = &
                assertThat( &
                        errors_from_atom_fractions.hasType.MISMATCH_TYPE, &
                        errors_from_atom_fractions%toString()) &
                .and.assertThat( &
                        errors_from_weight_fractions.hasType.MISMATCH_TYPE, &
                        errors_from_weight_fractions%toString())
    end function checkDiffIsotopes

    pure function checkNegativeFractions() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors_from_atom_fractions
        type(ErrorList_t) :: errors_from_weight_fractions
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, -1.0d0)], &
                messages, &
                errors_from_atom_fractions, &
                element)
        call fromWeightFractions( &
                H, &
                [ElementComponent(H_1, -1.0d0)], &
                messages, &
                errors_from_weight_fractions, &
                element)

        result_ = &
                assertThat( &
                        errors_from_atom_fractions.hasType.INVALID_ARGUMENT_TYPE, &
                        errors_from_atom_fractions%toString()) &
                .and.assertThat( &
                        errors_from_weight_fractions.hasType.INVALID_ARGUMENT_TYPE, &
                        errors_from_weight_fractions%toString())
    end function checkNegativeFractions

    pure function checkSingleIsotope() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Element_t) :: from_atom_fractions
        type(Element_t) :: from_weight_fractions
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 1.0d0)], &
                messages, &
                errors, &
                from_atom_fractions)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    H, &
                    [ElementComponent(H_1, 1.0d0)], &
                    messages, &
                    errors, &
                    from_weight_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ =  &
                        assertAllIsotope( &
                                H_1, from_atom_fractions, "atom fractions") &
                        .and.assertAllIsotope( &
                                H_1, from_weight_fractions, "weight fractions")
            end if
        end if
    end function checkSingleIsotope

    pure function checkKeepsTrack() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: from_atom_fractions
        type(Element_t) :: from_weight_fractions
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 0.6d0), ElementComponent(H_2, 0.4d0)], &
                messages, &
                errors, &
                from_atom_fractions)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    H, &
                    [ElementComponent(H_1, 0.6d0), ElementComponent(H_2, 0.4d0)], &
                    messages, &
                    errors, &
                    from_weight_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ = &
                        assertEquals( &
                                0.6d0, &
                                from_atom_fractions%atomFraction(H_1), &
                                "H-1 atom fraction") &
                        .and.assertEquals( &
                                0.4d0, &
                                from_atom_fractions%atomFraction(H_2), &
                                "H-2 atom fraction") &
                        .and.assertEquals( &
                                0.6d0, &
                                from_weight_fractions%weightFraction(H_1), &
                                "H-1 weight fraction") &
                        .and.assertEquals( &
                                0.4d0, &
                                from_weight_fractions%weightFraction(H_2), &
                                "H-2 weight fraction")
            end if
        end if
    end function checkKeepsTrack

    pure function checkNormalizedFractions() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: from_atom_fractions
        type(Element_t) :: from_weight_fractions
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 0.06d0), ElementComponent(H_2, 0.04d0)], &
                messages, &
                errors, &
                from_atom_fractions)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    H, &
                    [ElementComponent(H_1, 0.06d0), ElementComponent(H_2, 0.04d0)], &
                    messages, &
                    errors, &
                    from_weight_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ = &
                        assertEquals( &
                                0.6d0, &
                                from_atom_fractions%atomFraction(H_1), &
                                "H-1 atom fraction") &
                        .and.assertEquals( &
                                0.4d0, &
                                from_atom_fractions%atomFraction(H_2), &
                                "H-2 atom fraction") &
                        .and.assertEquals( &
                                0.6d0, &
                                from_weight_fractions%weightFraction(H_1), &
                                "H-1 weight fraction") &
                        .and.assertEquals( &
                                0.4d0, &
                                from_weight_fractions%weightFraction(H_2), &
                                "H-2 weight fraction")
            end if
        end if
    end function checkNormalizedFractions

    pure function checkNormalizedMessage() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages_from_atom_fractions
        type(MessageList_t) :: messages_from_weight_fractions

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 0.06d0), ElementComponent(H_2, 0.04d0)], &
                messages_from_atom_fractions, &
                errors, &
                element)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    H, &
                    [ElementComponent(H_1, 0.06d0), ElementComponent(H_2, 0.04d0)], &
                    messages_from_weight_fractions, &
                    errors, &
                    element)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ = &
                        assertThat( &
                                messages_from_atom_fractions.hasType.NORMALIZED_FRACTIONS_TYPE, &
                                messages_from_atom_fractions%toString()) &
                        .and.assertThat( &
                                messages_from_weight_fractions.hasType.NORMALIZED_FRACTIONS_TYPE, &
                                messages_from_weight_fractions%toString())
            end if
        end if
    end function checkNormalizedMessage

    pure function checkDuplicates() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: from_atom_fractions
        type(Element_t) :: from_weight_fractions
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, &
                [ElementComponent(H_1, 0.6d0), ElementComponent(H_1, 0.4d0)], &
                messages, &
                errors, &
                from_atom_fractions)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    H, &
                    [ElementComponent(H_1, 0.6d0), ElementComponent(H_1, 0.4d0)], &
                    messages, &
                    errors, &
                    from_weight_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ =  &
                        assertAllIsotope( &
                                H_1, from_atom_fractions, "atom fractions") &
                        .and.assertAllIsotope( &
                                H_1, from_weight_fractions, "weight fractions")
            end if
        end if
    end function checkDuplicates

    pure function checkCombineDifferentElementsIsError() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: combined
        type(ErrorList_t) :: errors_from_atom_fractions
        type(ErrorList_t) :: errors_from_weight_fractions
        type(MessageList_t) :: messages

        call combineByAtomFactors( &
                naturalHydrogen(), &
                1.0d0, &
                naturalHelium(), &
                1.0d0, &
                messages, &
                errors_from_atom_fractions, &
                combined)
        call combineByWeightFactors( &
                naturalHydrogen(), &
                1.0d0, &
                naturalHelium(), &
                1.0d0, &
                messages, &
                errors_from_weight_fractions, &
                combined)
        result_ = &
                assertThat( &
                        errors_from_atom_fractions.hasType.MISMATCH_TYPE, &
                        errors_from_atom_fractions%toString()) &
                .and.assertThat( &
                        errors_from_weight_fractions.hasType.MISMATCH_TYPE, &
                        errors_from_weight_fractions%toString())
    end function checkCombineDifferentElementsIsError

    pure function checkCombine() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Element_t) :: from_atom_factors
        type(Element_t) :: from_weight_factors
        type(MessageList_t) :: messages
        type(Element_t) :: pure_H_1
        type(Element_t) :: pure_H_2

        call fromAtomFractions(H, [ElementComponent(H_1, 1.0d0)], messages, errors, pure_H_1)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromAtomFractions(H, [ElementComponent(H_2, 1.0d0)], messages, errors, pure_H_2)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                call combineByAtomFactors(pure_H_1, 0.6d0, pure_H_2, 0.4d0, messages, errors, from_atom_factors)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    call combineByWeightFactors(pure_H_1, 0.6d0, pure_H_2, 0.4d0, messages, errors, from_weight_factors)
                    if (errors%hasAny()) then
                        result_ = fail(errors%toString())
                    else
                        result_ = &
                                assertEquals( &
                                        0.6d0, &
                                        from_atom_factors%atomFraction(H_1), &
                                        "H-1 atom fraction") &
                                .and.assertEquals( &
                                        0.4d0, &
                                        from_atom_factors%atomFraction(H_2), &
                                        "H-2 atom fraction") &
                                .and.assertEquals( &
                                        0.6d0, &
                                        from_weight_factors%weightFraction(H_1), &
                                        "H-1 weight fraction") &
                                .and.assertEquals( &
                                        0.4d0, &
                                        from_weight_factors%weightFraction(H_2), &
                                        "H-2 weight fraction")
                    end if
                end if
            end if
        end if
    end function checkCombine

    pure function checkNaturalElements() result(result_)
        type(Result_t) :: result_

        type(Element_t) :: hydrogen
        type(Element_t) :: helium
        type(Element_t) :: lithium
        type(Element_t) :: beryllium

        hydrogen = naturalHydrogen()
        helium = naturalHelium()
        lithium = naturalLithium()
        beryllium = naturalBeryllium()

        ! Atomic masses and tolerances are taken from the 17th Edition of the Chart of Nuclides
        result_ = &
                assertEqualsWithinAbsolute( &
                        1.00794d0.unit.GRAMS_PER_MOL, &
                        hydrogen%atomicMass(), &
                        0.00007d0.unit.GRAMS_PER_MOL, &
                        "H") &
                .and.assertEqualsWithinAbsolute( &
                        4.002602d0.unit.GRAMS_PER_MOL, &
                        helium%atomicMass(), &
                        0.000002d0.unit.GRAMS_PER_MOL, &
                        "He") &
                .and.assertEqualsWithinAbsolute( &
                        6.941d0.unit.GRAMS_PER_MOL, &
                        lithium%atomicMass(), &
                        0.002d0.unit.GRAMS_PER_MOL, &
                        "Li") &
                .and.assertEqualsWithinAbsolute( &
                        9.012182d0.unit.GRAMS_PER_MOL, &
                        beryllium%atomicMass(), &
                        0.000003d0.unit.GRAMS_PER_MOL, &
                        "Be")
    end function checkNaturalElements

    pure function assertAllIsotope(isotope, element, from) result(result_)
        type(Isotope_t), intent(in) :: isotope
        type(Element_t), intent(in) :: element
        character(len=*), intent(in) :: from
        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        1.0d0, &
                        element%atomFraction(isotope), &
                        "atom fraction from " // from) &
                .and.assertEquals( &
                        1.0d0, &
                        element%weightFraction(isotope), &
                        "weight fraction from " // from)
    end function assertAllIsotope
end module element_test
