module chemical_test
    use Chemical_m, only: Chemical_t, makeChemical
    use Chemical_component_m, only: ChemicalComponent_t, ChemicalComponent
    use Chemical_symbol_m, only: hydrogenGasSymbol
    use Element_m, only: &
            Element_t, fromAtomFractions, naturalHydrogen, naturalHelium
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: H
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: H_1
    use Utilities_m, only: MISMATCH_TYPE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, Describe, fail, It

    implicit none
    private

    public :: test_chemical
contains
    function test_chemical() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = It( &
                "Creating a chemical with elements not included in the symbol is an error", &
                checkNotInSymbol)
        individual_tests(2) = It( &
                "A single element chemical is all that element", &
                checkSingleElement)
        individual_tests(3) = It( &
                "A single isotope chemical is all that isotope", &
                checkSingleIsotope)
        individual_tests(4) = It( &
                "Created with duplicate elements has sum of duplicates", &
                checkDuplicate)
        tests = Describe("Chemical_t", individual_tests)
    end function test_chemical

    pure function checkNotInSymbol() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: components(1)
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        components(1) = ChemicalComponent(naturalHelium(), 1.0d0)
        call makeChemical( &
                hydrogenGasSymbol(), &
                components, &
                messages, &
                errors, &
                chemical)
        result_ = assertThat(errors.hasType.MISMATCH_TYPE, errors%toString())
    end function checkNotInSymbol

    pure function checkSingleElement() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: components(1)
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        components(1) = ChemicalComponent(naturalHydrogen(), 2.0d0)
        call makeChemical( &
                hydrogenGasSymbol(), &
                components, &
                messages, &
                errors, &
                chemical)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            result_ = &
                    assertEquals( &
                            1.0d0, &
                            chemical%atomFraction(H), &
                            "atom fraction") &
                    .and.assertEquals( &
                            1.0d0, &
                            chemical%weightFraction(H), &
                            "weight fraction")
        end if
    end function checkSingleElement

    pure function checkSingleIsotope() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: components(1)
        type(ErrorList_t) :: errors
        type(Element_t) :: hydrogen
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, [ElementComponent(H_1, 1.0d0)], messages, errors, hydrogen)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            components(1) = ChemicalComponent(hydrogen, 2.0d0)
            call makeChemical( &
                    hydrogenGasSymbol(), &
                    components, &
                    messages, &
                    errors, &
                    chemical)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ = &
                        assertEquals( &
                                1.0d0, &
                                chemical%atomFraction(H_1), &
                                "atom fraction") &
                        .and.assertEquals( &
                                1.0d0, &
                                chemical%weightFraction(H_1), &
                                "weight fraction")
            end if
        end if
    end function  checkSingleIsotope

    pure function checkDuplicate() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: components(2)
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages

        components(1) = ChemicalComponent(naturalHydrogen(), 1.0d0)
        components(2) = ChemicalComponent(naturalHydrogen(), 1.0d0)
        call makeChemical( &
                hydrogenGasSymbol(), &
                components, &
                messages, &
                errors, &
                chemical)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            result_ = &
                    assertEquals( &
                            1.0d0, &
                            chemical%atomFraction(H), &
                            "atom fraction") &
                    .and.assertEquals( &
                            1.0d0, &
                            chemical%weightFraction(H), &
                            "weight fraction")
        end if
    end function checkDuplicate
end module chemical_test
