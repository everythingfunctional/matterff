module chemical_test
    use Chemical_m, only: &
            Chemical_t, &
            combineByAtomFactors, &
            combineByWeightFactors, &
            find, &
            makeChemical, &
            naturalHydrogenGas, &
            naturalHeliumGas, &
            naturalWater
    use Chemical_component_m, only: ChemicalComponent_t, ChemicalComponent
    use Chemical_symbol_m, only: hydrogenGasSymbol, heliumGasSymbol, waterSymbol
    use Element_m, only: &
            Element_t, fromAtomFractions, naturalHydrogen, naturalHelium
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: H, O
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: H_1, H_2
    use Utilities_m, only: MISMATCH_TYPE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, Describe, fail, It

    implicit none
    private

    public :: test_chemical
contains
    function test_chemical() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(9)

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
        individual_tests(5) = It( &
                "Combining chemicals of different types is an error", &
                checkCombineError)
        individual_tests(6) = It( &
                "Combining chemicals results in correct fractios", checkCombine)
        individual_tests(7) = It( &
                "Water is 2/3 hydrogen and 1/3 oxygen by atom", &
                checkWaterFractions)
        individual_tests(8) = It( &
                "Has a position of 0 if it's not in a list", checkNotFound)
        individual_tests(9) = It( &
                "Can be found in a list", checkFind)
        tests = Describe("Chemical_t", individual_tests)
    end function test_chemical

    pure function checkNotInSymbol() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: components(1)
        type(ErrorList_t) :: errors

        components(1) = ChemicalComponent(naturalHelium(), 1.0d0)
        call makeChemical( &
                hydrogenGasSymbol(), &
                components, &
                errors, &
                chemical)
        result_ = assertThat(errors.hasType.MISMATCH_TYPE, errors%toString())
    end function checkNotInSymbol

    pure function checkSingleElement() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: components(1)
        type(ErrorList_t) :: errors

        components(1) = ChemicalComponent(naturalHydrogen(), 2.0d0)
        call makeChemical( &
                hydrogenGasSymbol(), &
                components, &
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

        components(1) = ChemicalComponent(naturalHydrogen(), 1.0d0)
        components(2) = ChemicalComponent(naturalHydrogen(), 1.0d0)
        call makeChemical( &
                hydrogenGasSymbol(), &
                components, &
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

    pure function checkCombineError() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ErrorList_t) :: errors_from_atom_factors
        type(ErrorList_t) :: errors_from_weight_factors

        call combineByAtomFactors( &
                naturalHydrogenGas(), &
                1.0d0, &
                naturalHeliumGas(), &
                1.0d0, &
                errors_from_atom_factors, &
                chemical)
        call combineByWeightFactors( &
                naturalHydrogenGas(), &
                1.0d0, &
                naturalHeliumGas(), &
                1.0d0, &
                errors_from_weight_factors, &
                chemical)
        result_ = &
                assertThat( &
                        errors_from_atom_factors.hasType.MISMATCH_TYPE, &
                        errors_from_atom_factors%toString()) &
                .and.assertThat( &
                        errors_from_weight_factors.hasType.MISMATCH_TYPE, &
                        errors_from_weight_factors%toString())
    end function checkCombineError

    pure function checkCombine() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: combined_by_atom_factors
        type(Chemical_t) :: combined_by_weight_factors
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages
        type(Element_t) :: pure_H_1
        type(Chemical_t) :: pure_H_1_gas
        type(ChemicalComponent_t) :: pure_H_1_gas_components(1)
        type(Element_t) :: pure_H_2
        type(Chemical_t) :: pure_H_2_gas
        type(ChemicalComponent_t) :: pure_H_2_gas_components(1)

        call fromAtomFractions( &
                H, [ElementComponent(H_1, 1.0d0)], messages, errors, pure_H_1)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromAtomFractions( &
                    H, [ElementComponent(H_2, 1.0d0)], messages, errors, pure_H_2)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                pure_H_1_gas_components(1) = ChemicalComponent(pure_H_1, 2.0d0)
                pure_H_2_gas_components(1) = ChemicalComponent(pure_H_2, 2.0d0)
                call makeChemical( &
                        hydrogenGasSymbol(), &
                        pure_H_1_gas_components, &
                        errors, &
                        pure_H_1_gas)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    call makeChemical( &
                            hydrogenGasSymbol(), &
                            pure_H_2_gas_components, &
                            errors, &
                            pure_H_2_gas)
                    if (errors%hasAny()) then
                        result_ = fail(errors%toString())
                    else
                        call combineByAtomFactors( &
                                pure_H_1_gas, &
                                0.6d0, &
                                pure_H_2_gas, &
                                0.4d0, &
                                errors, &
                                combined_by_atom_factors)
                        if (errors%hasAny()) then
                            result_ = fail(errors%toString())
                        else
                            call combineByWeightFactors( &
                                    pure_H_1_gas, &
                                    0.6d0, &
                                    pure_H_2_gas, &
                                    0.4d0, &
                                    errors, &
                                    combined_by_weight_factors)
                            if (errors%hasAny()) then
                                result_ = fail(errors%toString())
                            else
                                result_ = &
                                        assertEquals( &
                                                0.6d0, &
                                                combined_by_atom_factors%atomFraction(H_1), &
                                                "H-1 atom fraction") &
                                        .and.assertEquals( &
                                                0.4d0, &
                                                combined_by_atom_factors%atomFraction(H_2), &
                                                "H-2 atom fraction") &
                                        .and.assertEquals( &
                                                0.6d0, &
                                                combined_by_weight_factors%weightFraction(H_1), &
                                                "H-1 weight fraction") &
                                        .and.assertEquals( &
                                                0.4d0, &
                                                combined_by_weight_factors%weightFraction(H_2), &
                                                "H-2 weight fraction")
                            end if
                        end if
                    end if
                end if
            end if
        end if
    end function checkCombine

    pure function checkWaterFractions() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: water

        water = naturalWater()
        result_ = &
                assertEquals( &
                        2.0d0 / 3.0d0, &
                        water%atomFraction(H)) &
                .and.assertEquals( &
                        1.0d0 / 3.0d0, &
                        water%atomFraction(O))
    end function checkWaterFractions

    pure function checkNotFound() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemicals(2)

        chemicals(1) = naturalHydrogenGas()
        chemicals(2) = naturalHeliumGas()

        result_ = assertEquals(0, find(waterSymbol(), chemicals))
    end function checkNotFound

    pure function checkFind() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemicals(3)

        chemicals(1) = naturalHydrogenGas()
        chemicals(2) = naturalHeliumGas()
        chemicals(3) = naturalWater()

        result_ = assertEquals(2, find(heliumGasSymbol(), chemicals))
    end function checkFind
end module chemical_test
