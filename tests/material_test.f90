module material_test
    use Chemical_m, only: &
            Chemical_t, &
            makeChemical, &
            naturalHeliumGas, &
            naturalHydrogenGas, &
            naturalWater
    use Chemical_component_m, only: ChemicalComponent_t, ChemicalComponent
    use Chemical_symbol_m, only: &
            ChemicalSymbol_t, heliumGasSymbol, hydrogenGasSymbol, waterSymbol
    use Element_m, only: Element_t, fromAtomFractions, naturalHydrogen
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: ElementSymbol_t, H
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: Isotope_t, H_1
    use Material_m, only: &
            Material_t, &
            combineByAtomFactors, &
            combineByWeightFactors, &
            fromAtomFractions, &
            fromWeightFractions
    use Material_component_m, only: MaterialComponent_t, MaterialComponent
    use quaff_asserts_m, only: assertEquals
    use Utilities_m, only: INVALID_ARGUMENT_TYPE, NORMALIZED_FRACTIONS_TYPE
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, Describe, fail, It

    implicit none
    private

    public :: test_material
contains
    function test_material() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(10)

        individual_tests(1) = It( &
                "Creating a material with negative fractions is an error", &
                checkNegativeFractions)
        individual_tests(2) = It( &
                "A single isotope material is all that isotope", &
                checkSingleIsotope)
        individual_tests(3) = It( &
                "A single element material is all that element", &
                checkSingleElement)
        individual_tests(4) = It( &
                "A single chemical material is all that chemical", &
                checkSingleChemical)
        individual_tests(5) = It( &
                "A single chemical material has the same molar mass", &
                checkSingleChemicalMolarMass)
        individual_tests(6) = It( &
                "Keeps track of its components", checkKeepsTrack)
        individual_tests(7) = It( &
                "Has normalized fractions of its components", &
                checkNormalizedFractions)
        individual_tests(8) = It( &
                "Normalizing fractions of chemicals produces a message", &
                checkNormalizedMessages)
        individual_tests(9) = It( &
                "Created with duplicate chemicals has sum of duplicates", &
                checkDuplicates)
        individual_tests(10) = It( &
                "Combining materials results in correct fractions", checkCombine)
        tests = Describe("Material_t", individual_tests)
    end function test_material

    pure function checkNegativeFractions() result(result_)
        type(Result_t) :: result_

        type(MaterialComponent_t) :: components(1)
        type(Material_t) :: material
        type(ErrorList_t) :: errors_from_atom_fractions
        type(ErrorList_t) :: errors_from_weight_fractions
        type(MessageList_t) :: messages

        components(1) = MaterialComponent(naturalHydrogenGas(), -1.0d0)
        call fromAtomFractions( &
                components, &
                messages, &
                errors_from_atom_fractions, &
                material)
        call fromWeightFractions( &
                components, &
                messages, &
                errors_from_weight_fractions, &
                material)

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

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: chemical_components(1)
        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(Material_t) :: from_atom_fractions
        type(Material_t) :: from_weight_fractions
        type(MaterialComponent_t) :: material_components(1)
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                H, [ElementComponent(H_1, 1.0d0)], messages, errors, element)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            chemical_components(1) = ChemicalComponent(element, 2.0d0)
            call makeChemical( &
                    hydrogenGasSymbol(), chemical_components, errors, chemical)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                material_components(1) = MaterialComponent(chemical, 1.0d0)
                call fromWeightFractions( &
                        material_components, &
                        messages, &
                        errors, &
                        from_atom_fractions)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    call fromAtomFractions( &
                            material_components, &
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
            end if
        end if
    end function checkSingleIsotope

    pure function checkSingleElement() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: chemical_components(1)
        type(ErrorList_t) :: errors
        type(Material_t) :: from_atom_fractions
        type(Material_t) :: from_weight_fractions
        type(MaterialComponent_t) :: material_components(1)
        type(MessageList_t) :: messages

        chemical_components(1) = ChemicalComponent(naturalHydrogen(), 2.0d0)
        call makeChemical( &
                hydrogenGasSymbol(), chemical_components, errors, chemical)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            material_components(1) = MaterialComponent(chemical, 1.0d0)
            call fromAtomFractions( &
                    material_components, &
                    messages, &
                    errors, &
                    from_atom_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                call fromWeightFractions( &
                        material_components, &
                        messages, &
                        errors, &
                        from_weight_fractions)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    result_ =  &
                            assertAllElement( &
                                    H, from_atom_fractions, "atom fractions") &
                            .and.assertAllElement( &
                                    H, from_weight_fractions, "weight fractions")
                end if
            end if
        end if
    end function checkSingleElement

    pure function checkSingleChemical() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Material_t) :: from_atom_fractions
        type(Material_t) :: from_weight_fractions
        type(MaterialComponent_t) :: material_components(1)
        type(MessageList_t) :: messages

        material_components(1) = MaterialComponent(naturalHydrogenGas(), 1.0d0)
        call fromAtomFractions( &
                material_components, &
                messages, &
                errors, &
                from_atom_fractions)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    material_components, &
                    messages, &
                    errors, &
                    from_weight_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ =  &
                        assertAllChemical( &
                                hydrogenGasSymbol(), from_atom_fractions, "atom fractions") &
                        .and.assertAllChemical( &
                                hydrogenGasSymbol(), from_weight_fractions, "weight fractions")
            end if
        end if
    end function checkSingleChemical

    pure function checkSingleChemicalMolarMass() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(MaterialComponent_t) :: components(1)
        type(ErrorList_t) :: errors
        type(Material_t) :: material
        type(MessageList_t) :: messages

        chemical = naturalWater()
        components(1) = MaterialComponent(chemical, 1.0d0)
        call fromAtomFractions(components, messages, errors, material)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            result_ = assertEquals(chemical%molarMass(), material%molarMass())
        end if
    end function checkSingleChemicalMolarMass

    pure function checkKeepsTrack() result(result_)
        type(Result_t) :: result_

        type(MaterialComponent_t) :: components(2)
        type(ErrorList_t) :: errors
        type(Material_t) :: from_atom_fractions
        type(Material_t) :: from_weight_fractions
        type(MessageList_t) :: messages

        components(1) = MaterialComponent(naturalHydrogenGas(), 0.6d0)
        components(2) = MaterialComponent(naturalWater(), 0.4d0)
        call fromAtomFractions( &
                components, messages, errors, from_atom_fractions)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    components, messages, errors, from_weight_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ = &
                        assertEquals( &
                                0.6d0, &
                                from_atom_fractions%atomFraction(hydrogenGasSymbol()), &
                                "H2 atom fraction") &
                        .and.assertEquals( &
                                0.4d0, &
                                from_atom_fractions%atomFraction(waterSymbol()), &
                                "H2O atom fraction") &
                        .and.assertEquals( &
                                0.6d0, &
                                from_weight_fractions%weightFraction(hydrogenGasSymbol()), &
                                "H2 weight fraction") &
                        .and.assertEquals( &
                                0.4d0, &
                                from_weight_fractions%weightFraction(waterSymbol()), &
                                "H2O weight fraction")
            end if
        end if
    end function checkKeepsTrack

    pure function checkNormalizedFractions() result(result_)
        type(Result_t) :: result_

        type(MaterialComponent_t) :: components(2)
        type(ErrorList_t) :: errors
        type(Material_t) :: from_atom_fractions
        type(Material_t) :: from_weight_fractions
        type(MessageList_t) :: messages

        components(1) = MaterialComponent(naturalHydrogenGas(), 0.06d0)
        components(2) = MaterialComponent(naturalWater(), 0.04d0)
        call fromAtomFractions( &
                components, messages, errors, from_atom_fractions)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    components, messages, errors, from_weight_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ = &
                        assertEquals( &
                                0.6d0, &
                                from_atom_fractions%atomFraction(hydrogenGasSymbol()), &
                                "H2 atom fraction") &
                        .and.assertEquals( &
                                0.4d0, &
                                from_atom_fractions%atomFraction(waterSymbol()), &
                                "H2O atom fraction") &
                        .and.assertEquals( &
                                0.6d0, &
                                from_weight_fractions%weightFraction(hydrogenGasSymbol()), &
                                "H2 weight fraction") &
                        .and.assertEquals( &
                                0.4d0, &
                                from_weight_fractions%weightFraction(waterSymbol()), &
                                "H2O weight fraction")
            end if
        end if
    end function checkNormalizedFractions

    pure function checkNormalizedMessages() result(result_)
        type(Result_t) :: result_

        type(MaterialComponent_t) :: components(2)
        type(ErrorList_t) :: errors
        type(Material_t) :: material
        type(MessageList_t) :: messages_from_atom_fractions
        type(MessageList_t) :: messages_from_weight_fractions

        components(1) = MaterialComponent(naturalHydrogenGas(), 0.06d0)
        components(2) = MaterialComponent(naturalWater(), 0.04d0)
        call fromAtomFractions( &
                components, messages_from_atom_fractions, errors, material)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    components, messages_from_weight_fractions, errors, material)
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
    end function checkNormalizedMessages

    pure function checkDuplicates() result(result_)
        type(Result_t) :: result_

        type(MaterialComponent_t) :: components(2)
        type(ErrorList_t) :: errors
        type(Material_t) :: from_atom_fractions
        type(Material_t) :: from_weight_fractions
        type(MessageList_t) :: messages

        components(1) = MaterialComponent(naturalHydrogenGas(), 0.6d0)
        components(2) = MaterialComponent(naturalHydrogenGas(), 0.4d0)
        call fromAtomFractions( &
                components, messages, errors, from_atom_fractions)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call fromWeightFractions( &
                    components, messages, errors, from_weight_fractions)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                result_ = &
                        assertAllChemical( &
                                hydrogenGasSymbol(), from_atom_fractions, "atom fractions") &
                        .and.assertAllChemical( &
                                hydrogenGasSymbol(), from_weight_fractions, "weight fractions")
            end if
        end if
    end function checkDuplicates

    pure function checkCombine() result(result_)
        type(Result_t) :: result_

        type(ErrorList_t) :: errors
        type(Material_t) :: from_atom_factors
        type(Material_t) :: from_weight_factors
        type(Material_t) :: helium
        type(MaterialComponent_t) :: helium_components(1)
        type(Material_t) :: hydrogen
        type(MaterialComponent_t) :: hydrogen_components(1)
        type(MessageList_t) :: messages

        hydrogen_components(1) = MaterialComponent(naturalHydrogenGas(), 1.0d0)
        call fromAtomFractions(hydrogen_components, messages, errors, hydrogen)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            helium_components(1) = MaterialComponent(naturalHeliumGas(), 1.0d0)
            call fromAtomFractions(helium_components, messages, errors, helium)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                call combineByAtomFactors( &
                        hydrogen, &
                        0.6d0, &
                        helium, &
                        0.4d0, &
                        messages, &
                        errors, &
                        from_atom_factors)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    call combineByWeightFactors( &
                            hydrogen, &
                            0.6d0, &
                            helium, &
                            0.4d0, &
                            messages, &
                            errors, &
                            from_weight_factors)
                    if (errors%hasAny()) then
                        result_ = fail(errors%toString())
                    else
                        result_ = &
                                assertEquals( &
                                        0.6d0, &
                                        from_atom_factors%atomFraction(hydrogenGasSymbol()), &
                                        "H2 atom fraction") &
                                .and.assertEquals( &
                                        0.4d0, &
                                        from_atom_factors%atomFraction(heliumGasSymbol()), &
                                        "He atom fraction") &
                                .and.assertEquals( &
                                        0.6d0, &
                                        from_weight_factors%weightFraction(hydrogenGasSymbol()), &
                                        "H2 weight fraction") &
                                .and.assertEquals( &
                                        0.4d0, &
                                        from_weight_factors%weightFraction(heliumGasSymbol()), &
                                        "He weight fraction")
                    end if
                end if
            end if
        end if
    end function checkCombine

    pure function assertAllIsotope(isotope, material, from) result(result_)
        type(Isotope_t), intent(in) :: isotope
        type(Material_t), intent(in) :: material
        character(len=*), intent(in) :: from
        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        1.0d0, &
                        material%atomFraction(isotope), &
                        "atom fraction from " // from) &
                .and.assertEquals( &
                        1.0d0, &
                        material%weightFraction(isotope), &
                        "weight fraction from " // from)
    end function assertAllIsotope

    pure function assertAllElement(element, material, from) result(result_)
        type(ElementSymbol_t), intent(in) :: element
        type(Material_t), intent(in) :: material
        character(len=*), intent(in) :: from
        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        1.0d0, &
                        material%atomFraction(element), &
                        "atom fraction from " // from) &
                .and.assertEquals( &
                        1.0d0, &
                        material%weightFraction(element), &
                        "weight fraction from " // from)
    end function assertAllElement

    pure function assertAllChemical(chemical, material, from) result(result_)
        type(ChemicalSymbol_t), intent(in) :: chemical
        type(Material_t), intent(in) :: material
        character(len=*), intent(in) :: from
        type(Result_t) :: result_

        result_ = &
                assertEquals( &
                        1.0d0, &
                        material%atomFraction(chemical), &
                        "atom fraction from " // from) &
                .and.assertEquals( &
                        1.0d0, &
                        material%weightFraction(chemical), &
                        "weight fraction from " // from)
    end function assertAllChemical
end module material_test
