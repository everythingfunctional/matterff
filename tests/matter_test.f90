module matter_test
    use Chemical_m, only: Chemical_t, makeChemical
    use Chemical_component_m, only: ChemicalComponent_t, ChemicalComponent
    use Chemical_symbol_m, only: ChemicalSymbol_t, ChemicalSymbol, waterSymbol
    use Chemical_symbol_component_m, only: &
            ChemicalSymbolComponent_t, ChemicalSymbolComponent
    use Element_m, only: Element_t, fromAtomFractions, naturalOxygen
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: H, C
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: H_1, C_12
    use Material_m, only: &
            Material_t, &
            fromAtomFractions, &
            pureNaturalHydrogenGas, &
            pureNaturalHeliumGas
    use Material_component_m, only: MaterialComponent_t, MaterialComponent
    use Matter_m, only: Matter_t, operator(+), createMatter
    use quaff, only: operator(.unit.), GRAMS, MOLS
    use quaff_asserts_m, only: assertEquals
    use Vegetables_m, only: Result_t, TestItem_t, Describe, fail, It

    implicit none
    private

    public :: test_matter
contains
    function test_matter() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = It( &
                "1 mol of water has 2 mols of hydrogen", checkWaterAmount)
        individual_tests(2) = It( &
                "12 g of C-12 is 1 mol", checkCarbonAmount)
        individual_tests(3) = It( &
                "1 mol of C-12 is 12 g", checkCarbonMass)
        individual_tests(4) = It( &
                "Combining 1 g each of two different materials is 2 g", &
                checkCombineMatter)
        tests = Describe("Matter_t", individual_tests)
    end function test_matter

    pure function checkWaterAmount() result(result_)
        type(Result_t) :: result_

        type(ChemicalComponent_t) :: chemical_components(2)
        type(ErrorList_t) :: errors
        type(MessageList_t) :: messages
        type(Material_t) :: material
        type(MaterialComponent_t) :: material_components(1)
        type(Matter_t) :: matter
        type(Element_t) :: pure_H_1
        type(Chemical_t) :: water

        call fromAtomFractions( &
                H, [ElementComponent(H_1, 1.0d0)], messages, errors, pure_H_1)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            chemical_components(1) = ChemicalComponent(pure_H_1, 2.0d0)
            chemical_components(2) = ChemicalComponent(naturalOxygen(), 1.0d0)
            call makeChemical(waterSymbol(), chemical_components, errors, water)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                material_components(1) = MaterialComponent(water, 1.0d0)
                call fromAtomFractions( &
                        material_components, messages, errors, material)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    call createMatter(1.0d0.unit.MOLS, material, errors, matter)
                    if (errors%hasAny()) then
                        result_ = fail(errors%toString())
                    else
                        result_ = &
                                assertEquals( &
                                        2.0d0.unit.MOLS, &
                                        matter%amount(H), &
                                        "H amount") &
                                .and.assertEquals( &
                                        2.0d0.unit.MOLS, &
                                        matter%amount(H_1), &
                                        "H-1 amount")
                    end if
                end if
            end if
        end if
    end function checkWaterAmount

    pure function checkCarbonAmount() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: chemical_components(1)
        type(ChemicalSymbol_t) :: chemical_symbol
        type(ChemicalSymbolComponent_t) :: chemical_symbol_components(1)
        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(Material_t) :: material
        type(MaterialComponent_t) :: material_components(1)
        type(Matter_t) :: matter
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                C, [ElementComponent(C_12, 1.0d0)], messages, errors, element)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            chemical_symbol_components(1) = ChemicalSymbolComponent(C, 1)
            chemical_symbol = ChemicalSymbol(chemical_symbol_components)
            chemical_components(1) = ChemicalComponent(element, 1.0d0)
            call makeChemical( &
                    chemical_symbol, chemical_components, errors, chemical)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                material_components(1) = MaterialComponent(chemical, 1.0d0)
                call fromAtomFractions( &
                        material_components, messages, errors, material)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    call createMatter(12.0d0.unit.GRAMS, material, errors, matter)
                    if (errors%hasAny()) then
                        result_ = fail(errors%toString())
                    else
                        result_ = assertEquals(1.0d0.unit.MOLS, matter%amount())
                    end if
                end if
            end if
        end if
    end function checkCarbonAmount

    pure function checkCarbonMass() result(result_)
        type(Result_t) :: result_

        type(Chemical_t) :: chemical
        type(ChemicalComponent_t) :: chemical_components(1)
        type(ChemicalSymbol_t) :: chemical_symbol
        type(ChemicalSymbolComponent_t) :: chemical_symbol_components(1)
        type(Element_t) :: element
        type(ErrorList_t) :: errors
        type(Material_t) :: material
        type(MaterialComponent_t) :: material_components(1)
        type(Matter_t) :: matter
        type(MessageList_t) :: messages

        call fromAtomFractions( &
                C, [ElementComponent(C_12, 1.0d0)], messages, errors, element)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            chemical_symbol_components(1) = ChemicalSymbolComponent(C, 1)
            chemical_symbol = ChemicalSymbol(chemical_symbol_components)
            chemical_components(1) = ChemicalComponent(element, 1.0d0)
            call makeChemical( &
                    chemical_symbol, chemical_components, errors, chemical)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                material_components(1) = MaterialComponent(chemical, 1.0d0)
                call fromAtomFractions( &
                        material_components, messages, errors, material)
                if (errors%hasAny()) then
                    result_ = fail(errors%toString())
                else
                    call createMatter(1.0d0.unit.MOLS, material, errors, matter)
                    if (errors%hasAny()) then
                        result_ = fail(errors%toString())
                    else
                        result_ = assertEquals(12.0d0.unit.GRAMS, matter%mass())
                    end if
                end if
            end if
        end if
    end function checkCarbonMass

    pure function checkCombineMatter() result(result_)
        type(Result_t) :: result_

        type(Matter_t) :: combined
        type(ErrorList_t) :: errors
        type(Matter_t) :: matter1
        type(Matter_t) :: matter2

        call createMatter( &
                1.0d0.unit.GRAMS, pureNaturalHydrogenGas(), errors, matter1)
        if (errors%hasAny()) then
            result_ = fail(errors%toString())
        else
            call createMatter( &
                    1.0d0.unit.GRAMS, pureNaturalHeliumGas(), errors, matter2)
            if (errors%hasAny()) then
                result_ = fail(errors%toString())
            else
                combined = matter1 + matter2
                result_ = assertEquals(2.0d0.unit.GRAMS, combined%mass())
            end if
        end if
    end function checkCombineMatter
end module matter_test
