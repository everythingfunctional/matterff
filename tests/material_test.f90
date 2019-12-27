module material_test
    use Chemical_m, only: Chemical_t, makeChemical
    use Chemical_component_m, only: ChemicalComponent_t, ChemicalComponent
    use Chemical_symbol_m, only: hydrogenGasSymbol
    use Element_m, only: Element_t, fromAtomFractions
    use Element_component_m, only: ElementComponent
    use Element_symbol_m, only: H
    use erloff, only: ErrorList_t, MessageList_t
    use Isotope_m, only: Isotope_t, H_1
    use Material_m, only: Material_t, fromAtomFractions, fromWeightFractions
    use Material_component_m, only: MaterialComponent_t, MaterialComponent
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, Describe, fail, It

    implicit none
    private

    public :: test_material
contains
    function test_material() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "A single isotope material is all that isotope", &
                checkSingleIsotope)
        tests = Describe("Material_t", individual_tests)
    end function test_material

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
                call fromAtomFractions( &
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

    pure function assertAllIsotope(isotope, element, from) result(result_)
        type(Isotope_t), intent(in) :: isotope
        type(Material_t), intent(in) :: element
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
end module material_test
