module material_test
    use erloff, only: error_list_t, message_list_t
    use matterff, only: &
            fallible_chemical_t, &
            fallible_element_t, &
            fallible_material_t, &
            chemical_t, &
            chemical_component_t, &
            chemical_symbol_t, &
            element_component_t, &
            element_symbol_t, &
            isotope_t, &
            material_t, &
            material_component_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors, &
            from_atom_fractions, &
            from_weight_fractions, &
            helium_gas_symbol, &
            hydrogen_gas_symbol, &
            natural_helium_gas, &
            natural_hydrogen, &
            natural_hydrogen_gas, &
            natural_water, &
            water_symbol, &
            H, &
            H_1, &
            INVALID_ARGUMENT, &
            NORMALIZED_FRACTIONS
    use quaff_asserts_m, only: assert_equals
    use vegetables, only: &
            result_t, test_item_t, assert_equals, assert_that, describe, fail, it

    implicit none
    private
    public :: test_material
contains
    function test_material() result(tests)
        type(test_item_t) :: tests

        tests = describe ( &
                "material_t", &
                [ it( &
                        "Creating a material with negative fractions is an error", &
                        check_negative_fractions) &
                , it( &
                        "A single isotope material is all that isotope", &
                        check_single_isotope) &
                , it( &
                        "A single element material is all that element", &
                        check_single_element) &
                , it( &
                        "A single chemical material is all that chemical", &
                        check_single_chemical) &
                , it( &
                        "A single chemical material has the same molar mass", &
                        check_single_chemical_molar_mass) &
                , it("Keeps track of its components", check_keeps_track) &
                , it( &
                        "Has normalized fractions of its components", &
                        check_normalized_fractions) &
                , it( &
                        "Normalizing fractions of chemicals produces a message", &
                        check_normalized_messages) &
                , it( &
                        "Created with duplicate chemicals has sum of duplicates", &
                        check_duplicates) &
                , it("Combining materials results in correct fractions", check_combine) &
                ])
    end function

    function check_negative_fractions() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_material_t) :: maybe_material

        maybe_material = from_atom_fractions( &
                [material_component_t(natural_hydrogen_gas(), -1.0d0)])
        errors = maybe_material%errors()
        result_ = assert_that(errors.hasType.INVALID_ARGUMENT, errors%to_string())

        maybe_material = from_weight_fractions( &
                [material_component_t(natural_hydrogen_gas(), -1.0d0)])
        errors = maybe_material%errors()
        result_ = result_.and.assert_that(errors.hasType.INVALID_ARGUMENT, errors%to_string())
    end function

    function check_single_isotope() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_chemical
        type(fallible_element_t) :: maybe_element
        type(fallible_material_t) :: maybe_material

        maybe_element = from_atom_fractions(H, [element_component_t(H_1, 1.0d0)])
        if (maybe_element%failed()) then
            errors = maybe_element%errors()
            result_ = fail(errors%to_string())
        else
            maybe_chemical = fallible_chemical_t( &
                    hydrogen_gas_symbol(), &
                    [chemical_component_t(maybe_element%element(), 2.0d0)])
            if (maybe_chemical%failed()) then
                errors = maybe_chemical%errors()
                result_ = fail(errors%to_string())
            else
                maybe_material = from_atom_fractions( &
                        [material_component_t(maybe_chemical%chemical(), 1.0d0)])
                if (maybe_material%failed()) then
                    errors = maybe_material%errors()
                    result_ = fail(errors%to_string())
                else
                    result_ = assert_all_isotope( &
                            H_1, maybe_material%material(), "atom fractions")
                end if
                maybe_material = from_weight_fractions( &
                        [material_component_t(maybe_chemical%chemical(), 1.0d0)])
                if (maybe_material%failed()) then
                    errors = maybe_material%errors()
                    result_ = result_.and.fail(errors%to_string())
                else
                    result_ = result_.and.assert_all_isotope( &
                            H_1, maybe_material%material(), "weight fractions")
                end if
            end if
        end if
    end function

    function check_single_element() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_chemical
        type(fallible_material_t) :: maybe_material

        maybe_chemical = fallible_chemical_t( &
                hydrogen_gas_symbol(), &
                [chemical_component_t(natural_hydrogen(), 2.0d0)])
        if (maybe_chemical%failed()) then
            errors = maybe_chemical%errors()
            result_ = fail(errors%to_string())
        else
            maybe_material = from_atom_fractions( &
                    [material_component_t(maybe_chemical%chemical(), 1.0d0)])
            if (maybe_material%failed()) then
                errors = maybe_material%errors()
                result_ = fail(errors%to_string())
            else
                result_ = assert_all_element( &
                        H, maybe_material%material(), "atom fractions")
            end if
            maybe_material = from_weight_fractions( &
                    [material_component_t(maybe_chemical%chemical(), 1.0d0)])
            if (maybe_material%failed()) then
                errors = maybe_material%errors()
                result_ = result_.and.fail(errors%to_string())
            else
                result_ = result_.and.assert_all_element( &
                        H, maybe_material%material(), "weight fractions")
            end if
        end if
    end function

    function check_single_chemical() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_material_t) :: maybe_material

        maybe_material = from_atom_fractions( &
                [material_component_t(natural_hydrogen_gas(), 1.0d0)])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = fail(errors%to_string())
        else
            result_ = assert_all_chemical( &
                    hydrogen_gas_symbol(), maybe_material%material(), "atom fractions")
        end if
        maybe_material = from_weight_fractions( &
                [material_component_t(natural_hydrogen_gas(), 1.0d0)])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = result_.and.fail(errors%to_string())
        else
            result_ = result_.and.assert_all_chemical( &
                    hydrogen_gas_symbol(), maybe_material%material(), "weight fractions")
        end if
    end function

    function check_single_chemical_molar_mass() result(result_)
        type(result_t) :: result_

        type(chemical_t) :: chemical
        type(error_list_t) :: errors
        type(material_t) :: material
        type(fallible_material_t) :: maybe_material

        chemical = natural_water()
        maybe_material = from_atom_fractions( &
                [material_component_t(chemical, 1.0d0)])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = fail(errors%to_string())
        else
            material = maybe_material%material()
            result_ = assert_equals(chemical%molar_mass(), material%molar_mass())
        end if
    end function

    function check_keeps_track() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(material_t) :: material
        type(fallible_material_t) :: maybe_material

        maybe_material = from_atom_fractions( &
                [ material_component_t(natural_hydrogen_gas(), 0.6d0) &
                , material_component_t(natural_water(), 0.4d0) &
                ])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = fail(errors%to_string())
        else
            material = maybe_material%material()
            result_ = &
                    assert_equals( &
                            0.6d0, &
                            material%atom_fraction(hydrogen_gas_symbol()), &
                            "H2 atom fraction") &
                    .and. assert_equals( &
                            0.4d0, &
                            material%atom_fraction(water_symbol()), &
                            "H2O atom fraction")
        end if
        maybe_material = from_weight_fractions( &
                [ material_component_t(natural_hydrogen_gas(), 0.6d0) &
                , material_component_t(natural_water(), 0.4d0) &
                ])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = result_.and.fail(errors%to_string())
        else
            material = maybe_material%material()
            result_ = &
                    result_ &
                    .and.assert_equals( &
                            0.6d0, &
                            material%weight_fraction(hydrogen_gas_symbol()), &
                            "H2 weight fraction") &
                    .and. assert_equals( &
                            0.4d0, &
                            material%weight_fraction(water_symbol()), &
                            "H2O weight fraction")
        end if
    end function

    function check_normalized_fractions() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(material_t) :: material
        type(fallible_material_t) :: maybe_material

        maybe_material = from_atom_fractions( &
                [ material_component_t(natural_hydrogen_gas(), 0.06d0) &
                , material_component_t(natural_water(), 0.04d0) &
                ])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = fail(errors%to_string())
        else
            material = maybe_material%material()
            result_ = &
                    assert_equals( &
                            0.6d0, &
                            material%atom_fraction(hydrogen_gas_symbol()), &
                            "H2 atom fraction") &
                    .and. assert_equals( &
                            0.4d0, &
                            material%atom_fraction(water_symbol()), &
                            "H2O atom fraction")
        end if
        maybe_material = from_weight_fractions( &
                [ material_component_t(natural_hydrogen_gas(), 0.06d0) &
                , material_component_t(natural_water(), 0.04d0) &
                ])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = result_.and.fail(errors%to_string())
        else
            material = maybe_material%material()
            result_ = &
                    result_ &
                    .and.assert_equals( &
                            0.6d0, &
                            material%weight_fraction(hydrogen_gas_symbol()), &
                            "H2 weight fraction") &
                    .and. assert_equals( &
                            0.4d0, &
                            material%weight_fraction(water_symbol()), &
                            "H2O weight fraction")
        end if
    end function

    function check_normalized_messages() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_material_t) :: maybe_material
        type(message_list_t) :: messages

        maybe_material = from_atom_fractions( &
                [ material_component_t(natural_hydrogen_gas(), 0.06d0) &
                , material_component_t(natural_water(), 0.04d0) &
                ])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = fail(errors%to_string())
        else
            messages = maybe_material%messages()
            result_ = assert_that( &
                    messages.hasType.NORMALIZED_FRACTIONS, messages%to_string())
        end if
        maybe_material = from_weight_fractions( &
                [ material_component_t(natural_hydrogen_gas(), 0.06d0) &
                , material_component_t(natural_water(), 0.04d0) &
                ])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = result_.and.fail(errors%to_string())
        else
            messages = maybe_material%messages()
            result_ = result_.and.assert_that( &
                    messages.hasType.NORMALIZED_FRACTIONS, messages%to_string())
        end if
    end function

    function check_duplicates() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_material_t) :: maybe_material

        maybe_material = from_atom_fractions( &
                [ material_component_t(natural_hydrogen_gas(), 0.6d0) &
                , material_component_t(natural_hydrogen_gas(), 0.4d0) &
                ])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = fail(errors%to_string())
        else
            result_ = assert_all_chemical( &
                    hydrogen_gas_symbol(), maybe_material%material(), "atom fractions")
        end if
        maybe_material = from_weight_fractions( &
                [ material_component_t(natural_hydrogen_gas(), 0.6d0) &
                , material_component_t(natural_hydrogen_gas(), 0.4d0) &
                ])
        if (maybe_material%failed()) then
            errors = maybe_material%errors()
            result_ = result_.and.fail(errors%to_string())
        else
            result_ = result_.and.assert_all_chemical( &
                    hydrogen_gas_symbol(), maybe_material%material(), "weight fractions")
        end if
    end function

    function check_combine() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(material_t) :: combined
        type(material_t) :: helium
        type(material_t) :: hydrogen
        type(fallible_material_t) :: maybe_combined
        type(fallible_material_t) :: maybe_helium
        type(fallible_material_t) :: maybe_hydrogen

        maybe_hydrogen = from_atom_fractions( &
                [material_component_t(natural_hydrogen_gas(), 1.0d0)])
        if (maybe_hydrogen%failed()) then
            errors = maybe_hydrogen%errors()
            result_ = fail(errors%to_string())
        else
            hydrogen = maybe_hydrogen%material()
            maybe_helium = from_atom_fractions( &
                    [material_component_t(natural_helium_gas(), 1.0d0)])
            if (maybe_helium%failed()) then
                errors = maybe_helium%errors()
                result_ = fail(errors%to_string())
            else
                helium = maybe_helium%material()
                maybe_combined = combine_by_atom_factors( &
                        hydrogen, 0.6d0, helium, 0.4d0)
                if (maybe_combined%failed()) then
                    errors = maybe_combined%errors()
                    result_ = fail(errors%to_string())
                else
                    combined = maybe_combined%material()
                    result_ = &
                            assert_equals( &
                                    0.6d0, &
                                    combined%atom_fraction(hydrogen_gas_symbol()), &
                                    "H2 atom fraction") &
                            .and.assert_equals( &
                                    0.4d0, &
                                    combined%atom_fraction(helium_gas_symbol()), &
                                    "He atom fraction")
                end if
                maybe_combined = combine_by_weight_factors( &
                        hydrogen, 0.6d0, helium, 0.4d0)
                if (maybe_combined%failed()) then
                    errors = maybe_combined%errors()
                    result_ = result_.and.fail(errors%to_string())
                else
                    combined = maybe_combined%material()
                    result_ = &
                            result_ &
                            .and.assert_equals( &
                                    0.6d0, &
                                    combined%weight_fraction(hydrogen_gas_symbol()), &
                                    "H2 weight fraction") &
                            .and.assert_equals( &
                                    0.4d0, &
                                    combined%weight_fraction(helium_gas_symbol()), &
                                    "He weight fraction")
                end if
            end if
        end if
    end function

    function assert_all_isotope(isotope, material, from) result(result_)
        type(isotope_t), intent(in) :: isotope
        type(material_t), intent(in) :: material
        character(len=*), intent(in) :: from
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        1.0d0, &
                        material%atom_fraction(isotope), &
                        "atom fraction from " // from) &
                .and.assert_equals( &
                        1.0d0, &
                        material%weight_fraction(isotope), &
                        "weight fraction from " // from)
    end function

    function assert_all_element(element, material, from) result(result_)
        type(element_symbol_t), intent(in) :: element
        type(material_t), intent(in) :: material
        character(len=*), intent(in) :: from
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        1.0d0, &
                        material%atom_fraction(element), &
                        "atom fraction from " // from) &
                .and.assert_equals( &
                        1.0d0, &
                        material%weight_fraction(element), &
                        "weight fraction from " // from)
    end function

    function assert_all_chemical(chemical, material, from) result(result_)
        type(chemical_symbol_t), intent(in) :: chemical
        type(material_t), intent(in) :: material
        character(len=*), intent(in) :: from
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        1.0d0, &
                        material%atom_fraction(chemical), &
                        "atom fraction from " // from) &
                .and.assert_equals( &
                        1.0d0, &
                        material%weight_fraction(chemical), &
                        "weight fraction from " // from)
    end function
end module
