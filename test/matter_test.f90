module matter_test
    use erloff, only: error_list_t
    use matterff, only: &
            chemical_component_t, &
            chemical_symbol_t, &
            chemical_symbol_component_t, &
            element_component_t, &
            fallible_chemical_t, &
            fallible_element_t, &
            fallible_material_t, &
            fallible_matter_t, &
            material_component_t, &
            matter_t, &
            from_atom_fractions, &
            natural_oxygen, &
            pure_natural_helium_gas, &
            pure_natural_hydrogen_gas, &
            water_symbol, &
            C, &
            C_12, &
            H, &
            H_1
    use quaff, only: operator(.unit.), GRAMS, MOLS
    use quaff_asserts_m, only: assert_equals
    use vegetables, only: result_t, test_item_t, describe, fail, it

    implicit none
    private
    public :: test_matter
contains
    function test_matter() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "matter_t", &
                [ it("1 mol of water has 2 mols of hydrogen", check_water_amount) &
                , it("12 g of C-12 is 1 mol", check_carbon_amount) &
                , it("1 mol of C-12 is 12 g", check_carbon_mass) &
                , it( &
                        "Combining 1 g each of two different materials is 2 g", &
                        check_combine_matter) &
                ])
    end function

    function check_water_amount() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(matter_t) :: matter
        type(fallible_element_t) :: maybe_H_1
        type(fallible_material_t) :: maybe_material
        type(fallible_matter_t) :: maybe_matter
        type(fallible_chemical_t) :: maybe_water

        maybe_H_1 = from_atom_fractions(H, [element_component_t(H_1, 1.0d0)])
        if (maybe_H_1%failed()) then
            errors = maybe_H_1%errors()
            result_ = fail(errors%to_string())
        else
            maybe_water = fallible_chemical_t( &
                    water_symbol(), &
                    [ chemical_component_t(maybe_H_1%element(), 2.0d0) &
                    , chemical_component_t(natural_oxygen(), 1.0d0) &
                    ])
            if (maybe_water%failed()) then
                errors = maybe_water%errors()
                result_ = fail(errors%to_string())
            else
                maybe_material = from_atom_fractions([material_component_t(maybe_water%chemical(), 1.0d0)])
                if (maybe_material%failed()) then
                    errors = maybe_material%errors()
                    result_ = fail(errors%to_string())
                else
                    maybe_matter = fallible_matter_t(1.0d0.unit.MOLS, maybe_material%material())
                    if (maybe_matter%failed()) then
                        errors = maybe_matter%errors()
                        result_ = fail(errors%to_string())
                    else
                        matter = maybe_matter%matter()
                        result_ = &
                                assert_equals( &
                                        2.0d0.unit.MOLS, &
                                        matter%amount(H), &
                                        "H amount") &
                                .and.assert_equals( &
                                        2.0d0.unit.MOLS, &
                                        matter%amount(H_1), &
                                        "H-1 amount")
                    end if
                end if
            end if
        end if
    end function

    function check_carbon_amount() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(matter_t) :: matter
        type(fallible_element_t) :: maybe_element
        type(fallible_chemical_t) :: maybe_chemical
        type(fallible_material_t) :: maybe_material
        type(fallible_matter_t) :: maybe_matter

        maybe_element = from_atom_fractions(C, [element_component_t(C_12, 1.0d0)])
        if (maybe_element%failed()) then
            errors = maybe_element%errors()
            result_ = fail(errors%to_string())
        else
            maybe_chemical = fallible_chemical_t( &
                    chemical_symbol_t([chemical_symbol_component_t(C, 1)]), &
                    [ chemical_component_t(maybe_element%element(), 1.0d0) &
                    ])
            if (maybe_chemical%failed()) then
                errors = maybe_chemical%errors()
                result_ = fail(errors%to_string())
            else
                maybe_material = from_atom_fractions([material_component_t(maybe_chemical%chemical(), 1.0d0)])
                if (maybe_material%failed()) then
                    errors = maybe_material%errors()
                    result_ = fail(errors%to_string())
                else
                    maybe_matter = fallible_matter_t(12.0d0.unit.GRAMS, maybe_material%material())
                    if (maybe_matter%failed()) then
                        errors = maybe_matter%errors()
                        result_ = fail(errors%to_string())
                    else
                        matter = maybe_matter%matter()
                        result_ = assert_equals(1.0d0.unit.MOLS, matter%amount())
                    end if
                end if
            end if
        end if
    end function

    function check_carbon_mass() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(matter_t) :: matter
        type(fallible_element_t) :: maybe_element
        type(fallible_chemical_t) :: maybe_chemical
        type(fallible_material_t) :: maybe_material
        type(fallible_matter_t) :: maybe_matter

        maybe_element = from_atom_fractions(C, [element_component_t(C_12, 1.0d0)])
        if (maybe_element%failed()) then
            errors = maybe_element%errors()
            result_ = fail(errors%to_string())
        else
            maybe_chemical = fallible_chemical_t( &
                    chemical_symbol_t([chemical_symbol_component_t(C, 1)]), &
                    [ chemical_component_t(maybe_element%element(), 1.0d0) &
                    ])
            if (maybe_chemical%failed()) then
                errors = maybe_chemical%errors()
                result_ = fail(errors%to_string())
            else
                maybe_material = from_atom_fractions([material_component_t(maybe_chemical%chemical(), 1.0d0)])
                if (maybe_material%failed()) then
                    errors = maybe_material%errors()
                    result_ = fail(errors%to_string())
                else
                    maybe_matter = fallible_matter_t(1.0d0.unit.MOLS, maybe_material%material())
                    if (maybe_matter%failed()) then
                        errors = maybe_matter%errors()
                        result_ = fail(errors%to_string())
                    else
                        matter = maybe_matter%matter()
                        result_ = assert_equals(12.0d0.unit.GRAMS, matter%mass())
                    end if
                end if
            end if
        end if
    end function

    function check_combine_matter() result(result_)
        type(result_t) :: result_

        type(matter_t) :: combined
        type(error_list_t) :: errors
        type(fallible_matter_t) :: maybe_matter1
        type(fallible_matter_t) :: maybe_matter2

        maybe_matter1 = fallible_matter_t( &
                1.0d0.unit.GRAMS, pure_natural_hydrogen_gas())
        if (maybe_matter1%failed()) then
            errors = maybe_matter1%errors()
            result_ = fail(errors%to_string())
        else
            maybe_matter2 = fallible_matter_t( &
                    1.0d0.unit.GRAMS, pure_natural_helium_gas())
            if (maybe_matter2%failed()) then
                errors = maybe_matter1%errors()
                result_ = fail(errors%to_string())
            else
                combined = maybe_matter1%matter() + maybe_matter2%matter()
                result_ = assert_equals(2.0d0.unit.GRAMS, combined%mass())
            end if
        end if
    end function
end module
