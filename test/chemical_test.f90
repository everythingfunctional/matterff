module chemical_test
    use erloff, only: error_list_t, module_t, procedure_t
    use matterff, only: &
            chemical_t, &
            chemical_component_t, &
            element_t, &
            element_component_t, &
            fallible_chemical_t, &
            fallible_element_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors, &
            find, &
            from_atom_fractions, &
            helium_gas_symbol, &
            hydrogen_gas_symbol, &
            natural_helium, &
            natural_helium_gas, &
            natural_hydrogen, &
            natural_hydrogen_gas, &
            natural_water, &
            water_symbol, &
            H, &
            H_1, &
            H_2, &
            O
    use matterff_utilities_m, only: INVALID_ARGUMENT, MISMATCH
    use vegetables, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_that, &
            describe, &
            fail, &
            it

    implicit none
    private
    public :: test_chemical

    character(len=*), parameter :: MODULE_NAME = "chemical_test"
contains
    function test_chemical() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "chemical_t", &
                [ it( &
                        "Creating a chemical with elements not included in the symbol is an error", &
                        check_not_in_symbol) &
                , it( &
                        "Creating a chemical with negative multipliers is an error", &
                        check_negative_multipliers) &
                , it( &
                        "A single element chemical is all that element", &
                        check_single_element) &
                , it( &
                        "A single isotope chemical is all that isotope", &
                        check_single_isotope) &
                , it( &
                        "Created with duplicate elements has sum of duplicates", &
                        check_duplicate) &
                , it( &
                        "Combining chemicals of different types is an error", &
                        check_combine_error) &
                , it("Combining chemicals results in correct fractions", check_combine) &
                , it( &
                        "Water is 2/3 hydrogen and 1/3 oxygen by atom", &
                        check_water_fractions) &
                , it( &
                        "Has a position of 0 if it's not in a list", &
                        check_not_found) &
                , it("Can be found in a list", check_find) &
                ])
    end function

    function check_not_in_symbol() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_chemical

        maybe_chemical = fallible_chemical_t( &
                hydrogen_gas_symbol(), &
                [chemical_component_t(natural_helium(), 1.0d0)])
        errors = maybe_chemical%errors()
        result_ = assert_that(errors.hasType.MISMATCH, errors%to_string())
    end function

    function check_negative_multipliers() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_chemical

        maybe_chemical = fallible_chemical_t( &
                hydrogen_gas_symbol(), &
                [chemical_component_t(natural_hydrogen(), -2.0d0)])
        errors = maybe_chemical%errors()
        result_ = assert_that(errors.hasType.INVALID_ARGUMENT, errors%to_string())
    end function

    function check_single_element() result(result_)
        type(result_t) :: result_

        type(chemical_t) :: chemical
        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_chemical

        maybe_chemical = fallible_chemical_t( &
                hydrogen_gas_symbol(), &
                [chemical_component_t(natural_hydrogen(), 2.0d0)])
        if (maybe_chemical%failed()) then
            errors = maybe_chemical%errors()
        else
            chemical = maybe_chemical%chemical()
            result_ = &
                    assert_equals(1.0d0, chemical%atom_fraction(H), "atom fraction") &
                    .and.assert_equals(1.0d0, chemical%weight_fraction(H), "weight fraction")
        end if
    end function

    function check_single_isotope() result(result_)
        type(result_t) :: result_

        type(chemical_t) :: chemical
        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_chemical
        type(fallible_element_t) :: maybe_element

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
                chemical = maybe_chemical%chemical()
                result_ = &
                        assert_equals(1.0d0, chemical%atom_fraction(H_1), "atom fraction") &
                        .and.assert_equals(1.0d0, chemical%weight_fraction(H_1), "weight fraction")
            end if
        end if
    end function

    function check_duplicate() result(result_)
        type(result_t) :: result_

        type(chemical_t) :: chemical
        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_chemical

        maybe_chemical = fallible_chemical_t( &
                hydrogen_gas_symbol(), &
                [ chemical_component_t(natural_hydrogen(), 1.0d0) &
                , chemical_component_t(natural_hydrogen(), 1.0d0) &
                ])
        if (maybe_chemical%failed()) then
            errors = maybe_chemical%errors()
        else
            chemical = maybe_chemical%chemical()
            result_ = &
                    assert_equals(1.0d0, chemical%atom_fraction(H), "atom fraction") &
                    .and.assert_equals(1.0d0, chemical%weight_fraction(H), "weight fraction")
        end if
    end function

    function check_combine_error() result(result_)
        type(result_t) :: result_

        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_chemical

        maybe_chemical = combine_by_atom_factors( &
                natural_hydrogen_gas(), &
                1.0d0, &
                natural_helium_gas(), &
                1.0d0)
        errors = maybe_chemical%errors()
        result_ = assert_that(errors.hasType.MISMATCH, errors%to_string())

        maybe_chemical = combine_by_weight_factors( &
                natural_hydrogen_gas(), &
                1.0d0, &
                natural_helium_gas(), &
                1.0d0)
        errors = maybe_chemical%errors()
        result_ = result_.and.assert_that(errors.hasType.MISMATCH, errors%to_string())
    end function

    function check_combine() result(result_)
        type(result_t) :: result_

        character(len=*), parameter :: PROCEDURE_NAME = "check_combine"
        type(chemical_t) :: combined
        type(error_list_t) :: errors
        type(fallible_chemical_t) :: maybe_combined
        type(fallible_element_t) :: maybe_H_1
        type(fallible_chemical_t) :: maybe_H_1_gas
        type(fallible_element_t) :: maybe_H_2
        type(fallible_chemical_t) :: maybe_H_2_gas
        type(element_t) :: pure_H_1
        type(chemical_t) :: pure_H_1_gas
        type(element_t) :: pure_H_2
        type(chemical_t) :: pure_H_2_gas

        maybe_H_1 = from_atom_fractions(H, [element_component_t(H_1, 1.0d0)])
        maybe_H_2 = from_atom_fractions(H, [element_component_t(H_2, 1.0d0)])
        if (any([maybe_H_1%failed(), maybe_H_2%failed()])) then
            errors = error_list_t( &
                    pack([maybe_H_1%errors(), maybe_H_2%errors()], [maybe_H_1%failed(), maybe_H_2%failed()]), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
            result_ = fail(errors%to_string())
        else
            pure_H_1 = maybe_H_1%element()
            pure_H_2 = maybe_H_2%element()
            maybe_H_1_gas = fallible_chemical_t( &
                    hydrogen_gas_symbol(), &
                    [chemical_component_t(pure_H_1, 2.0d0)])
            maybe_H_2_gas = fallible_chemical_t( &
                    hydrogen_gas_symbol(), &
                    [chemical_component_t(pure_H_2, 2.0d0)])
            if (any([maybe_H_1_gas%failed(), maybe_H_2_gas%failed()])) then
                errors = error_list_t( &
                        pack([maybe_H_1_gas%errors(), maybe_H_2_gas%errors()], [maybe_H_1_gas%failed(), maybe_H_2_gas%failed()]), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
                result_ = fail(errors%to_string())
            else
                pure_H_1_gas = maybe_H_1_gas%chemical()
                pure_H_2_gas = maybe_H_2_gas%chemical()
                maybe_combined = combine_by_atom_factors( &
                        pure_H_1_gas, 0.6d0, pure_H_2_gas, 0.4d0)
                if (maybe_combined%failed()) then
                    errors = maybe_combined%errors()
                    result_ = fail(errors%to_string())
                else
                    combined = maybe_combined%chemical()
                    result_ = &
                            assert_equals( &
                                    0.6d0, &
                                    combined%atom_fraction(H_1), &
                                    "H-1 atom fraction") &
                            .and.assert_equals( &
                                    0.4d0, &
                                    combined%atom_fraction(H_2), &
                                    "H-2 atom fraction")
                end if
                maybe_combined = combine_by_weight_factors( &
                        pure_H_1_gas, 0.6d0, pure_H_2_gas, 0.4d0)
                if (maybe_combined%failed()) then
                    errors = maybe_combined%errors()
                    result_ = result_.and.fail(errors%to_string())
                else
                    combined = maybe_combined%chemical()
                    result_ = &
                            result_ &
                            .and.assert_equals( &
                                    0.6d0, &
                                    combined%weight_fraction(H_1), &
                                    "H-1 weight fraction") &
                            .and.assert_equals( &
                                    0.4d0, &
                                    combined%weight_fraction(H_2), &
                                    "H-2 weight fraction")
                end if
            end if
        end if
    end function

    function check_water_fractions() result(result_)
        type(result_t) :: result_

        type(chemical_t) :: water

        water = natural_water()
        result_ = &
                assert_equals(2.0d0 / 3.0d0, water%atom_fraction(H)) &
                .and.assert_equals(1.0d0 / 3.0d0, water%atom_fraction(O))
    end function

    function check_not_found() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                0, &
                find( &
                        water_symbol(), &
                        [ natural_hydrogen_gas() &
                        , natural_helium_gas() &
                        ]))
    end function

    function check_find() result(result_)
        type(result_t) :: result_

        result_ = assert_equals( &
                2, &
                find( &
                        helium_gas_symbol(), &
                        [ natural_hydrogen_gas() &
                        , natural_helium_gas() &
                        , natural_water() &
                        ]))
    end function
end module
