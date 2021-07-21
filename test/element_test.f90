module element_test
    use erloff, only: error_list_t, message_list_t
    use matterff, only: &
            element_t, &
            element_component_t, &
            fallible_element_t, &
            isotope_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors, &
            find, &
            from_atom_fractions, &
            from_weight_fractions, &
            natural_hydrogen, &
            natural_helium, &
            natural_lithium, &
            natural_beryllium, &
            natural_boron, &
            natural_carbon, &
            natural_nitrogen, &
            natural_oxygen, &
            natural_argon, &
            natural_krypton, &
            natural_xenon, &
            H, &
            H_1, &
            H_2, &
            He_3
    use matterff_utilities_m, only: &
            INVALID_ARGUMENT, MISMATCH, NORMALIZED_FRACTIONS
    use quaff, only: operator(.unit.), GRAMS_PER_MOL
    use quaff_asserts_m, only: assert_equals_within_absolute
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
    public :: test_element
contains
    function test_element() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "An element_t", &
                [ it( &
                        "creating an element with isotopes of a different element is an error", &
                        check_diff_isotopes) &
                , it( &
                        "creating an element with negative fractions is an error", &
                        check_negative_fractions) &
                , it( &
                        "a single isotope element is all that isotope", &
                        check_single_isotope) &
                , it("keeps track of its components", check_keeps_track) &
                , it( &
                        "has normalized fractions of its components", &
                        check_normalized_fractions) &
                , it( &
                        "normalizing fractions of isotopes produces a message", &
                        check_normalized_message) &
                , it( &
                        "created with duplicate isotopes has sum of duplicates", &
                        check_duplicates) &
                , it( &
                        "combining elements of a different type is an error", &
                        check_combine_different_elements_is_error) &
                , it( &
                        "combining elements results in correct fractions", &
                        check_combine) &
                , it( &
                        "natural compositions have their atomic mass from the Periodic Table", &
                        check_natural_elements) &
                , it("has a position of 0 if it's not in a list", check_not_found) &
                , it("can be found in a list", check_find) &
                ])
    end function

    function check_diff_isotopes() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_element

        maybe_element = from_atom_fractions( &
                H, [element_component_t(He_3, 1.0d0)])
        associate(errors => maybe_element%errors())
            result_ = assert_that(errors.hasType.MISMATCH, errors%to_string())
        end associate

        maybe_element = from_weight_fractions( &
                H, [element_component_t(He_3, 1.0d0)])
        associate(errors => maybe_element%errors())
            result_ = &
                    result_ &
                    .and.assert_that(errors.hasType.MISMATCH, errors%to_string())
        end associate
    end function

    function check_negative_fractions() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_element

        maybe_element = from_atom_fractions( &
                H, [element_component_t(H_1, -1.0d0)])
        associate(errors => maybe_element%errors())
            result_ = assert_that(errors.hasType.INVALID_ARGUMENT, errors%to_string())
        end associate

        maybe_element = from_weight_fractions( &
                H, [element_component_t(H_1, -1.0d0)])
        associate(errors => maybe_element%errors())
            result_ = &
                    result_ &
                    .and.assert_that(errors.hasType.INVALID_ARGUMENT, errors%to_string())
        end associate
    end function

    function check_single_isotope() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_element

        maybe_element = from_atom_fractions( &
                H, [element_component_t(H_1, 1.0d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = fail(errors%to_string())
            end associate
        else
            result_ = assert_all_isotope(H_1, maybe_element%element(), "atom fractions")
        end if

        maybe_element = from_weight_fractions( &
                H, [element_component_t(H_1, 1.0d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = result_.and.fail(errors%to_string())
            end associate
        else
            result_ = result_.and.assert_all_isotope(H_1, maybe_element%element(), "atom fractions")
        end if
    end function

    function check_keeps_track() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_element

        maybe_element = from_atom_fractions( &
                H, [element_component_t(H_1, 0.6d0), element_component_t(H_2, 0.4d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = fail(errors%to_string())
            end associate
        else
            associate(element => maybe_element%element())
                result_ = &
                        assert_equals( &
                                0.6d0, &
                                element%atom_fraction(H_1), &
                                "H-1 atom fraction") &
                        .and.assert_equals( &
                                0.4d0, &
                                element%atom_fraction(H_2), &
                                "H-2 atom fraction")
            end associate
        end if

        maybe_element = from_weight_fractions( &
                H, [element_component_t(H_1, 0.6d0), element_component_t(H_2, 0.4d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = result_.and.fail(errors%to_string())
            end associate
        else
            associate(element => maybe_element%element())
                result_ = &
                        result_ &
                        .and.assert_equals( &
                                0.6d0, &
                                element%weight_fraction(H_1), &
                                "H-1 weight fraction") &
                        .and.assert_equals( &
                                0.4d0, &
                                element%weight_fraction(H_2), &
                                "H-2 weight fraction")
            end associate
        end if
    end function

    function check_normalized_fractions() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_element

        maybe_element = from_atom_fractions( &
                H, [element_component_t(H_1, 0.06d0), element_component_t(H_2, 0.04d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = fail(errors%to_string())
            end associate
        else
            associate(element => maybe_element%element())
                result_ = &
                        assert_equals( &
                                0.6d0, &
                                element%atom_fraction(H_1), &
                                "H-1 atom fraction") &
                        .and.assert_equals( &
                                0.4d0, &
                                element%atom_fraction(H_2), &
                                "H-2 atom fraction")
            end associate
        end if

        maybe_element = from_weight_fractions( &
                H, [element_component_t(H_1, 0.06d0), element_component_t(H_2, 0.04d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = result_.and.fail(errors%to_string())
            end associate
        else
            associate(element => maybe_element%element())
                result_ = &
                        result_ &
                        .and.assert_equals( &
                                0.6d0, &
                                element%weight_fraction(H_1), &
                                "H-1 weight fraction") &
                        .and.assert_equals( &
                                0.4d0, &
                                element%weight_fraction(H_2), &
                                "H-2 weight fraction")
            end associate
        end if
    end function

    function check_normalized_message() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_element
        type(message_list_t) :: messages

        maybe_element = from_atom_fractions( &
                H, [element_component_t(H_1, 0.06d0), element_component_t(H_2, 0.04d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = fail(errors%to_string())
            end associate
        else
            messages = maybe_element%messages()
            result_ = &
                    assert_that( &
                            messages.hasType.NORMALIZED_FRACTIONS, &
                            messages%to_string())
        end if

        maybe_element = from_weight_fractions( &
                H, [element_component_t(H_1, 0.06d0), element_component_t(H_2, 0.04d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = result_.and.fail(errors%to_string())
            end associate
        else
            messages = maybe_element%messages()
            result_ = &
                    result_ &
                    .and.assert_that( &
                            messages.hasType.NORMALIZED_FRACTIONS, &
                            messages%to_string())
        end if
    end function

    function check_duplicates() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_element

        maybe_element = from_atom_fractions( &
                H, [element_component_t(H_1, 0.06d0), element_component_t(H_1, 0.04d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = fail(errors%to_string())
            end associate
        else
            result_ = assert_all_isotope(H_1, maybe_element%element(), "atom fractions")
        end if

        maybe_element = from_weight_fractions( &
                H, [element_component_t(H_1, 0.06d0), element_component_t(H_1, 0.04d0)])
        if (maybe_element%failed()) then
            associate(errors => maybe_element%errors())
                result_ = result_.and.fail(errors%to_string())
            end associate
        else
            result_ = result_.and.assert_all_isotope(H_1, maybe_element%element(), "weight fractions")
        end if
    end function

    function check_combine_different_elements_is_error() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_element
        type(error_list_t) :: errors

        maybe_element = combine_by_atom_factors( &
                natural_hydrogen(), &
                1.0d0, &
                natural_helium(), &
                1.0d0)
        errors = maybe_element%errors()
        result_ = assert_that( &
                errors.hasType.MISMATCH, &
                errors%to_string())

        maybe_element = combine_by_weight_factors( &
                natural_hydrogen(), &
                1.0d0, &
                natural_helium(), &
                1.0d0)
        errors = maybe_element%errors()
        result_ = result_.and.assert_that( &
                errors.hasType.MISMATCH, &
                errors%to_string())
    end function

    function check_combine() result(result_)
        type(result_t) :: result_

        type(fallible_element_t) :: maybe_combined
        type(fallible_element_t) :: maybe_H_1
        type(fallible_element_t) :: maybe_H_2

        maybe_H_1 = from_atom_fractions(H, [element_component_t(H_1, 1.0d0)])
        if (maybe_H_1%failed()) then
            associate(errors => maybe_H_1%errors())
                result_ = fail(errors%to_string())
            end associate
        else
            maybe_H_2 = from_atom_fractions(H, [element_component_t(H_2, 1.0d0)])
            if (maybe_H_2%failed()) then
                associate(errors => maybe_H_2%errors())
                    result_ = fail(errors%to_string())
                end associate
            else
                maybe_combined = combine_by_atom_factors( &
                        maybe_H_1%element(), 0.6d0, maybe_H_2%element(), 0.4d0)
                if (maybe_combined%failed()) then
                    associate(errors => maybe_combined%errors())
                        result_ = fail(errors%to_string())
                    end associate
                else
                    associate(combined => maybe_combined%element())
                        result_ = &
                                assert_equals( &
                                        0.6d0, &
                                        combined%atom_fraction(H_1), &
                                        "H-1 atom fraction") &
                                .and.assert_equals( &
                                        0.4d0, &
                                        combined%atom_fraction(H_2), &
                                        "H-2 atom fraction")
                    end associate
                end if
                maybe_combined = combine_by_weight_factors( &
                        maybe_H_1%element(), 0.6d0, maybe_H_2%element(), 0.4d0)
                if (maybe_combined%failed()) then
                    associate(errors => maybe_combined%errors())
                        result_ = result_.and.fail(errors%to_string())
                    end associate
                else
                    associate(combined => maybe_combined%element())
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
                    end associate
                end if
            end if
        end if
    end function

    pure function check_natural_elements() result(result_)
        type(result_t) :: result_

        type(element_t) :: hydrogen
        type(element_t) :: helium
        type(element_t) :: lithium
        type(element_t) :: beryllium
        type(element_t) :: boron
        type(element_t) :: carbon
        type(element_t) :: nitrogen
        type(element_t) :: oxygen
        type(element_t) :: argon
        type(element_t) :: krypton
        type(element_t) :: xenon

        hydrogen = natural_hydrogen()
        helium = natural_helium()
        lithium = natural_lithium()
        beryllium = natural_beryllium()
        boron = natural_boron()
        carbon = natural_carbon()
        nitrogen = natural_nitrogen()
        oxygen = natural_oxygen()
        argon = natural_argon()
        krypton = natural_krypton()
        xenon = natural_xenon()

        ! Atomic masses and tolerances are taken from the 17th Edition of the Chart of Nuclides
        result_ = &
                assert_equals_within_absolute( &
                        1.00794d0.unit.GRAMS_PER_MOL, &
                        hydrogen%atomic_mass(), &
                        0.00007d0.unit.GRAMS_PER_MOL, &
                        "H") &
                .and.assert_equals_within_absolute( &
                        4.002602d0.unit.GRAMS_PER_MOL, &
                        helium%atomic_mass(), &
                        0.000002d0.unit.GRAMS_PER_MOL, &
                        "He") &
                .and.assert_equals_within_absolute( &
                        6.941d0.unit.GRAMS_PER_MOL, &
                        lithium%atomic_mass(), &
                        0.002d0.unit.GRAMS_PER_MOL, &
                        "Li") &
                .and.assert_equals_within_absolute( &
                        9.012182d0.unit.GRAMS_PER_MOL, &
                        beryllium%atomic_mass(), &
                        0.000003d0.unit.GRAMS_PER_MOL, &
                        "Be") &
                .and.assert_equals_within_absolute( &
                        10.811d0.unit.GRAMS_PER_MOL, &
                        boron%atomic_mass(), &
                        0.007d0.unit.GRAMS_PER_MOL, &
                        "B") &
                .and.assert_equals_within_absolute( &
                        12.0107d0.unit.GRAMS_PER_MOL, &
                        carbon%atomic_mass(), &
                        0.0008d0.unit.GRAMS_PER_MOL, &
                        "C") &
                .and.assert_equals_within_absolute( &
                        14.0067d0.unit.GRAMS_PER_MOL, &
                        nitrogen%atomic_mass(), &
                        0.0002d0.unit.GRAMS_PER_MOL, &
                        "N") &
                .and.assert_equals_within_absolute( &
                        15.9994d0.unit.GRAMS_PER_MOL, &
                        oxygen%atomic_mass(), &
                        0.0003d0.unit.GRAMS_PER_MOL, &
                        "O") &
                .and.assert_equals_within_absolute( &
                        39.948d0.unit.GRAMS_PER_MOL, &
                        argon%atomic_mass(), &
                        0.001d0.unit.GRAMS_PER_MOL, &
                        "Ar") &
                .and.assert_equals_within_absolute( &
                        83.798d0.unit.GRAMS_PER_MOL, &
                        krypton%atomic_mass(), &
                        0.002d0.unit.GRAMS_PER_MOL, &
                        "Kr") &
                .and.assert_equals_within_absolute( &
                        131.293d0.unit.GRAMS_PER_MOL, &
                        xenon%atomic_mass(), &
                        0.006d0.unit.GRAMS_PER_MOL, &
                        "Xe")
    end function

    pure function check_not_found() result(result_)
        type(result_t) :: result_

        type(element_t) :: elements(3)

        elements(1) = natural_helium()
        elements(2) = natural_lithium()
        elements(3) = natural_beryllium()

        result_ = assert_equals(0, find(H, elements))
    end function

    pure function check_find() result(result_)
        type(result_t) :: result_

        type(element_t) :: elements(3)

        elements(1) = natural_hydrogen()
        elements(2) = natural_helium()
        elements(3) = natural_lithium()

        result_ = assert_equals(1, find(H, elements))
    end function

    pure function assert_all_isotope(isotope, element, from) result(result_)
        type(isotope_t), intent(in) :: isotope
        type(element_t), intent(in) :: element
        character(len=*), intent(in) :: from
        type(result_t) :: result_

        result_ = &
                assert_equals( &
                        1.0d0, &
                        element%atom_fraction(isotope), &
                        "atom fraction from " // from) &
                .and.assert_equals( &
                        1.0d0, &
                        element%weight_fraction(isotope), &
                        "weight fraction from " // from)
    end function
end module
