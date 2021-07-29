module element_m
    use element_component_m, only: element_component_t, combine_duplicates
    use element_symbol_m, only: &
            element_symbol_t, &
            H, &
            He, &
            Li, &
            Be, &
            B, &
            C, &
            N, &
            O, &
            Ar, &
            Kr, &
            Xe
    use isotope_m, only: &
            isotope_t, &
            find, &
            H_1, &
            H_2, &
            He_3, &
            He_4, &
            Li_6, &
            Li_7, &
            Be_9, &
            B_10, &
            B_11, &
            C_12, &
            C_13, &
            N_14, &
            N_15, &
            O_16, &
            O_17, &
            O_18, &
            Ar_36, &
            Ar_38, &
            Ar_40, &
            Kr_78, &
            Kr_80, &
            Kr_82, &
            Kr_83, &
            Kr_84, &
            Kr_86, &
            Xe_124, &
            Xe_126, &
            Xe_128, &
            Xe_129, &
            Xe_130, &
            Xe_131, &
            Xe_132, &
            Xe_134, &
            Xe_136
    use jsonff, only: &
            json_array_t, &
            json_element_t, &
            json_number_t, &
            json_object_t, &
            json_member_unsafe, &
            json_string_unsafe
    use isotope_symbol_m, only: isotope_symbol_t
    use quaff, only: mass_t, molar_mass_t, operator(/), sum

    implicit none
    private
    public :: &
            element_t, &
            combine_by_atom_factors_unsafe, &
            combine_by_weight_factors_unsafe, &
            find, &
            from_atom_fractions_unsafe, &
            from_weight_fractions_unsafe, &
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
            natural_xenon

    type :: element_t
        private
        type(element_symbol_t) :: symbol_
        type(element_component_t), allocatable :: components_(:)
    contains
        private
        procedure, public :: symbol
        procedure, public :: components
        procedure :: atom_fraction_isotope
        procedure :: atom_fraction_symbol
        generic, public :: atom_fraction => &
                atom_fraction_isotope, atom_fraction_symbol
        procedure, public :: atomic_mass
        procedure, public :: to_json_with_multiplier
        procedure :: weight_fraction_isotope
        procedure :: weight_fraction_symbol
        generic, public :: weight_fraction => &
                weight_fraction_isotope, weight_fraction_symbol
    end type

    interface combine_by_atom_factors_unsafe
        module procedure combine_elements_by_atom_factors_unsafe
    end interface

    interface combine_by_weight_factors_unsafe
        module procedure combine_elements_by_weight_factors_unsafe
    end interface

    interface from_atom_fractions_unsafe
        module procedure element_from_atom_fractions_unsafe
    end interface

    interface from_weight_fractions_unsafe
        module procedure element_from_weight_fractions_unsafe
    end interface

    interface find
        module procedure find_element
    end interface
contains
    pure function combine_elements_by_atom_factors_unsafe( &
            element1, factor1, element2, factor2) result(combined)
        type(element_t), intent(in) :: element1
        double precision, intent(in) :: factor1
        type(element_t), intent(in) :: element2
        double precision, intent(in) :: factor2
        type(element_t) :: combined

        combined = from_atom_fractions_unsafe( &
                element1%symbol_, &
                element_component_t( &
                        [element1%components_%isotope(), element2%components_%isotope()], &
                        [element1%components_%fraction_()*factor1, element2%components_%fraction_()*factor2]))
    end function

    pure function combine_elements_by_weight_factors_unsafe( &
            element1, factor1, element2, factor2) result(combined)
        type(element_t), intent(in) :: element1
        double precision, intent(in) :: factor1
        type(element_t), intent(in) :: element2
        double precision, intent(in) :: factor2
        type(element_t) :: combined

        associate( &
                isotopes1 => element1%components_%isotope(), &
                isotopes2 => element2%components_%isotope())
            combined = from_weight_fractions_unsafe( &
                    element1%symbol_, &
                    element_component_t( &
                            [isotopes1, isotopes2], &
                            [element1%weight_fraction(isotopes1)*factor1, element2%weight_fraction(isotopes2)*factor2]))
        end associate
    end function

    pure function element_from_atom_fractions_unsafe( &
            symbol, components) result(element)
        type(element_symbol_t), intent(in) :: symbol
        type(element_component_t), intent(in) :: components(:)
        type(element_t) :: element

        element%symbol_ = symbol
        allocate(element%components_, source = combine_duplicates(components))
    end function

    pure function element_from_weight_fractions_unsafe( &
            symbol, components) result(element)
        type(element_symbol_t), intent(in) :: symbol
        type(element_component_t), intent(in) :: components(:)
        type(element_t) :: element

        type(mass_t), parameter :: ONE_KILOGRAM = mass_t(kilograms = 1.0d0)

        associate(isotopes => components%isotope())
            associate(&
                    amounts => &
                            components%fraction_() &
                            * ONE_KILOGRAM &
                            / isotopes%atomic_mass())
                element = from_atom_fractions_unsafe( &
                        symbol, &
                        element_component_t(isotopes, amounts / sum(amounts)))
            end associate
        end associate
    end function

    ! Atomic fractions are taken from the 17th Edition of the Chart of Nuclides
    pure function natural_hydrogen()
        type(element_t) :: natural_hydrogen

        natural_hydrogen%symbol_ = H
        allocate(natural_hydrogen%components_, source = &
                [ element_component_t(H_1, 0.999885d0) &
                , element_component_t(H_2, 0.000115d0) &
                ])
    end function

    pure function natural_helium()
        type(element_t) :: natural_helium

        natural_helium%symbol_ = He
        allocate(natural_helium%components_, source = &
                [ element_component_t(He_3, 0.000001344d0) &
                , element_component_t(He_4, 0.99999866d0) &
                ])
    end function

    pure function natural_lithium()
        type(element_t) :: natural_lithium

        natural_lithium%symbol_ = Li
        allocate(natural_lithium%components_, source = &
                [ element_component_t(Li_6, 0.0759d0) &
                , element_component_t(Li_7, 0.9241d0) &
                ])
    end function

    pure function natural_beryllium()
        type(element_t) :: natural_beryllium

        natural_beryllium%symbol_ = Be
        allocate(natural_beryllium%components_, source = &
                [element_component_t(Be_9, 1.0d0)])
    end function

    pure function natural_boron()
        type(element_t) :: natural_boron

        natural_boron%symbol_ = B
        allocate(natural_boron%components_, source = &
                [ element_component_t(B_10, 0.199d0) &
                , element_component_t(B_11, 0.801d0) &
                ])
    end function

    pure function natural_carbon()
        type(element_t) :: natural_carbon

        natural_carbon%symbol_ = C
        allocate(natural_carbon%components_, source = &
                [ element_component_t(C_12, 0.9893d0) &
                , element_component_t(C_13, 0.0107d0) &
                ])
    end function

    pure function natural_nitrogen()
        type(element_t) :: natural_nitrogen

        natural_nitrogen%symbol_ = N
        allocate(natural_nitrogen%components_, source = &
                [ element_component_t(N_14, 0.99636d0) &
                , element_component_t(N_15, 0.00364d0) &
                ])
    end function

    pure function natural_oxygen()
        type(element_t) :: natural_oxygen

        natural_oxygen%symbol_ = O
        allocate(natural_oxygen%components_, source = &
                [ element_component_t(O_16, 0.99757d0) &
                , element_component_t(O_17, 0.00038d0) &
                , element_component_t(O_18, 0.00205d0) &
                ])
    end function

    pure function natural_argon()
        type(element_t) :: natural_argon

        natural_argon%symbol_ = Ar
        allocate(natural_argon%components_, source = &
                [ element_component_t(Ar_36, 0.003365d0) &
                , element_component_t(Ar_38, 0.000632d0) &
                , element_component_t(Ar_40, 0.996003d0) &
                ])
    end function

    pure function natural_krypton()
        type(element_t) :: natural_krypton

        natural_krypton%symbol_ = Kr
        allocate(natural_krypton%components_, source = &
                [ element_component_t(Kr_78, 0.00355d0) &
                , element_component_t(Kr_80, 0.02286d0) &
                , element_component_t(Kr_82, 0.11593d0) &
                , element_component_t(Kr_83, 0.115d0) &
                , element_component_t(Kr_84, 0.56987d0) &
                , element_component_t(Kr_86, 0.17279d0) &
                ])
    end function

    pure function natural_xenon()
        type(element_t) :: natural_xenon

        natural_xenon%symbol_ = Xe
        allocate(natural_xenon%components_, source = &
                [ element_component_t(Xe_124, 0.000952d0) &
                , element_component_t(Xe_126, 0.00089d0) &
                , element_component_t(Xe_128, 0.019102d0) &
                , element_component_t(Xe_129, 0.264006d0) &
                , element_component_t(Xe_130, 0.040710d0) &
                , element_component_t(Xe_131, 0.212324d0) &
                , element_component_t(Xe_132, 0.269086d0) &
                , element_component_t(Xe_134, 0.104357d0) &
                , element_component_t(Xe_136, 0.088573d0) &
                ])
    end function

    elemental function symbol(self)
        class(element_t), intent(in) :: self
        type(element_symbol_t) :: symbol

        symbol = self%symbol_
    end function

    pure function components(self)
        class(element_t), intent(in) :: self
        type(element_component_t), allocatable :: components(:)

        components = self%components_
    end function

    elemental function atom_fraction_isotope(self, isotope) result(atom_fraction)
        class(element_t), intent(in) :: self
        type(isotope_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = self%atom_fraction(isotope%symbol())
    end function

    elemental function atom_fraction_symbol(self, isotope) result(atom_fraction)
        class(element_t), intent(in) :: self
        type(isotope_symbol_t), intent(in) :: isotope
        double precision :: atom_fraction

        associate(position => find(isotope, self%components_%isotope()))
            if (position > 0) then
                atom_fraction = self%components_(position)%fraction_()
            else
                atom_fraction = 0.0d0
            end if
        end associate
    end function

    elemental function atomic_mass(self)
        class(element_t), intent(in) :: self
        type(molar_mass_t) :: atomic_mass

        associate(isotopes => self%components_%isotope())
            atomic_mass = sum(self%components_%fraction_() * isotopes%atomic_mass())
        end associate
    end function

    impure elemental function to_json_with_multiplier(self, multiplier) result(json)
        class(element_t), intent(in) :: self
        double precision, intent(in) :: multiplier
        type(json_object_t) :: json

        associate( &
                isotopes => self%components_%isotope(), &
                fractions => self%components_%fraction_())
            json = json_object_t( &
                    [ json_member_unsafe("multiplier", json_number_t(multiplier)) &
                    , json_member_unsafe("element", json_string_unsafe(self%symbol_%to_string())) &
                    , json_member_unsafe("atom fractions", json_array_t(json_element_t( &
                            isotopes%to_json_with_fraction(fractions)))) &
                    ])
        end associate
    end function

    elemental function weight_fraction_isotope(self, isotope) result(weight_fraction)
        class(element_t), intent(in) :: self
        type(isotope_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = self%weight_fraction(isotope%symbol())
    end function

    elemental function weight_fraction_symbol(self, isotope) result(weight_fraction)
        class(element_t), intent(in) :: self
        type(isotope_symbol_t), intent(in) :: isotope
        double precision :: weight_fraction

        associate(isotopes => self%components_%isotope())
            associate(position => find(isotope, isotopes))
                if (position > 0) then
                    associate(masses => self%components_%fraction_() * isotopes%atomic_mass())
                        weight_fraction = masses(position) / sum(masses)
                    end associate
                else
                    weight_fraction = 0.0d0
                end if
            end associate
        end associate
    end function

    pure function find_element(symbol, elements) result(position)
        type(element_symbol_t), intent(in) :: symbol
        type(element_t), intent(in) :: elements(:)
        integer :: position

        integer :: i

        position = 0
        do i = 1, size(elements)
            if (elements(i)%symbol_ == symbol) then
                position = i
                exit
            end if
        end do
    end function
end module
