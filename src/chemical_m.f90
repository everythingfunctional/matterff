module chemical_m
    use chemical_component_m, only: chemical_component_t, combine_duplicates
    use chemical_symbol_m, only: &
            chemical_symbol_t, &
            argon_gas_symbol, &
            helium_gas_symbol, &
            hydrogen_gas_symbol, &
            krypton_gas_symbol, &
            nitrogen_gas_symbol, &
            oxygen_gas_symbol, &
            water_symbol, &
            xenon_gas_symbol
    use element_m, only: &
            find, &
            natural_hydrogen, &
            natural_helium, &
            natural_nitrogen, &
            natural_oxygen, &
            natural_argon, &
            natural_krypton, &
            natural_xenon
    use element_symbol_m, only: element_symbol_t
    use isotope_m, only: isotope_t
    use isotope_symbol_m, only: isotope_symbol_t
    use jsonff, only: &
            json_array_t, &
            json_element_t, &
            json_number_t, &
            json_object_t, &
            json_member_unsafe
    use quaff, only: &
            amount_t, &
            mass_t, &
            molar_mass_t, &
            operator(/), &
            operator(.unit.), &
            sum, &
            MOLS

    implicit none
    private
    public :: &
            chemical_t, &
            chemical_unsafe, &
            combine_by_atom_factors_unsafe, &
            combine_by_weight_factors_unsafe, &
            find, &
            natural_argon_gas, &
            natural_helium_gas, &
            natural_hydrogen_gas, &
            natural_krypton_gas, &
            natural_nitrogen_gas, &
            natural_oxygen_gas, &
            natural_water, &
            natural_xenon_gas

    type :: chemical_t
        private
        type(chemical_symbol_t) :: symbol_
        type(chemical_component_t), allocatable :: components_(:)
    contains
        private
        procedure, public :: symbol
        procedure :: amount_element
        procedure :: amount_isotope
        generic, public :: amount => amount_element, amount_isotope
        procedure :: atom_fraction_element
        procedure :: atom_fraction_isotope
        procedure :: atom_fraction_isotope_symbol
        generic, public :: atom_fraction => &
                atom_fraction_element, &
                atom_fraction_isotope, &
                atom_fraction_isotope_symbol
        procedure, public :: molar_mass
        procedure, public :: to_json_with_fraction
        procedure :: weight_fraction_element
        procedure :: weight_fraction_isotope
        procedure :: weight_fraction_isotope_symbol
        generic, public :: weight_fraction => &
                weight_fraction_element, &
                weight_fraction_isotope, &
                weight_fraction_isotope_symbol
    end type

    interface combine_by_atom_factors_unsafe
        module procedure combine_chemicals_by_atom_factors_unsafe
    end interface

    interface combine_by_weight_factors_unsafe
        module procedure combine_chemicals_by_weight_factors_unsafe
    end interface

    interface find
        module procedure find_chemical
    end interface
contains
    pure function chemical_unsafe(symbol, components) result(new_chemical)
        type(chemical_symbol_t), intent(in) :: symbol
        type(chemical_component_t), intent(in) :: components(:)
        type(chemical_t) :: new_chemical

        new_chemical%symbol_ = symbol
        new_chemical%components_ = combine_duplicates(components)
    end function

    pure function combine_chemicals_by_atom_factors_unsafe( &
            chemical1, factor1, chemical2, factor2) result(combined)
        type(chemical_t), intent(in) :: chemical1
        double precision, intent(in) :: factor1
        type(chemical_t), intent(in) :: chemical2
        double precision, intent(in) :: factor2
        type(chemical_t) :: combined

        combined = chemical_unsafe( &
                chemical1%symbol_, &
                chemical_component_t( &
                        [chemical1%components_%element(), chemical2%components_%element()], &
                        [chemical1%components_%multiplier() * factor1, chemical2%components_%multiplier() * factor2]))
    end function

    pure function combine_chemicals_by_weight_factors_unsafe( &
            chemical1, factor1, chemical2, factor2) result(combined)
        type(chemical_t), intent(in) :: chemical1
        double precision, intent(in) :: factor1
        type(chemical_t), intent(in) :: chemical2
        double precision, intent(in) :: factor2
        type(chemical_t) :: combined

        type(mass_t), parameter :: ONE_KILOGRAM = mass_t(kilograms = 1.0d0)

        associate( &
                amount1 => factor1 * ONE_KILOGRAM / chemical1%molar_mass(), &
                amount2 => factor2 * ONE_KILOGRAM / chemical2%molar_mass())
            associate(total_amount => amount1 + amount2)
                combined = combine_by_atom_factors_unsafe( &
                        chemical1, &
                        amount1 / total_amount, &
                        chemical2, &
                        amount2 / total_amount)
            end associate
        end associate
    end function

    pure function natural_argon_gas()
        type(chemical_t) :: natural_argon_gas

        natural_argon_gas%symbol_ = argon_gas_symbol()
        natural_argon_gas%components_ = &
                [chemical_component_t(natural_argon(), 1.0d0)]
    end function

    pure function natural_helium_gas()
        type(chemical_t) :: natural_helium_gas

        natural_helium_gas%symbol_ = helium_gas_symbol()
        natural_helium_gas%components_ = &
                [chemical_component_t(natural_helium(), 1.0d0)]
    end function

    pure function natural_hydrogen_gas()
        type(chemical_t) :: natural_hydrogen_gas

        natural_hydrogen_gas%symbol_ = hydrogen_gas_symbol()
        natural_hydrogen_gas%components_ = &
                [chemical_component_t(natural_hydrogen(), 2.0d0)]
    end function

    pure function natural_krypton_gas()
        type(chemical_t) :: natural_krypton_gas

        natural_krypton_gas%symbol_ = krypton_gas_symbol()
        natural_krypton_gas%components_ = &
                [chemical_component_t(natural_krypton(), 1.0d0)]
    end function

    pure function natural_nitrogen_gas()
        type(chemical_t) :: natural_nitrogen_gas

        natural_nitrogen_gas%symbol_ = nitrogen_gas_symbol()
        natural_nitrogen_gas%components_ = &
                [chemical_component_t(natural_nitrogen(), 2.0d0)]
    end function

    pure function natural_oxygen_gas()
        type(chemical_t) :: natural_oxygen_gas

        natural_oxygen_gas%symbol_ = oxygen_gas_symbol()
        natural_oxygen_gas%components_ = &
                [chemical_component_t(natural_oxygen(), 2.0d0)]
    end function

    pure function natural_water()
        type(chemical_t) :: natural_water

        natural_water%symbol_ = water_symbol()
        natural_water%components_ = &
                [ chemical_component_t(natural_hydrogen(), 2.0d0) &
                , chemical_component_t(natural_oxygen(), 1.0d0) &
                ]
    end function

    pure function natural_xenon_gas()
        type(chemical_t) :: natural_xenon_gas

        natural_xenon_gas%symbol_ = xenon_gas_symbol()
        natural_xenon_gas%components_ = &
                [chemical_component_t(natural_xenon(), 1.0d0)]
    end function

    pure function symbol(self)
        class(chemical_t), intent(in) :: self
        type(chemical_symbol_t) :: symbol

        symbol = self%symbol_
    end function

    elemental function amount_element(self, total_amount, element) result(amount)
        class(chemical_t), intent(in) :: self
        type(amount_t), intent(in) :: total_amount
        type(element_symbol_t), intent(in) :: element
        type(amount_t) :: amount

        associate(position => find(element, self%components_%element()))
            if (position > 0) then
                amount = self%components_(position)%multiplier() * total_amount
            else
                amount = 0.0d0.unit.MOLS
            end if
        end associate
    end function

    elemental function amount_isotope(self, total_amount, isotope) result(amount)
        class(chemical_t), intent(in) :: self
        type(amount_t), intent(in) :: total_amount
        type(isotope_t), intent(in) :: isotope
        type(amount_t) :: amount

        associate(elements => self%components_%element())
            amount = sum(self%components_%multiplier() * elements%atom_fraction(isotope) * total_amount)
        end associate
    end function

    elemental function atom_fraction_element(self, element) result(atom_fraction)
        class(chemical_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        double precision :: atom_fraction

        associate(position => find(element, self%components_%element()))
            if (position > 0) then
                atom_fraction = &
                        self%components_(position)%multiplier() &
                        / sum(self%components_%multiplier())
            else
                atom_fraction = 0.0d0
            end if
        end associate
    end function

    elemental function atom_fraction_isotope(self, isotope) result(atom_fraction)
        class(chemical_t), intent(in) :: self
        type(isotope_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = self%atom_fraction(isotope%symbol())
    end function

    elemental function atom_fraction_isotope_symbol(self, isotope) result(atom_fraction)
        class(chemical_t), intent(in) :: self
        type(isotope_symbol_t), intent(in) :: isotope
        double precision :: atom_fraction

        associate( &
                elements => self%components_%element(), &
                multipliers => self%components_%multiplier())
            atom_fraction = sum(multipliers / sum(multipliers) * elements%atom_fraction(isotope))
        end associate
    end function

    elemental function molar_mass(self)
        class(chemical_t), intent(in) :: self
        type(molar_mass_t) :: molar_mass

        associate(elements => self%components_%element())
            molar_mass = sum(self%components_%multiplier() * elements%atomic_mass())
        end associate
    end function

    function to_json_with_fraction(self, fraction) result(json)
        class(chemical_t), intent(in) :: self
        double precision, intent(in) :: fraction
        type(json_object_t) :: json

        associate( &
                elements => self%components_%element(), &
                multipliers => self%components_%multiplier())
            json = json_object_t( &
                    [ json_member_unsafe("fraction", json_number_t(fraction)) &
                    , json_member_unsafe("chemical", self%symbol_%to_json()) &
                    , json_member_unsafe("elements", json_array_t(json_element_t( &
                            elements%to_json_with_multiplier(multipliers)))) &
                    ])
        end associate
    end function

    elemental function weight_fraction_element(self, element) result(weight_fraction)
        class(chemical_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        double precision :: weight_fraction

        associate(elements => self%components_%element())
            associate(position => find(element, elements))
                if (position > 0) then
                    associate(multipliers => self%components_%multiplier())
                        associate(masses => multipliers / sum(multipliers) * elements%atomic_mass())
                            weight_fraction = masses(position) / sum(masses)
                        end associate
                    end associate
                else
                    weight_fraction = 0.0d0
                end if
            end associate
        end associate
    end function

    elemental function weight_fraction_isotope(self, isotope) result(weight_fraction)
        class(chemical_t), intent(in) :: self
        type(isotope_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = self%weight_fraction(isotope%symbol())
    end function

    elemental function weight_fraction_isotope_symbol(self, isotope) result(weight_fraction)
        class(chemical_t), intent(in) :: self
        type(isotope_symbol_t), intent(in) :: isotope
        double precision :: weight_fraction

        associate(elements => self%components_%element())
            weight_fraction = sum(self%weight_fraction(elements%symbol()) * elements%weight_fraction(isotope))
        end associate
    end function

    pure function find_chemical(symbol, chemicals) result(position)
        type(chemical_symbol_t), intent(in) :: symbol
        type(chemical_t), intent(in) :: chemicals(:)
        integer :: position

        integer :: i

        position = 0
        do i = 1, size(chemicals)
            if (chemicals(i)%symbol_ == symbol) then
                position = i
                exit
            end if
        end do
    end function
end module
