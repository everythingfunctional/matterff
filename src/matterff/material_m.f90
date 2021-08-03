module matterff_material_m
    use jsonff, only: &
            json_array_t, &
            json_element_t, &
            json_number_t, &
            json_object_t, &
            json_member_unsafe, &
            json_string_unsafe
    use matterff_chemical_m, only: &
            find, &
            natural_argon_gas, &
            natural_helium_gas, &
            natural_hydrogen_gas, &
            natural_krypton_gas, &
            natural_nitrogen_gas, &
            natural_oxygen_gas, &
            natural_water, &
            natural_xenon_gas
    use matterff_chemical_symbol_m, only: chemical_symbol_t
    use matterff_element_symbol_m, only: element_symbol_t
    use matterff_isotope_m, only: isotope_t
    use matterff_isotope_symbol_m, only: isotope_symbol_t
    use matterff_material_component_m, only: &
            material_component_t, combine_duplicates
    use quaff, only: amount_t, mass_t, molar_mass_t, operator(/), sum

    implicit none
    private
    public :: &
            material_t, &
            combine_by_atom_factors_unsafe, &
            combine_by_weight_factors_unsafe, &
            from_atom_fractions_unsafe, &
            from_weight_fractions_unsafe, &
            pure_natural_argon_gas, &
            pure_natural_helium_gas, &
            pure_natural_hydrogen_gas, &
            pure_natural_krypton_gas, &
            pure_natural_nitrogen_gas, &
            pure_natural_oxygen_gas, &
            pure_natural_water, &
            pure_natural_xenon_gas

    type :: material_t
        private
        type(material_component_t), allocatable :: components_(:)
    contains
        private
        procedure, public :: components
        procedure :: amount_element
        procedure :: amount_isotope
        generic, public :: amount => amount_element, amount_isotope
        procedure :: atom_fraction_chemical
        procedure :: atom_fraction_element
        procedure :: atom_fraction_isotope
        procedure :: atom_fraction_isotope_symbol
        generic, public :: atom_fraction => &
                atom_fraction_chemical, &
                atom_fraction_element, &
                atom_fraction_isotope, &
                atom_fraction_isotope_symbol
        procedure, public :: molar_mass
        procedure, public :: to_json
        procedure :: weight_fraction_chemical
        procedure :: weight_fraction_element
        procedure :: weight_fraction_isotope
        procedure :: weight_fraction_isotope_symbol
        generic, public :: weight_fraction => &
                weight_fraction_chemical, &
                weight_fraction_element, &
                weight_fraction_isotope, &
                weight_fraction_isotope_symbol
    end type

    interface combine_by_atom_factors_unsafe
        module procedure combine_materials_by_atom_factors_unsafe
    end interface

    interface combine_by_weight_factors_unsafe
        module procedure combine_materials_by_weight_factors_unsafe
    end interface

    interface from_atom_fractions_unsafe
        module procedure material_from_atom_fractions_unsafe
    end interface

    interface from_weight_fractions_unsafe
        module procedure material_from_weight_fractions_unsafe
    end interface
contains
    pure function combine_materials_by_atom_factors_unsafe( &
            material1, factor1, material2, factor2) result(combined)
        type(material_t), intent(in) :: material1
        double precision, intent(in) :: factor1
        type(material_t), intent(in) :: material2
        double precision, intent(in) :: factor2
        type(material_t) :: combined

        combined = from_atom_fractions_unsafe( &
                material_component_t( &
                        [material1%components_%chemical(), material2%components_%chemical()], &
                        [material1%components_%fraction_()*factor1, material2%components_%fraction_()*factor2]))
    end function

    pure function combine_materials_by_weight_factors_unsafe( &
            material1, factor1, material2, factor2) result(combined)
        type(material_t), intent(in) :: material1
        double precision, intent(in) :: factor1
        type(material_t), intent(in) :: material2
        double precision, intent(in) :: factor2
        type(material_t) :: combined

        associate( &
                chemicals1 => material1%components_%chemical(), &
                chemicals2 => material2%components_%chemical())
            combined = from_weight_fractions_unsafe( &
                    material_component_t( &
                            [chemicals1, chemicals2], &
                            [ material1%weight_fraction(chemicals1%symbol())*factor1 &
                            , material2%weight_fraction(chemicals2%symbol())*factor2 &
                            ]))
        end associate
    end function

    pure function material_from_atom_fractions_unsafe(components) result(material)
        type(material_component_t), intent(in) :: components(:)
        type(material_t) :: material

        allocate(material%components_, source = combine_duplicates(components))
    end function

    pure function material_from_weight_fractions_unsafe(components) result(material)
        type(material_component_t), intent(in) :: components(:)
        type(material_t) :: material

        type(mass_t), parameter :: ONE_KILOGRAM = mass_t(kilograms = 1.0d0)

        associate(chemicals => components%chemical())
            associate(&
                    amounts => &
                            components%fraction_() &
                            * ONE_KILOGRAM &
                            / chemicals%molar_mass())
                material = from_atom_fractions_unsafe( &
                        material_component_t(chemicals, amounts / sum(amounts)))
            end associate
        end associate
    end function

    pure function pure_natural_argon_gas()
        type(material_t) :: pure_natural_argon_gas

        allocate(pure_natural_argon_gas%components_, source = &
                [material_component_t(natural_argon_gas(), 1.0d0)])
    end function

    pure function pure_natural_helium_gas()
        type(material_t) :: pure_natural_helium_gas

        allocate(pure_natural_helium_gas%components_, source = &
                [material_component_t(natural_helium_gas(), 1.0d0)])
    end function

    pure function pure_natural_hydrogen_gas()
        type(material_t) :: pure_natural_hydrogen_gas

        allocate(pure_natural_hydrogen_gas%components_, source = &
                [material_component_t(natural_hydrogen_gas(), 1.0d0)])
    end function

    pure function pure_natural_krypton_gas()
        type(material_t) :: pure_natural_krypton_gas

        allocate(pure_natural_krypton_gas%components_, source = &
                [material_component_t(natural_krypton_gas(), 1.0d0)])
    end function

    pure function pure_natural_nitrogen_gas()
        type(material_t) :: pure_natural_nitrogen_gas

        allocate(pure_natural_nitrogen_gas%components_, source = &
                [material_component_t(natural_nitrogen_gas(), 1.0d0)])
    end function

    pure function pure_natural_oxygen_gas()
        type(material_t) :: pure_natural_oxygen_gas

        allocate(pure_natural_oxygen_gas%components_, source = &
                [material_component_t(natural_oxygen_gas(), 1.0d0)])
    end function

    pure function pure_natural_water()
        type(material_t) :: pure_natural_water

        allocate(pure_natural_water%components_, source = &
                [material_component_t(natural_water(), 1.0d0)])
    end function

    pure function pure_natural_xenon_gas()
        type(material_t) :: pure_natural_xenon_gas

        allocate(pure_natural_xenon_gas%components_, source = &
                [material_component_t(natural_xenon_gas(), 1.0d0)])
    end function

    pure function components(self)
        class(material_t), intent(in) :: self
        type(material_component_t), allocatable :: components(:)

        components = self%components_
    end function

    elemental function amount_element(self, total_amount, element) result(amount)
        class(material_t), intent(in) :: self
        type(amount_t), intent(in) :: total_amount
        type(element_symbol_t), intent(in) :: element
        type(amount_t) :: amount

        associate(chemicals => self%components_%chemical())
            amount = sum(chemicals%amount(total_amount, element) * self%components_%fraction_())
        end associate
    end function

    elemental function amount_isotope(self, total_amount, isotope) result(amount)
        class(material_t), intent(in) :: self
        type(amount_t), intent(in) :: total_amount
        type(isotope_t), intent(in) :: isotope
        type(amount_t) :: amount

        associate(chemicals => self%components_%chemical())
            amount = sum(chemicals%amount(total_amount, isotope) * self%components_%fraction_())
        end associate
    end function

    elemental function atom_fraction_chemical(self, chemical) result(atom_fraction)
        class(material_t), intent(in) :: self
        type(chemical_symbol_t), intent(in) :: chemical
        double precision :: atom_fraction

        associate(position => find(chemical, self%components_%chemical()))
            if (position > 0) then
                atom_fraction = self%components_(position)%fraction_()
            else
                atom_fraction = 0.0d0
            end if
        end associate
    end function

    elemental function atom_fraction_element(self, element) result(atom_fraction)
        class(material_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        double precision :: atom_fraction

        associate(chemicals => self%components_%chemical())
            atom_fraction = sum( &
                    chemicals%atom_fraction(element) &
                    * self%components_%fraction_())
        end associate
    end function

    elemental function atom_fraction_isotope(self, isotope) result(atom_fraction)
        class(material_t), intent(in) :: self
        type(isotope_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = self%atom_fraction(isotope%symbol())
    end function

    elemental function atom_fraction_isotope_symbol(self, isotope) result(atom_fraction)
        class(material_t), intent(in) :: self
        type(isotope_symbol_t), intent(in) :: isotope
        double precision :: atom_fraction

        associate(chemicals => self%components_%chemical())
            atom_fraction = sum( &
                    chemicals%atom_fraction(isotope) &
                    * self%components_%fraction_())
        end associate
    end function

    elemental function molar_mass(self)
        class(material_t), intent(in) :: self
        type(molar_mass_t) :: molar_mass

        associate(chemicals => self%components_%chemical())
            molar_mass = sum(self%components_%fraction_() * chemicals%molar_mass())
        end associate
    end function

    function to_json(self) result(json)
        class(material_t), intent(in) :: self
        type(json_object_t) :: json

        associate( &
                chemicals => self%components_%chemical(), &
                fractions => self%components_%fraction_())
            json = json_object_t([json_member_unsafe( &
                    "atom fractions", &
                    json_array_t(json_element_t( &
                            chemicals%to_json_with_fraction(fractions))))])
        end associate
    end function

    elemental function weight_fraction_chemical(self, chemical) result(weight_fraction)
        class(material_t), intent(in) :: self
        type(chemical_symbol_t), intent(in) :: chemical
        double precision :: weight_fraction

        associate(chemicals => self%components_%chemical())
            associate(position => find(chemical, chemicals))
                if (position > 0) then
                    associate(masses => self%components_%fraction_() * chemicals%molar_mass())
                        weight_fraction = masses(position) / sum(masses)
                    end associate
                else
                    weight_fraction = 0.0d0
                end if
            end associate
        end associate
    end function

    elemental function weight_fraction_element(self, element) result(weight_fraction)
        class(material_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        double precision :: weight_fraction

        associate(chemicals => self%components_%chemical())
            weight_fraction = sum( &
                    self%weight_fraction(chemicals%symbol()) &
                    * chemicals%weight_fraction(element))
        end associate
    end function

    elemental function weight_fraction_isotope(self, isotope) result(weight_fraction)
        class(material_t), intent(in) :: self
        type(isotope_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = self%weight_fraction(isotope%symbol())
    end function

    elemental function weight_fraction_isotope_symbol(self, isotope) result(weight_fraction)
        class(material_t), intent(in) :: self
        type(isotope_symbol_t), intent(in) :: isotope
        double precision :: weight_fraction

        associate(chemicals => self%components_%chemical())
            weight_fraction = sum( &
                    self%weight_fraction(chemicals%symbol()) &
                    * chemicals%weight_fraction(isotope))
        end associate
    end function
end module
