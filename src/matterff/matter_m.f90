module matterff_matter_m
    use jsonff, only: json_object_t, json_member_unsafe, json_string_unsafe
    use matterff_chemical_symbol_m, only: chemical_symbol_t
    use matterff_element_symbol_m, only: element_symbol_t
    use matterff_isotope_m, only: isotope_t
    use matterff_material_m, only: material_t, combine_by_atom_factors_unsafe
    use quaff, only: amount_t, mass_t, operator(*), operator(/)

    implicit none
    private
    public :: matter_t, matter_unsafe

    type :: matter_t
        private
        type(amount_t) :: amount_
        type(material_t) :: material
    contains
        private
        procedure :: combine_matter
        generic, public :: operator(+) => combine_matter
        procedure :: total_amount
        procedure :: amount_chemical
        procedure :: amount_element
        procedure :: amount_isotope
        generic, public :: amount => &
                total_amount, amount_chemical, amount_element, amount_isotope
        procedure :: total_mass
        procedure :: mass_chemical
        procedure :: mass_element
        procedure :: mass_isotope
        generic, public :: mass => &
                total_mass, mass_chemical, mass_element, mass_isotope
        procedure, public :: to_json
    end type

    interface matter_unsafe
        module procedure matter_with_amount_unsafe
        module procedure matter_with_mass_unsafe
    end interface
contains
    pure function matter_with_amount_unsafe(amount, material) result(matter)
        type(amount_t), intent(in) :: amount
        type(material_t), intent(in) :: material
        type(matter_t) :: matter

        matter%amount_ = amount
        matter%material = material
    end function

    pure function matter_with_mass_unsafe(mass, material) result(matter)
        type(mass_t), intent(in) :: mass
        type(material_t), intent(in) :: material
        type(matter_t) :: matter

        matter = matter_unsafe(mass / material%molar_mass(), material)
    end function

    pure function combine_matter(lhs, rhs) result(combined)
        class(matter_t), intent(in) :: lhs
        type(matter_t), intent(in) :: rhs
        type(matter_t) :: combined

        combined%amount_ = lhs%amount_ + rhs%amount_
        combined%material = combine_by_atom_factors_unsafe( &
                lhs%material, &
                lhs%amount_ / combined%amount_, &
                rhs%material, &
                rhs%amount_ / combined%amount_)
    end function

    pure function total_amount(self) result(amount)
        class(matter_t), intent(in) :: self
        type(amount_t) :: amount

        amount = self%amount_
    end function

    pure function amount_chemical(self, chemical) result(amount)
        class(matter_t), intent(in) :: self
        type(chemical_symbol_t), intent(in) :: chemical
        type(amount_t) :: amount

        amount = self%amount_ * self%material%atom_fraction(chemical)
    end function

    pure function amount_element(self, element) result(amount)
        class(matter_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        type(amount_t) :: amount

        amount = self%material%amount(self%amount_, element)
    end function

    pure function amount_isotope(self, isotope) result(amount)
        class(matter_t), intent(in) :: self
        type(isotope_t), intent(in) :: isotope
        type(amount_t) :: amount

        amount = self%material%amount(self%amount_, isotope)
    end function

    pure function total_mass(self) result(mass)
        class(matter_t), intent(in) :: self
        type(mass_t) :: mass

        mass = self%amount_ * self%material%molar_mass()
    end function

    pure function mass_chemical(self, chemical) result(mass)
        class(matter_t), intent(in) :: self
        type(chemical_symbol_t), intent(in) :: chemical
        type(mass_t) :: mass

        mass = self%mass() * self%material%weight_fraction(chemical)
    end function

    pure function mass_element(self, element) result(mass)
        class(matter_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        type(mass_t) :: mass

        mass = self%mass() * self%material%weight_fraction(element)
    end function

    pure function mass_isotope(self, isotope) result(mass)
        class(matter_t), intent(in) :: self
        type(isotope_t), intent(in) :: isotope
        type(mass_t) :: mass

        mass = self%mass() * self%material%weight_fraction(isotope)
    end function

    function to_json(self) result(json)
        class(matter_t), intent(in) :: self
        type(json_object_t) :: json

        json = json_object_t( &
                [ json_member_unsafe("amount", json_string_unsafe(self%amount_%to_string())) &
                , json_member_unsafe("material", self%material%to_json()) &
                ])
    end function
end module
