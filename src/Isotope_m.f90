module Isotope_m
    use Element_symbol_m, only: ElementSymbol_t
    use iso_varying_string, only: VARYING_STRING
    use Isotope_symbol_m, only: IsotopeSymbol_t, H_1_SYM, He_3_SYM
    use quaff, only: MolarMass_t

    implicit none
    private

    type, public :: Isotope_t
        private
        type(IsotopeSymbol_t), public :: symbol
        type(MolarMass_t) :: atomic_mass
    contains
        private
        procedure, public :: is
        procedure, public :: toString
    end type Isotope_t

    ! Atomic masses are taken from the 17th Edition of the Chart of Nuclides
    ! Where atomic mass is not provided for an isotope in the table, the
    ! atomic mass is taken from:
    ! [1] https://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl
    type(Isotope_t), parameter, public :: H_1 = Isotope_t(H_1_SYM, MolarMass_t(kilograms_per_mol = 1.0078250321d-3))

    type(Isotope_t), parameter, public :: He_3 = Isotope_t(He_3_SYM, MolarMass_t(kilograms_per_mol = 3.016029319d-3))
contains
    elemental function is(self, element)
        class(Isotope_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        logical :: is

        is = self%symbol%is(element)
    end function is

    elemental function toString(self) result(string)
        class(Isotope_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%symbol%toString()
    end function toString
end module Isotope_m
