module Isotope_symbol_m
    use Element_symbol_m, only: ElementSymbol_t, H, He
    use iso_varying_string, only: VARYING_STRING, operator(//)
    use strff, only: toString

    implicit none
    private

    type, public :: IsotopeSymbol_t
        private
        type(ElementSymbol_t) :: element
        integer :: mass_number
    contains
        private
        procedure, public :: is
        procedure :: isotopeSymbolEquals
        generic, public :: operator(==) => isotopeSymbolEquals
        procedure, public :: toString => isotopeSymbolToString
    end type IsotopeSymbol_t

    type(IsotopeSymbol_t), parameter, public :: H_1_SYM = IsotopeSymbol_t(H, 1)
    type(IsotopeSymbol_t), parameter, public :: H_2_SYM = IsotopeSymbol_t(H, 2)
    type(IsotopeSymbol_t), parameter, public :: H_3_SYM = IsotopeSymbol_t(H, 3)

    type(IsotopeSymbol_t), parameter, public :: He_3_SYM = IsotopeSymbol_t(He, 3)
    type(IsotopeSymbol_t), parameter, public :: He_4_SYM = IsotopeSymbol_t(He, 4)
contains
    elemental function is(self, element)
        class(IsotopeSymbol_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        logical :: is

        is = self%element == element
    end function is

    elemental function isotopeSymbolEquals(lhs, rhs)
        class(IsotopeSymbol_t), intent(in) :: lhs
        type(IsotopeSymbol_t), intent(in) :: rhs
        logical :: isotopeSymbolEquals

        isotopeSymbolEquals = &
                lhs%element == rhs%element &
                .and. lhs%mass_number == rhs%mass_number
    end function isotopeSymbolEquals

    elemental function isotopeSymbolToString(self) result(string)
        class(IsotopeSymbol_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%element%toString() // "-" // toString(self%mass_number)
    end function isotopeSymbolToString
end module Isotope_symbol_m
