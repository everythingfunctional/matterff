module Element_symbol_m
    use iso_varying_string, only: VARYING_STRING, assignment(=)

    implicit none
    private

    type, public :: ElementSymbol_t
        private
        character(len=2) :: symbol
    contains
        private
        procedure :: elementSymbolEquals
        generic, public :: operator(==) => elementSymbolEquals
        procedure, public :: toString
    end type ElementSymbol_t

    type(ElementSymbol_t), parameter, public :: H = ElementSymbol_t("H ")
    type(ElementSymbol_t), parameter, public :: He = ElementSymbol_t("He")
    type(ElementSymbol_t), parameter, public :: Li = ElementSymbol_t("Li")
    type(ElementSymbol_t), parameter, public :: Be = ElementSymbol_t("Be")
contains
    elemental function elementSymbolEquals(lhs, rhs)
        class(ElementSymbol_t), intent(in) :: lhs
        type(ElementSymbol_t), intent(in) :: rhs
        logical :: elementSymbolEquals

        elementSymbolEquals = lhs%symbol == rhs%symbol
    end function elementSymbolEquals

    elemental function toString(self) result(string)
        class(ElementSymbol_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = trim(self%symbol)
    end function toString
end module Element_symbol_m
