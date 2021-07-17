module Element_symbol_m
    use iso_varying_string, only: VARYING_STRING, assignment(=), char

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

    interface ElementSymbol
        module procedure ElementSymbolC
        module procedure ElementSymbolS
    end interface ElementSymbol

    type(ElementSymbol_t), parameter, public :: H = ElementSymbol_t("H ")
    type(ElementSymbol_t), parameter, public :: He = ElementSymbol_t("He")
    type(ElementSymbol_t), parameter, public :: Li = ElementSymbol_t("Li")
    type(ElementSymbol_t), parameter, public :: Be = ElementSymbol_t("Be")
    type(ElementSymbol_t), parameter, public :: B = ElementSymbol_t("B ")
    type(ElementSymbol_t), parameter, public :: C = ElementSymbol_t("C ")
    type(ElementSymbol_t), parameter, public :: N = ElementSymbol_t("N ")
    type(ElementSymbol_t), parameter, public :: O = ElementSymbol_t("O ")
    type(ElementSymbol_t), parameter, public :: Ar = ElementSymbol_t("Ar")
    type(ElementSymbol_t), parameter, public :: Kr = ElementSymbol_t("Kr")
    type(ElementSymbol_t), parameter, public :: Xe = ElementSymbol_t("Xe")

    public :: ElementSymbol
contains
    pure function ElementSymbolC(symbol)
        character(len=*), intent(in) :: symbol
        type(ElementSymbol_t) :: ElementSymbolC

        ElementSymbolC%symbol = symbol
    end function ElementSymbolC

    pure function ElementSymbolS(symbol)
        type(VARYING_STRING), intent(in) :: symbol
        type(ElementSymbol_t) :: ElementSymbolS

        ElementSymbolS = ElementSymbol(char(symbol))
    end function ElementSymbolS

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
