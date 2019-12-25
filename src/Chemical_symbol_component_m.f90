module Chemical_symbol_component_m
    use Element_symbol_m, only: ElementSymbol_t
    use iso_varying_string, only: VARYING_STRING, operator(//)
    use strff, only: toString

    implicit none
    private

    type, public :: ChemicalSymbolComponent_t
        private
        type(ElementSymbol_t), public :: element
        integer :: multiple
    contains
        private
        procedure :: chemicalSymbolComponentEquals
        generic, public :: operator(==) => chemicalSymbolComponentEquals
        procedure, public :: toString => symbolToString
    end type ChemicalSymbolComponent_t

    public :: ChemicalSymbolComponent
contains
    elemental function ChemicalSymbolComponent(element, multiple)
        type(ElementSymbol_t), intent(in) :: element
        integer, intent(in) :: multiple
        type(ChemicalSymbolComponent_t) :: ChemicalSymbolComponent

        ChemicalSymbolComponent%element = element
        ChemicalSymbolComponent%multiple = multiple
    end function ChemicalSymbolComponent

    elemental function chemicalSymbolComponentEquals(lhs, rhs)
        class(ChemicalSymbolComponent_t), intent(in) :: lhs
        type(ChemicalSymbolComponent_t), intent(in) :: rhs
        logical :: chemicalSymbolComponentEquals

        chemicalSymbolComponentEquals = &
                lhs%element == rhs%element &
                .and. lhs%multiple == rhs%multiple
    end function chemicalSymbolComponentEquals

    elemental function symbolToString(self) result(string)
        class(ChemicalSymbolComponent_t), intent(in) :: self
        type(VARYING_STRING) :: string

        if (self%multiple == 1) then
            string = self%element%toString()
        else
            string = self%element%toString() // toString(self%multiple)
        end if
    end function symbolToString
end module Chemical_symbol_component_m
