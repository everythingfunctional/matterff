module Chemical_symbol_component_m
    use Element_symbol_m, only: ElementSymbol_t

    implicit none
    private

    type, public :: ChemicalSymbolComponent_t
        private
        type(ElementSymbol_t) :: element
        integer :: multiple
    contains
        private
        procedure :: chemicalSymbolComponentEquals
        generic, public :: operator(==) => chemicalSymbolComponentEquals
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
end module Chemical_symbol_component_m
