module Chemical_symbol_m
    use Chemical_symbol_component_m, only: &
            ChemicalSymbolComponent_t, ChemicalSymbolComponent
    use Element_symbol_m, only: ElementSymbol_t, H
    use iso_varying_string, only: VARYING_STRING
    use strff, only: join

    implicit none
    private

    type, public :: ChemicalSymbol_t
        private
        type(ChemicalSymbolComponent_t), allocatable :: components(:)
    contains
        private
        procedure, public :: includes
        procedure, public :: toString
    end type ChemicalSymbol_t

    public :: hydrogenGasSymbol
contains
    pure function hydrogenGasSymbol()
        type(ChemicalSymbol_t) :: hydrogenGasSymbol

        allocate(hydrogenGasSymbol%components, source = &
                [ChemicalSymbolComponent(H, 2)])
    end function hydrogenGasSymbol

    elemental function includes(self, element)
        class(ChemicalSymbol_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        logical :: includes

        includes = any(self%components%element == element)
    end function includes

    elemental function toString(self) result(string)
        class(ChemicalSymbol_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = join(self%components%toString(), "")
    end function toString
end module Chemical_symbol_m
