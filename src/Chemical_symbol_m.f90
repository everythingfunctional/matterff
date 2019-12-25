module Chemical_symbol_m
    use Chemical_symbol_component_m, only: &
            ChemicalSymbolComponent_t, ChemicalSymbolComponent
    use Element_symbol_m, only: ElementSymbol_t, H, He
    use iso_varying_string, only: VARYING_STRING
    use strff, only: join

    implicit none
    private

    type, public :: ChemicalSymbol_t
        private
        type(ChemicalSymbolComponent_t), allocatable :: components(:)
    contains
        private
        procedure :: chemicalSymbolEquals
        generic, public :: operator(==) => chemicalSymbolEquals
        procedure, public :: includes
        procedure, public :: toString
    end type ChemicalSymbol_t

    interface operator(.in.)
        module procedure componentInList
    end interface operator(.in.)

    interface operator(.notIn.)
        module procedure componentNotInList
    end interface operator(.notIn.)

    interface operator(.allIn.)
        module procedure allComponentsInList
    end interface operator(.allIn.)

    public :: hydrogenGasSymbol, heliumGasSymbol
contains
    pure function hydrogenGasSymbol()
        type(ChemicalSymbol_t) :: hydrogenGasSymbol

        allocate(hydrogenGasSymbol%components, source = &
                [ChemicalSymbolComponent(H, 2)])
    end function hydrogenGasSymbol

    pure function heliumGasSymbol()
        type(ChemicalSymbol_t) :: heliumGasSymbol

        allocate(heliumGasSymbol%components, source = &
                [ChemicalSymbolComponent(He, 1)])
    end function heliumGasSymbol

    elemental function chemicalSymbolEquals(lhs, rhs) result(areEqual)
        class(ChemicalSymbol_t), intent(in) :: lhs
        type(ChemicalSymbol_t), intent(in) :: rhs
        logical :: areEqual

        areEqual = &
                size(lhs%components) == size(rhs%components) &
                .and. (lhs%components.allIn.rhs%components) &
                .and. (rhs%components.allIn.lhs%components)
    end function chemicalSymbolEquals

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

    pure function componentInList(item, list) result(is_in)
        type(ChemicalSymbolComponent_t), intent(in) :: item
        type(ChemicalSymbolComponent_t), intent(in) :: list(:)
        logical :: is_in

        integer :: i

        is_in = .false.
        do i = 1, size(list)
            if (item == list(i)) then
                is_in = .true.
                exit
            end if
        end do
    end function componentInList

    pure function componentNotInList(item, list) result(is_not_in)
        type(ChemicalSymbolComponent_t), intent(in) :: item
        type(ChemicalSymbolComponent_t), intent(in) :: list(:)
        logical :: is_not_in

        is_not_in = .not. (item.in.list)
    end function componentNotInList

    pure function allComponentsInList(items, list) result(are_all_in)
        type(ChemicalSymbolComponent_t), intent(in) :: items(:)
        type(ChemicalSymbolComponent_t), intent(in) :: list(:)
        logical :: are_all_in

        integer :: i

        are_all_in = .true.
        do i = 1, size(items)
            if (items(i).notIn.list) then
                are_all_in = .false.
                exit
            end if
        end do
    end function allComponentsInList
end module Chemical_symbol_m
