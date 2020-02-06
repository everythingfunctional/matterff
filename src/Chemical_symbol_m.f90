module Chemical_symbol_m
    use Chemical_symbol_component_m, only: &
            ChemicalSymbolComponent_t, ChemicalSymbolComponent, fromJson
    use Element_symbol_m, only: ElementSymbol_t, H, He, N, O, Ar, Kr, Xe
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
    use iso_varying_string, only: VARYING_STRING, char
    use jsonff, only: &
            JsonArray_t, &
            JsonElement_t, &
            JsonObject_t, &
            JsonArray, &
            JsonElement
    use strff, only: join
    use Utilities_m, only: INVALID_ARGUMENT_TYPE

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
        procedure, public :: toJson
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

    interface fromJson
        module procedure chemicalSymbolFromJson
    end interface fromJson

    interface fromString
        module procedure fromStringC
        module procedure fromStringS
    end interface fromString

    character(len=*), parameter :: MODULE_NAME = "Chemical_symbol_m"

    public :: &
            ChemicalSymbol, &
            fromJson, &
            fromString, &
            argonGasSymbol, &
            heliumGasSymbol, &
            hydrogenGasSymbol, &
            kryptonGasSymbol, &
            nitrogenGasSymbol, &
            waterSymbol, &
            xenonGasSymbol
contains
    pure function ChemicalSymbol(components)
        type(ChemicalSymbolComponent_t), intent(in) :: components(:)
        type(ChemicalSymbol_t) :: ChemicalSymbol

        allocate(ChemicalSymbol%components, source = components)
    end function ChemicalSymbol

    pure subroutine chemicalSymbolFromJson(json, errors, symbol)
        type(JsonArray_t), intent(in) :: json
        type(ErrorList_t), intent(out) :: errors
        type(ChemicalSymbol_t), intent(out) :: symbol

        character(len=*), parameter :: PROCEDURE_NAME = "chemicalSymbolFromJson"
        type(JsonElement_t) :: component_element
        type(ChemicalSymbolComponent_t), allocatable :: components(:)
        type(ErrorList_t) :: errors_
        integer :: i
        integer :: num_components

        num_components = json%length()
        allocate(components(num_components))
        do i = 1, num_components
            call json%getElement(i, errors_, component_element)
            if (errors_%hasAny()) then
                call errors%appendErrors( &
                        errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
            else
                select type (component_object => component_element%element)
                type is (JsonObject_t)
                    call fromJson(component_object, errors_, components(i))
                    if (errors_%hasAny()) then
                        call errors%appendErrors( &
                                errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                        return
                    end if
                class default
                    call errors%appendError(Fatal( &
                            INVALID_ARGUMENT_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "chemical symbol array must all be objects"))
                    return
                end select
            end if
        end do
        symbol = ChemicalSymbol(components)
    end subroutine chemicalSymbolFromJson

    pure subroutine fromStringC(string, errors, symbol)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ChemicalSymbol_t), intent(out) :: symbol

        select case (string)
        case ("Ar", "Ar1")
            symbol = argonGasSymbol()
        case ("He", "He1")
            symbol = heliumGasSymbol()
        case ("H2")
            symbol = hydrogenGasSymbol()
        case ("Kr", "Kr1")
            symbol = kryptonGasSymbol()
        case ("N2")
            symbol = nitrogenGasSymbol()
        case ("H2O", "H2O1", "water")
            symbol = waterSymbol()
        case ("Xe", "Xe1")
            symbol = xenonGasSymbol()
        case default
            call errors%appendError(Fatal( &
                    INVALID_ARGUMENT_TYPE, &
                    Module_(MODULE_NAME), &
                    Procedure_("fromStringC"), &
                    "No chemical symbol available for " // string))
        end select
    end subroutine fromStringC

    pure subroutine fromStringS(string, errors, symbol)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(ChemicalSymbol_t), intent(out) :: symbol

        type(ErrorList_t) :: errors_

        call fromString(char(string), errors_, symbol)
        call errors%appendErrors( &
                errors_, Module_(MODULE_NAME), Procedure_("fromStringS"))
    end subroutine fromStringS

    pure function argonGasSymbol()
        type(ChemicalSymbol_t) :: argonGasSymbol

        allocate(argonGasSymbol%components, source = &
                [ChemicalSymbolComponent(Ar, 1)])
    end function argonGasSymbol

    pure function heliumGasSymbol()
        type(ChemicalSymbol_t) :: heliumGasSymbol

        allocate(heliumGasSymbol%components, source = &
                [ChemicalSymbolComponent(He, 1)])
    end function heliumGasSymbol

    pure function hydrogenGasSymbol()
        type(ChemicalSymbol_t) :: hydrogenGasSymbol

        allocate(hydrogenGasSymbol%components, source = &
                [ChemicalSymbolComponent(H, 2)])
    end function hydrogenGasSymbol

    pure function nitrogenGasSymbol()
        type(ChemicalSymbol_t) :: nitrogenGasSymbol

        allocate(nitrogenGasSymbol%components, source = &
                [ChemicalSymbolComponent(N, 2)])
    end function nitrogenGasSymbol

    pure function kryptonGasSymbol()
        type(ChemicalSymbol_t) :: kryptonGasSymbol

        allocate(kryptonGasSymbol%components, source = &
                [ChemicalSymbolComponent(Kr, 1)])
    end function kryptonGasSymbol

    pure function waterSymbol()
        type(ChemicalSymbol_t) :: waterSymbol

        allocate(waterSymbol%components, source = &
                [ChemicalSymbolComponent(H, 2), &
                 ChemicalSymbolComponent(O, 1)])
    end function waterSymbol

    pure function xenonGasSymbol()
        type(ChemicalSymbol_t) :: xenonGasSymbol

        allocate(xenonGasSymbol%components, source = &
                [ChemicalSymbolComponent(Xe, 1)])
    end function xenonGasSymbol

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

    pure function toJson(self) result(json)
        class(ChemicalSymbol_t), intent(in) :: self
        type(JsonArray_t) :: json

        integer :: i
        type(JsonElement_t) :: json_elements(size(self%components))

        do i = 1, size(self%components)
            json_elements(i) = JsonElement(self%components(i)%toJson())
        end do
        json = JsonArray(json_elements)
    end function toJson

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
