module Chemical_symbol_component_m
    use Element_symbol_m, only: ElementSymbol_t, ElementSymbol
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
    use iso_varying_string, only: VARYING_STRING, operator(//)
    use jsonff, only: &
            JsonElement_t, &
            JsonNumber_t, &
            JsonMember_t, &
            JsonObject_t, &
            JsonString_t, &
            JsonMemberUnsafe, &
            JsonNumber, &
            JsonObject, &
            JsonStringUnsafe
    use strff, only: toString
    use Utilities_m, only: INVALID_ARGUMENT_TYPE

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
        procedure, public :: toJson
        procedure, public :: toString => symbolToString
    end type ChemicalSymbolComponent_t

    interface fromJson
        module procedure chemicalSymbolComponentFromJson
    end interface fromJson

    character(len=*), parameter :: MODULE_NAME = "Chemical_symbol_component_m"

    public :: ChemicalSymbolComponent, fromJson
contains
    elemental function ChemicalSymbolComponent(element, multiple)
        type(ElementSymbol_t), intent(in) :: element
        integer, intent(in) :: multiple
        type(ChemicalSymbolComponent_t) :: ChemicalSymbolComponent

        ChemicalSymbolComponent%element = element
        ChemicalSymbolComponent%multiple = multiple
    end function ChemicalSymbolComponent

    pure subroutine chemicalSymbolComponentFromJson(json, errors, component)
        type(JsonObject_t), intent(in) :: json
        type(ErrorList_t), intent(out) :: errors
        type(ChemicalSymbolComponent_t), intent(out) :: component

        character(len=*), parameter :: PROCEDURE_NAME = &
                "chemicalSymbolComponentFromJson"
        type(JsonElement_t) :: element_element
        type(ElementSymbol_t) :: element_symbol
        type(ErrorList_t) :: errors_
        type(JsonElement_t) :: multiple_element

        call json%getElement("element", errors_, element_element)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            select type (element_string => element_element%element)
            type is (JsonString_t)
                element_symbol = ElementSymbol(element_string%getValue())
                call json%getElement("multiple", errors_, multiple_element)
                if (errors_%hasAny()) then
                    call errors%appendErrors( &
                            errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                else
                    select type (multiple => multiple_element%element)
                    type is (JsonNumber_t)
                        component = ChemicalSymbolComponent( &
                                element_symbol, int(multiple%getValue()))
                    class default
                        call errors%appendError(Fatal( &
                                INVALID_ARGUMENT_TYPE, &
                                Module_(MODULE_NAME), &
                                Procedure_(PROCEDURE_NAME), &
                                "Multiple must be a number"))
                    end select
                end if
            class default
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "Element symbol must be a string"))
            end select
        end if
    end subroutine chemicalSymbolComponentFromJson

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

    pure function toJson(self) result(json)
        class(ChemicalSymbolComponent_t), intent(in) :: self
        type(JsonObject_t) :: json

        type(JsonMember_t) :: members(2)

        members(1) = JsonMemberUnsafe("element", jsonStringUnsafe(self%element%toString()))
        members(2) = JsonMemberUnsafe("multiple", jsonNumber(dble(self%multiple)))
        json = JsonObject(members)
    end function toJson
end module Chemical_symbol_component_m
