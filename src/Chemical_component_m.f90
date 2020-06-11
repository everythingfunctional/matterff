module Chemical_component_m
    use Element_m, only: Element_t, fromJson
    use erloff, only: ErrorList_t, MessageList_t, Fatal, Module_, Procedure_
    use jsonff, only: JsonElement_t, JsonNumber_t, JsonObject_t
    use matterff_Utilities_m, only: INVALID_ARGUMENT_TYPE

    implicit none
    private

    type, public :: ChemicalComponent_t
        private
        type(Element_t), public :: element
        double precision, public :: multiplier
    end type ChemicalComponent_t

    interface fromJson
        module procedure chemicalComponentFromJson
    end interface fromJson

    character(len=*), parameter :: MODULE_NAME = "Chemical_component_m"

    public :: ChemicalComponent, fromJson
contains
    elemental function ChemicalComponent(element, multiplier)
        type(Element_t), intent(in) :: element
        double precision, intent(in) :: multiplier
        type(ChemicalComponent_t) :: ChemicalComponent

        ChemicalComponent%element = element
        ChemicalComponent%multiplier = multiplier
    end function ChemicalComponent

    pure subroutine chemicalComponentFromJson(json, messages, errors, component)
        type(JsonObject_t), intent(in) :: json
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(ChemicalComponent_t), intent(out) :: component

        character(len=*), parameter :: PROCEDURE_NAME = "chemicalComponentFromJson"
        type(Element_t) :: element
        type(ErrorList_t) :: errors_
        type(MessageList_t) :: messages_
        type(JsonElement_t) :: multiplier_element

        call fromJson(json, messages_, errors_, element)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            call json%getElement("multiplier", errors_, multiplier_element)
            if (errors_%hasAny()) then
                call errors%appendErrors( &
                        errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
            else
                select type (multiplier => multiplier_element%element)
                type is (JsonNumber_t)
                    component = ChemicalComponent(element, multiplier%getValue())
                class default
                    call errors%appendError(Fatal( &
                            INVALID_ARGUMENT_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "multiplier must be a number"))
                end select
            end if
        end if
    end subroutine chemicalComponentFromJson
end module Chemical_component_m
