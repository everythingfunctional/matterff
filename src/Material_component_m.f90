module Material_component_m
    use Chemical_m, only: Chemical_t, fromJson
    use erloff, only: ErrorList_t, MessageList_t, Fatal, Module_, Procedure_
    use jsonff, only: JsonElement_t, JsonNumber_t, JsonObject_t
    use Utilities_m, only: INVALID_ARGUMENT_TYPE

    implicit none
    private

    type, public :: MaterialComponent_t
        private
        type(Chemical_t), public :: chemical
        double precision, public :: fraction
    end type MaterialComponent_t

    interface fromJson
        module procedure materialComponentFromJson
    end interface fromJson

    character(len=*), parameter :: MODULE_NAME = "Material_component_m"

    public :: MaterialComponent, fromJson
contains
    elemental function MaterialComponent(chemical, fraction)
        type(Chemical_t), intent(in) :: chemical
        double precision, intent(in) :: fraction
        type(MaterialComponent_t) :: MaterialComponent

        MaterialComponent%chemical = chemical
        MaterialComponent%fraction = fraction
    end function MaterialComponent

    pure subroutine materialComponentFromJson(json, messages, errors, component)
        type(JsonObject_t), intent(in) :: json
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(MaterialComponent_t), intent(out) :: component

        character(len=*), parameter :: PROCEDURE_NAME = "materialComponentFromJson"
        type(Chemical_t) :: chemical
        type(ErrorList_t) :: errors_
        type(JsonElement_t) :: fraction_element
        type(MessageList_t) :: messages_

        call fromJson(json, messages_, errors_, chemical)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            call json%getElement("fraction", errors_, fraction_element)
            if (errors_%hasAny()) then
                call errors%appendErrors( &
                        errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
            else
                select type (fraction => fraction_element%element)
                type is (JsonNumber_t)
                    component = MaterialComponent(chemical, fraction%getValue())
                class default
                    call errors%appendError(Fatal( &
                            INVALID_ARGUMENT_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "fraction must be a number"))
                end select
            end if
        end if
    end subroutine materialComponentFromJson
end module Material_component_m
