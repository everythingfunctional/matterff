module Element_component_m
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_
    use Isotope_m, only: Isotope_t, fromJson
    use jsonff, only: JsonElement_t, JsonNumber_t, JsonObject_t
    use matterff_Utilities_m, only: INVALID_ARGUMENT_TYPE

    implicit none
    private

    type, public :: ElementComponent_t
        private
        type(Isotope_t), public :: isotope
        double precision, public :: fraction
    end type ElementComponent_t

    interface fromJson
        module procedure elementComponentFromJson
    end interface fromJson

    character(len=*), parameter :: MODULE_NAME = "Element_component_m"

    public :: ElementComponent, fromJson
contains
    elemental function ElementComponent(isotope, fraction)
        type(Isotope_t), intent(in) :: isotope
        double precision, intent(in) :: fraction
        type(ElementComponent_t) :: ElementComponent

        ElementComponent%isotope = isotope
        ElementComponent%fraction = fraction
    end function ElementComponent


    pure subroutine elementComponentFromJson(json, errors, component)
        type(JsonObject_t), intent(in) :: json
        type(ErrorList_t), intent(out) :: errors
        type(ElementComponent_t), intent(out) :: component

        character(len=*), parameter :: PROCEDURE_NAME = "elementComponentFromJson"
        type(ErrorList_t) :: errors_
        type(JsonElement_t) :: fraction_element
        type(Isotope_t) :: isotope

        call fromJson(json, errors_, isotope)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            call json%getElement("fraction", errors_, fraction_element)
            if (errors_%hasAny()) then
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "Isotope requires a fraction"))
            else
                select type (fraction => fraction_element%element)
                type is (JsonNumber_t)
                    component = ElementComponent(isotope, fraction%getValue())
                class default
                    call errors%appendError(Fatal( &
                            INVALID_ARGUMENT_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "fraction must be a number"))
                end select
            end if
        end if
    end subroutine elementComponentFromJson
end module Element_component_m
