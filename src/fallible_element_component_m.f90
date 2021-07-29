module fallible_element_component_m
    use element_component_m, only: element_component_t
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use fallible_isotope_m, only: fallible_isotope_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: &
            fallible_json_value_t, json_number_t, json_object_t, json_value_t
    use matterff_utilities_m, only: INVALID_ARGUMENT

    implicit none
    private
    public :: fallible_element_component_t

    type :: fallible_element_component_t
        private
        type(element_component_t) :: element_component_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: element_component
        procedure, public :: errors
    end type

    interface fallible_element_component_t
        module procedure from_fallible_element_component
        module procedure from_json_value
    end interface

    character(len=*), parameter :: MODULE_NAME = "fallible_element_component_m"
contains
    function from_fallible_element_component( &
            fallible_element_component, &
            module_, &
            procedure_) &
            result(new_fallible_element_component)
        type(fallible_element_component_t), intent(in) :: fallible_element_component
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_element_component_t) :: new_fallible_element_component

        if (fallible_element_component%failed()) then
            new_fallible_element_component%errors_ = error_list_t(&
                    fallible_element_component%errors_, module_, procedure_)
        else
            new_fallible_element_component%element_component_ = fallible_element_component%element_component_
        end if
    end function

    function from_json_object(json) result(new_fallible_element_component)
        type(json_object_t), intent(in) :: json
        type(fallible_element_component_t) :: new_fallible_element_component

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_object"
        type(fallible_json_value_t) :: maybe_fraction
        type(fallible_isotope_t) :: maybe_isotope

        maybe_isotope = fallible_isotope_t(json)
        if (maybe_isotope%failed()) then
            new_fallible_element_component%errors_ = error_list_t( &
                    maybe_isotope%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            maybe_fraction = json%get_element("fraction")
            if (maybe_fraction%failed()) then
                new_fallible_element_component%errors_ = error_list_t( &
                        maybe_fraction%errors(), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            else
                select type (fraction => maybe_fraction%value_())
                type is (json_number_t)
                    new_fallible_element_component%element_component_= &
                            element_component_t(maybe_isotope%isotope(), fraction%get_value())
                class default
                    new_fallible_element_component%errors_ = error_list_t(fatal_t( &
                            INVALID_ARGUMENT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "fraction must be a number"))
                end select
            end if
        end if
    end function

    function from_json_value(json) result(new_fallible_element_component)
        class(json_value_t), intent(in) :: json
        type(fallible_element_component_t) :: new_fallible_element_component

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_value"

        select type (json)
        type is (json_object_t)
            new_fallible_element_component = fallible_element_component_t( &
                    from_json_object(json), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        class default
            new_fallible_element_component%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "element component must be an object, but was" // json%to_compact_string()))
        end select
    end function

    elemental function failed(self)
        class(fallible_element_component_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function element_component(self)
        class(fallible_element_component_t), intent(in) :: self
        type(element_component_t) :: element_component

        element_component = self%element_component_
    end function

    impure elemental function errors(self)
        class(fallible_element_component_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
