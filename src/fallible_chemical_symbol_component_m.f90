module fallbile_chemical_symbol_component_m
    use chemical_symbol_component_m, only: chemical_symbol_component_t
    use element_symbol_m, only: element_symbol_t
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: &
            fallible_json_value_t, &
            json_integer_t, &
            json_object_t, &
            json_string_t, &
            json_value_t
    use matterff_utilities_m, only: INVALID_ARGUMENT

    implicit none
    private
    public :: fallible_chemical_symbol_component_t

    type :: fallible_chemical_symbol_component_t
        private
        type(chemical_symbol_component_t) :: chemical_symbol_component_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: chemical_symbol_component
        procedure, public :: errors
    end type

    interface fallible_chemical_symbol_component_t
        module procedure from_fallible_chemical_symbol_component
        module procedure from_json_value
    end interface

    character(len=*), parameter :: MODULE_NAME = "fallbile_chemical_symbol_component_m"
contains
    function from_fallible_chemical_symbol_component( &
            fallible_chemical_symbol_component, &
            module_, &
            procedure_) &
            result(new_fallible_chemical_symbol_component)
        type(fallible_chemical_symbol_component_t), intent(in) :: fallible_chemical_symbol_component
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_chemical_symbol_component_t) :: new_fallible_chemical_symbol_component

        if (fallible_chemical_symbol_component%failed()) then
            new_fallible_chemical_symbol_component%errors_ = error_list_t( &
                    fallible_chemical_symbol_component%errors_, &
                    module_, &
                    procedure_)
        else
            new_fallible_chemical_symbol_component%chemical_symbol_component_ = &
                    fallible_chemical_symbol_component%chemical_symbol_component_
        end if
    end function

    function from_json_object(json) result(fallible_chemical_symbol_component)
        type(json_object_t), intent(in) :: json
        type(fallible_chemical_symbol_component_t) :: fallible_chemical_symbol_component

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_object"
        type(element_symbol_t) :: element_symbol
        type(fallible_json_value_t) :: maybe_element
        type(fallible_json_value_t) :: maybe_multiple

        maybe_element = json%get_element("element")
        if (maybe_element%failed()) then
            fallible_chemical_symbol_component%errors_ = error_list_t( &
                    maybe_element%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            select type (element_string => maybe_element%value_())
            type is (json_string_t)
                element_symbol = element_symbol_t(element_string%get_value())
                maybe_multiple = json%get_element("multiple")
                if (maybe_multiple%failed()) then
                    fallible_chemical_symbol_component%errors_ = error_list_t( &
                            maybe_multiple%errors(), &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME))
                else
                    select type (multiple => maybe_multiple%value_())
                    type is (json_integer_t)
                        fallible_chemical_symbol_component%chemical_symbol_component_ = &
                                chemical_symbol_component_t(element_symbol, multiple%get_value())
                    class default
                        fallible_chemical_symbol_component%errors_ = error_list_t(fatal_t( &
                                INVALID_ARGUMENT, &
                                module_t(MODULE_NAME), &
                                procedure_t(PROCEDURE_NAME), &
                                "Multiple must be a number, but was: " // multiple%to_compact_string()))
                    end select
                end if
            class default
                fallible_chemical_symbol_component%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Element symbol must be a string, but was: " // element_string%to_compact_string()))
            end select
        end if
    end function

    function from_json_value(json) result(fallible_chemical_symbol_component)
        class(json_value_t), intent(in) :: json
        type(fallible_chemical_symbol_component_t) :: fallible_chemical_symbol_component

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_value"

        select type (json)
        type is (json_object_t)
            fallible_chemical_symbol_component = fallible_chemical_symbol_component_t( &
                    from_json_object(json), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        class default
            fallible_chemical_symbol_component%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "chemical symbol component must be an object, but was" &
                    // json%to_compact_string()))
        end select
    end function

    elemental function failed(self)
        class(fallible_chemical_symbol_component_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function chemical_symbol_component(self)
        class(fallible_chemical_symbol_component_t), intent(in) :: self
        type(chemical_symbol_component_t) :: chemical_symbol_component

        chemical_symbol_component = self%chemical_symbol_component_
    end function

    impure elemental function errors(self)
        class(fallible_chemical_symbol_component_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
