module matterff_fallible_chemical_component_m
    use erloff, only: &
            error_list_t, fatal_t, message_list_t, module_t, procedure_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: &
            fallible_json_value_t, &
            json_element_t, &
            json_integer_t, &
            json_number_t, &
            json_object_t, &
            json_value_t
    use matterff_chemical_component_m, only: chemical_component_t
    use matterff_fallible_double_precision_m, only: fallible_double_precision_t
    use matterff_fallible_element_m, only: fallible_element_t
    use matterff_utilities_m, only: INVALID_ARGUMENT

    implicit none
    private
    public :: fallible_chemical_component_t

    type :: fallible_chemical_component_t
        private
        type(chemical_component_t) :: chemical_component_
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: chemical_component
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface fallible_chemical_component_t
        module procedure from_fallible_chemical_component
        module procedure from_fallible_parts
        module procedure from_json_value
        module procedure from_json_element
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_chemical_component_m"
contains
    function from_fallible_chemical_component( &
            fallible_chemical_component, &
            module_, &
            procedure_) &
            result(new_fallible_chemical_component)
        type(fallible_chemical_component_t), intent(in) :: fallible_chemical_component
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_chemical_component_t) :: new_fallible_chemical_component

        new_fallible_chemical_component%messages_ = message_list_t( &
                fallible_chemical_component%messages_, module_, procedure_)
        if (fallible_chemical_component%failed()) then
            new_fallible_chemical_component%errors_ = error_list_t(&
                    fallible_chemical_component%errors_, module_, procedure_)
        else
            new_fallible_chemical_component%chemical_component_ = &
                    fallible_chemical_component%chemical_component_
        end if
    end function

    function from_fallible_parts( &
            maybe_element, &
            maybe_multiplier, &
            module_, &
            procedure_) &
            result(fallible_chemical_component)
        type(fallible_element_t), intent(in) :: maybe_element
        type(fallible_double_precision_t), intent(in) :: maybe_multiplier
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_chemical_component_t) :: fallible_chemical_component

        fallible_chemical_component%messages_ = message_list_t( &
                maybe_element%messages(), module_, procedure_)
        if (any([maybe_element%failed(), maybe_multiplier%failed()])) then
                fallible_chemical_component%errors_ = error_list_t( &
                        [maybe_element%errors(), maybe_multiplier%errors()], &
                        module_, &
                        procedure_)
        else
            fallible_chemical_component%chemical_component_ = chemical_component_t( &
                    maybe_element%element(), maybe_multiplier%value_())
        end if
    end function

    function from_json_object(json) result(fallible_chemical_component)
        type(json_object_t), intent(in) :: json
        type(fallible_chemical_component_t) :: fallible_chemical_component

        fallible_chemical_component = fallible_chemical_component_t( &
                fallible_element_t(json), &
                extract_multiplier(json), &
                module_t(MODULE_NAME), &
                procedure_t("from_json_object"))
    end function

    function from_json_value(json) result(new_fallible_chemical_component)
        class(json_value_t), intent(in) :: json
        type(fallible_chemical_component_t) :: new_fallible_chemical_component

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_value"

        select type (json)
        type is (json_object_t)
            new_fallible_chemical_component = fallible_chemical_component_t( &
                    from_json_object(json), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        class default
            new_fallible_chemical_component%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "chemical component must be an object, but was" // json%to_compact_string()))
        end select
    end function

    impure elemental function from_json_element(json) result(fallible_chemical_component)
        type(json_element_t), intent(in) :: json
        type(fallible_chemical_component_t) :: fallible_chemical_component

        fallible_chemical_component = fallible_chemical_component_t( &
                fallible_chemical_component_t(json%value_()), &
                module_t(MODULE_NAME), &
                procedure_t("from_json_element"))
    end function

    elemental function failed(self)
        class(fallible_chemical_component_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function chemical_component(self)
        class(fallible_chemical_component_t), intent(in) :: self
        type(chemical_component_t) :: chemical_component

        chemical_component = self%chemical_component_
    end function

    impure elemental function messages(self)
        class(fallible_chemical_component_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    impure elemental function errors(self)
        class(fallible_chemical_component_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function extract_multiplier(json) result(maybe_multiplier)
        type(json_object_t), intent(in) :: json
        type(fallible_double_precision_t) :: maybe_multiplier

        character(len=*), parameter :: PROCEDURE_NAME = "extract_multiplier"
        type(fallible_json_value_t) :: maybe_number

        maybe_number = json%get_element("multiplier")
        if (maybe_number%failed()) then
            maybe_multiplier = fallible_double_precision_t(error_list_t( &
                    maybe_number%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME)))
        else
            select type (multiplier => maybe_number%value_())
            type is (json_number_t)
                maybe_multiplier = fallible_double_precision_t(multiplier%get_value())
            type is (json_integer_t)
                maybe_multiplier = fallible_double_precision_t(dble(multiplier%get_value()))
            class default
                maybe_multiplier = fallible_double_precision_t(error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "multiplier must be a number, but was: " //  multiplier%to_compact_string())))
            end select
        end if
    end function
end module
