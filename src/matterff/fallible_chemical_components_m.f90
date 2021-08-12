module matterff_fallible_chemical_components_m
    use erloff, only: &
            error_list_t, &
            fatal_t, &
            internal_t, &
            message_list_t, &
            module_t, &
            procedure_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: fallible_json_value_t, json_array_t, json_object_t
    use matterff_chemical_component_m, only: chemical_component_t
    use matterff_chemical_symbol_m, only: chemical_symbol_t
    use matterff_fallible_chemical_component_m, only: &
            fallible_chemical_component_t
    use matterff_utilities_m, only: INVALID_ARGUMENT, MISMATCH
    use strff, only: join

    implicit none
    private
    public :: fallible_chemical_components_t

    type :: fallible_chemical_components_t
        private
        type(chemical_component_t), allocatable :: components_(:)
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: components
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface fallible_chemical_components_t
        module procedure check_consistency
        module procedure from_fallible_chemical_components
        module procedure from_json_array
        module procedure from_json_object
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_chemical_components_m"
contains
    function check_consistency(symbol, components) result(fallible_components)
        type(chemical_symbol_t), intent(in) :: symbol
        type(chemical_component_t), intent(in) :: components(:)
        type(fallible_chemical_components_t) :: fallible_components

        character(len=*), parameter :: PROCEDURE_NAME = "check_consistency"

        associate(elements => components%element())
            associate(symbols => elements%symbol())
                if (all(symbol%includes(symbols))) then
                    if (all(components%multiplier() > 0.0d0)) then
                        allocate(fallible_components%components_, source = components)
                    else
                        fallible_components%errors_ = error_list_t(internal_t( &
                                INVALID_ARGUMENT, &
                                module_t(MODULE_NAME), &
                                procedure_t(PROCEDURE_NAME), &
                                "All multipliers must be greater than 0."))
                    end if
                else
                    fallible_components%errors_ = error_list_t(internal_t( &
                            MISMATCH, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "Attempted to create a chemical with an element it doesn't" &
                            // " contain. Chemical: " // symbol%to_string() &
                            // ", Elements: [" // join(symbols%to_string(), ", ") // "]"))
                end if
            end associate
        end associate
    end function

    function from_fallible_chemical_components( &
            fallible_chemical_components, &
            module_, &
            procedure_) &
            result(new_fallible_chemical_components)
        type(fallible_chemical_components_t), intent(in) :: fallible_chemical_components
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_chemical_components_t) :: new_fallible_chemical_components

        new_fallible_chemical_components%messages_ = message_list_t( &
                fallible_chemical_components%messages(), &
                module_, &
                procedure_)
        if (fallible_chemical_components%failed()) then
            new_fallible_chemical_components%errors_ = error_list_t( &
                    fallible_chemical_components%errors(), module_, procedure_)
        else
            allocate(new_fallible_chemical_components%components_, source = &
                    fallible_chemical_components%components_)
        end if
    end function

    function from_json_array(json) result(fallible_chemical_components)
        type(json_array_t), intent(in) :: json
        type(fallible_chemical_components_t) :: fallible_chemical_components

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_array"

        associate(maybe_components => fallible_chemical_component_t(json%get_elements()))
            fallible_chemical_components%messages_ = message_list_t( &
                    maybe_components%messages(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
            if (any(maybe_components%failed())) then
                fallible_chemical_components%errors_ = error_list_t( &
                        maybe_components%errors(), &
                        module_t(MODULE_NAME), &
                        procedure_t("from_json_array"))
            else
                allocate(fallible_chemical_components%components_, source = &
                        maybe_components%chemical_component())
            end if
        end associate
    end function

    function from_json_object(json) result(fallible_chemical_components)
        type(json_object_t), intent(in) :: json
        type(fallible_chemical_components_t) :: fallible_chemical_components

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_object"
        type(fallible_json_value_t) :: maybe_elements_array

        maybe_elements_array = json%get_element("elements")
        if (maybe_elements_array%failed()) then
            fallible_chemical_components%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "chemical must contain list of element compositions"))
        else
            select type (elements_array => maybe_elements_array%value_())
            type is (json_array_t)
                fallible_chemical_components = fallible_chemical_components_t( &
                        fallible_chemical_components_t(elements_array), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            class default
                fallible_chemical_components%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "elements must be an array, but was: " // elements_array%to_compact_string()))
            end select
        end if
    end function

    pure function failed(self)
        class(fallible_chemical_components_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function components(self)
        class(fallible_chemical_components_t), intent(in) :: self
        type(chemical_component_t), allocatable :: components(:)

        components = self%components_
    end function

    function messages(self)
        class(fallible_chemical_components_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    function errors(self)
        class(fallible_chemical_components_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
