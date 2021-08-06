module matterff_fallible_element_components_m
    use erloff, only: &
            error_list_t, &
            info_t, &
            internal_t, &
            message_list_t, &
            module_t, &
            procedure_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: json_array_t
    use matterff_element_component_m, only: element_component_t
    use matterff_element_symbol_m, only: element_symbol_t
    use matterff_fallible_element_component_m, only: fallible_element_component_t
    use matterff_utilities_m, only: &
            operator(.sumsTo.), INVALID_ARGUMENT, MISMATCH, NORMALIZED_FRACTIONS
    use strff, only: join

    implicit none
    private
    public :: fallible_element_components_t

    type :: fallible_element_components_t
        private
        type(element_component_t), allocatable :: components_(:)
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: components
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface fallible_element_components_t
        module procedure check_consistency
        module procedure from_json
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_element_components_m"
contains
    function check_consistency(symbol, components) result(fallible_components)
        type(element_symbol_t), intent(in) :: symbol
        type(element_component_t), intent(in) :: components(:)
        type(fallible_element_components_t) :: fallible_components

        character(len=*), parameter :: PROCEDURE_NAME = "check_consistency"

        associate( &
                isotopes => components%isotope(), &
                fractions => components%fraction_())
            if (all(isotopes%is(symbol)))then
                if (all(fractions > 0.0d0)) then
                    if (fractions.sumsTo.1.0d0) then
                        allocate(fallible_components%components_, source = components)
                    else
                        fallible_components%messages_ = message_list_t(info_t( &
                                NORMALIZED_FRACTIONS, &
                                module_t(MODULE_NAME), &
                                procedure_t(PROCEDURE_NAME), &
                                "Attempted to create composition with component" &
                                // " fractions that did not sum to 1.0."))
                        allocate(fallible_components%components_, source = element_component_t( &
                                isotopes, fractions / sum(fractions)))
                    end if
                else
                    fallible_components%errors_ = error_list_t(internal_t( &
                            INVALID_ARGUMENT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "All fractions must be greater than 0."))
                end if
            else
                fallible_components%errors_ = error_list_t(internal_t( &
                        MISMATCH, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Attempted to create an element with an isotope not of that element." &
                        // " Element: " // symbol%to_string() // ", Isotopes: [" &
                        // join(isotopes%to_string(), ", ") // "]"))
            end if
        end associate
    end function

    function from_json(json) result(fallible_element_components)
        type(json_array_t), intent(in) :: json
        type(fallible_element_components_t) :: fallible_element_components

        associate(maybe_components => fallible_element_component_t(json%get_elements()))
            associate(failures => maybe_components%failed())
                if (any(failures)) then
                    fallible_element_components%errors_ = error_list_t( &
                            pack(maybe_components%errors(), failures), &
                            module_t(MODULE_NAME), &
                            procedure_t("from_json"))
                else
                    fallible_element_components%components_ = maybe_components%element_component()
                end if
            end associate
        end associate
    end function

    pure function failed(self)
        class(fallible_element_components_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function components(self)
        class(fallible_element_components_t), intent(in) :: self
        type(element_component_t), allocatable :: components(:)

        components = self%components_
    end function

    function messages(self)
        class(fallible_element_components_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    function errors(self)
        class(fallible_element_components_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
