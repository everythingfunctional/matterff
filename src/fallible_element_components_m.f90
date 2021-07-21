module fallible_element_components_m
    use element_component_m, only: element_component_t
    use element_symbol_m, only: element_symbol_t
    use erloff, only: &
            error_list_t, &
            info_t, &
            internal_t, &
            message_list_t, &
            module_t, &
            procedure_t
    use iso_varying_string, only: operator(//)
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
        module procedure constructor
    end interface

    character(len=*), parameter :: MODULE_NAME = "fallible_element_components_m"
contains
    function constructor(symbol, components) result(fallible_components)
        type(element_symbol_t), intent(in) :: symbol
        type(element_component_t), intent(in) :: components(:)
        type(fallible_element_components_t) :: fallible_components

        character(len=*), parameter :: PROCEDURE_NAME = "constructor"

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
