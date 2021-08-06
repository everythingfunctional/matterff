module matterff_fallible_chemical_components_m
    use erloff, only: error_list_t, internal_t, module_t, procedure_t
    use iso_varying_string, only: operator(//)
    use matterff_chemical_component_m, only: chemical_component_t
    use matterff_chemical_symbol_m, only: chemical_symbol_t
    use matterff_utilities_m, only: INVALID_ARGUMENT, MISMATCH
    use strff, only: join

    implicit none
    private
    public :: fallible_chemical_components_t

    type :: fallible_chemical_components_t
        private
        type(chemical_component_t), allocatable :: components_(:)
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: components
        procedure, public :: errors
    end type

    interface fallible_chemical_components_t
        module procedure constructor
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_chemical_components_m"
contains
    function constructor(symbol, components) result(fallible_components)
        type(chemical_symbol_t), intent(in) :: symbol
        type(chemical_component_t), intent(in) :: components(:)
        type(fallible_chemical_components_t) :: fallible_components

        character(len=*), parameter :: PROCEDURE_NAME = "constructor"

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

    function errors(self)
        class(fallible_chemical_components_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
