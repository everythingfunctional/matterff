module matterff_fallible_chemical_symbol_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t
    use iso_varying_string, only: varying_string, operator(//), char
    use jsonff, only: &
            fallible_json_value_t, json_array_t, json_string_t, json_value_t
    use matterff_chemical_symbol_m, only: &
            chemical_symbol_t, &
            argon_gas_symbol, &
            helium_gas_symbol, &
            hydrogen_gas_symbol, &
            krypton_gas_symbol, &
            nitrogen_gas_symbol, &
            oxygen_gas_symbol, &
            water_symbol, &
            xenon_gas_symbol
    use matterff_fallbile_chemical_symbol_component_m, only: &
            fallible_chemical_symbol_component_t
    use matterff_utilities_m, only: INVALID_ARGUMENT

    implicit none
    private
    public :: fallible_chemical_symbol_t

    type :: fallible_chemical_symbol_t
        private
        type(chemical_symbol_t) :: chemical_symbol_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: chemical_symbol
        procedure, public :: errors
    end type

    interface fallible_chemical_symbol_t
        module procedure from_character
        module procedure from_fallible_chemical_symbol
        module procedure from_json_value
        module procedure from_string
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_chemical_symbol_m"
contains
    function from_json_array(json) result(fallible_chemical_symbol)
        type(json_array_t), intent(in) :: json
        type(fallible_chemical_symbol_t) :: fallible_chemical_symbol

        character(len=*), parameter :: PROCEDURE_NAME = "from_json"
        type(fallible_json_value_t), allocatable :: failed_objects(:)
        integer :: i
        type(fallible_chemical_symbol_component_t), allocatable :: maybe_components(:)
        type(fallible_json_value_t), allocatable :: maybe_objects(:)
        integer :: num_components

        num_components = json%length()
        allocate(maybe_objects, source = [(json%get_element(i), i = 1, num_components)])
        if (any([(maybe_objects(i)%failed(), i = 1, num_components)])) then
            failed_objects = pack(maybe_objects, [(maybe_objects(i)%failed(), i = 1, num_components)])
            fallible_chemical_symbol%errors_ = error_list_t( &
                    [(failed_objects(i)%errors(), i = 1, size(failed_objects))], &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            allocate(maybe_components, source = &
                    [(fallible_chemical_symbol_component_t(maybe_objects(i)%value_()), i = 1, num_components)])
            if (any(maybe_components%failed())) then
                fallible_chemical_symbol%errors_ = error_list_t( &
                        pack(maybe_components%errors(), maybe_components%failed()), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            else
                fallible_chemical_symbol%chemical_symbol_ = chemical_symbol_t( &
                        maybe_components%chemical_symbol_component())
            end if
        end if
    end function

    function from_json_value(json) result(fallible_chemical_symbol)
        class(json_value_t), intent(in) :: json
        type(fallible_chemical_symbol_t) :: fallible_chemical_symbol

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_value"

        select type (json)
        type is (json_array_t)
            fallible_chemical_symbol = fallible_chemical_symbol_t( &
                    from_json_array(json), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        type is (json_string_t)
            fallible_chemical_symbol = fallible_chemical_symbol_t( &
                    fallible_chemical_symbol_t(json%get_value()), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        class default
            fallible_chemical_symbol%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "chemical symbol must be an array or string, but was" // json%to_compact_string()))
        end select
    end function

    function from_fallible_chemical_symbol( &
            fallible_chemical_symbol, &
            module_, &
            procedure_) &
            result(new_fallible_chemical_symbol)
        type(fallible_chemical_symbol_t), intent(in) :: fallible_chemical_symbol
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_chemical_symbol_t) :: new_fallible_chemical_symbol

        if (fallible_chemical_symbol%failed()) then
            new_fallible_chemical_symbol%errors_ = error_list_t( &
                    fallible_chemical_symbol%errors_, &
                    module_, &
                    procedure_)
        else
            new_fallible_chemical_symbol%chemical_symbol_ = &
                    fallible_chemical_symbol%chemical_symbol_
        end if
    end function

    function from_character(string) result(fallible_chemical_symbol)
        character(len=*), intent(in) :: string
        type(fallible_chemical_symbol_t) :: fallible_chemical_symbol

        select case (string)
        case ("Ar", "Ar1")
            fallible_chemical_symbol%chemical_symbol_ = argon_gas_symbol()
        case ("He", "He1")
            fallible_chemical_symbol%chemical_symbol_ = helium_gas_symbol()
        case ("H2")
            fallible_chemical_symbol%chemical_symbol_ = hydrogen_gas_symbol()
        case ("Kr", "Kr1")
            fallible_chemical_symbol%chemical_symbol_ = krypton_gas_symbol()
        case ("N2")
            fallible_chemical_symbol%chemical_symbol_ = nitrogen_gas_symbol()
        case ("O2")
            fallible_chemical_symbol%chemical_symbol_ = oxygen_gas_symbol()
        case ("H2O", "H2O1", "water")
            fallible_chemical_symbol%chemical_symbol_ = water_symbol()
        case ("Xe", "Xe1")
            fallible_chemical_symbol%chemical_symbol_ = xenon_gas_symbol()
        case default
            fallible_chemical_symbol%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t("from_character"), &
                    "No chemical symbol available for " // string))
        end select
    end function

    function from_string(string) result(fallible_chemical_symbol)
        type(varying_string), intent(in) :: string
        type(fallible_chemical_symbol_t) :: fallible_chemical_symbol

        fallible_chemical_symbol = fallible_chemical_symbol_t( &
                fallible_chemical_symbol_t(char(string)), &
                module_t(MODULE_NAME), &
                procedure_t("from_string"))
    end function

    pure function failed(self)
        class(fallible_chemical_symbol_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function chemical_symbol(self)
        class(fallible_chemical_symbol_t), intent(in) :: self
        type(chemical_symbol_t) :: chemical_symbol

        chemical_symbol = self%chemical_symbol_
    end function

    function errors(self)
        class(fallible_chemical_symbol_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
