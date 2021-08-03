module matterff_fallible_chemical_m
    use erloff, only: &
            error_list_t, &
            internal_t, &
            fatal_t, &
            message_list_t, &
            module_t, &
            procedure_t
    use iso_varying_string, only: varying_string, char, operator(//)
    use jsonff, only: &
            fallible_json_value_t, json_array_t, json_object_t, json_string_t
    use matterff_chemical_m, only: &
            chemical_t, &
            chemical_unsafe, &
            combine_by_atom_factors_unsafe, &
            combine_by_weight_factors_unsafe, &
            natural_argon_gas, &
            natural_helium_gas, &
            natural_hydrogen_gas, &
            natural_krypton_gas, &
            natural_nitrogen_gas, &
            natural_oxygen_gas, &
            natural_water, &
            natural_xenon_gas
    use matterff_chemical_component_m, only: chemical_component_t
    use matterff_chemical_symbol_m, only: chemical_symbol_t
    use matterff_fallible_chemical_component_m, only: &
            fallible_chemical_component_t, from_json_value
    use matterff_fallible_chemical_components_m, only: &
            fallible_chemical_components_t
    use matterff_fallible_chemical_symbol_m, only: fallible_chemical_symbol_t
    use matterff_utilities_m, only: INVALID_ARGUMENT, MISMATCH

    implicit none
    private
    public :: &
            fallible_chemical_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors, &
            get_natural

    type :: fallible_chemical_t
        private
        type(chemical_t) :: chemical_
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: chemical
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface combine_by_atom_factors
        module procedure combine_chemicals_by_atom_factors
    end interface

    interface combine_by_weight_factors
        module procedure combine_chemicals_by_weight_factors
    end interface

    interface fallible_chemical_t
        module procedure from_components
        module procedure from_fallible_chemical
        module procedure from_json
    end interface

    interface get_natural
        module procedure get_natural_c
        module procedure get_natural_s
    end interface

    character(len=*), parameter :: MODULE_NAME = "fallible_chemical_m"
contains
    function combine_chemicals_by_atom_factors( &
            chemical1, factor1, chemical2, factor2) result(combined)
        type(chemical_t), intent(in) :: chemical1
        double precision, intent(in) :: factor1
        type(chemical_t), intent(in) :: chemical2
        double precision, intent(in) :: factor2
        type(fallible_chemical_t) :: combined

        type(chemical_symbol_t) :: symbol1, symbol2

        symbol1 = chemical1%symbol()
        symbol2 = chemical2%symbol()
        if (symbol1 == symbol2) then
            associate(factor_total => factor1 + factor2)
                combined%chemical_ = combine_by_atom_factors_unsafe( &
                        chemical1, &
                        factor1 / factor_total, &
                        chemical2, &
                        factor2 / factor_total)
            end associate
        else
            combined%errors_ = error_list_t(internal_t( &
                    MISMATCH, &
                    module_t(MODULE_NAME), &
                    procedure_t("combine_chemicals_by_atom_factors"), &
                    "Attempted to combine different chemicals: " &
                    // symbol1%to_string() // " and " // symbol2%to_string()))
        end if
    end function

    function combine_chemicals_by_weight_factors( &
            chemical1, factor1, chemical2, factor2) result(combined)
        type(chemical_t), intent(in) :: chemical1
        double precision, intent(in) :: factor1
        type(chemical_t), intent(in) :: chemical2
        double precision, intent(in) :: factor2
        type(fallible_chemical_t) :: combined

        type(chemical_symbol_t) :: symbol1, symbol2

        symbol1 = chemical1%symbol()
        symbol2 = chemical2%symbol()
        if (symbol1 == symbol2) then
            combined%chemical_ = combine_by_weight_factors_unsafe( &
                    chemical1, factor1, chemical2, factor2)
        else
            combined%errors_ = error_list_t(internal_t( &
                    MISMATCH, &
                    module_t(MODULE_NAME), &
                    procedure_t("combine_chemicals_by_weight_factors"), &
                    "Attempted to combine different chemicals: " &
                    // symbol1%to_string() // " and " // symbol2%to_string()))
        end if
    end function

    function from_components(symbol, components) result(new_fallible_chemical)
        type(chemical_symbol_t), intent(in) :: symbol
        type(chemical_component_t), intent(in) :: components(:)
        type(fallible_chemical_t) :: new_fallible_chemical

        type(fallible_chemical_components_t) :: checked_components

        checked_components = fallible_chemical_components_t(symbol, components)
        if (checked_components%failed()) then
            new_fallible_chemical%errors_ = error_list_t( &
                    checked_components%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t("from_components"))
        else
            new_fallible_chemical%chemical_ = chemical_unsafe( &
                    symbol, checked_components%components())
        end if
    end function

    function from_fallible_chemical( &
            fallible_chemical, module_, procedure_) result(new_fallible_chemical)
        type(fallible_chemical_t), intent(in) :: fallible_chemical
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_chemical_t) :: new_fallible_chemical

        new_fallible_chemical%messages_ = message_list_t( &
                fallible_chemical%messages_, module_, procedure_)
        if (fallible_chemical%failed()) then
            new_fallible_chemical%errors_ = error_list_t( &
                    fallible_chemical%errors_, module_, procedure_)
        else
            new_fallible_chemical%chemical_ = fallible_chemical%chemical_
        end if
    end function

    function from_json(json) result(fallible_chemical)
        type(json_object_t), intent(in) :: json
        type(fallible_chemical_t) :: fallible_chemical

        character(len=*), parameter :: PROCEDURE_NAME = "from_json"
        type(fallible_json_value_t), allocatable :: failed_elements(:)
        integer :: i
        type(fallible_json_value_t) :: maybe_chemical
        type(fallible_chemical_component_t), allocatable :: maybe_components(:)
        type(fallible_json_value_t), allocatable :: maybe_elements(:)
        type(fallible_json_value_t) :: maybe_elements_array
        type(fallible_json_value_t) :: maybe_natural
        type(fallible_chemical_symbol_t) :: maybe_symbol
        integer :: num_elements

        maybe_chemical = json%get_element("chemical")
        if (maybe_chemical%failed()) then
            maybe_natural = json%get_element("natural")
            if (maybe_natural%failed()) then
                fallible_chemical%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "chemical must have chemical and elements or natural"))
            else
                select type (chemical_string => maybe_natural%value_())
                type is (json_string_t)
                    fallible_chemical = fallible_chemical_t( &
                            get_natural(chemical_string%get_value()), &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME))
                class default
                    fallible_chemical%errors_ = error_list_t(fatal_t( &
                            INVALID_ARGUMENT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "natural must be a string"))
                end select
            end if
        else
            maybe_symbol = fallible_chemical_symbol_t(maybe_chemical%value_())
            if (maybe_symbol%failed()) then
                fallible_chemical%errors_ = error_list_t( &
                        maybe_symbol%errors(), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            else
                maybe_elements_array = json%get_element("elements")
                if (maybe_elements_array%failed()) then
                    fallible_chemical%errors_ = error_list_t(fatal_t( &
                            INVALID_ARGUMENT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "chemical must contain list of element compositions"))
                else
                    select type (elements_array => maybe_elements_array%value_())
                    type is (json_array_t)
                        num_elements = elements_array%length()
                        maybe_elements = [(elements_array%get_element(i), i = 1, num_elements)]
                        if (any([(maybe_elements(i)%failed(), i = 1, num_elements)])) then
                            failed_elements = pack(maybe_elements, [(maybe_elements(i)%failed(), i = 1, num_elements)])
                            fallible_chemical%errors_ = error_list_t( &
                                    [(failed_elements(i)%errors(), i = 1, size(failed_elements))], &
                                    module_t(MODULE_NAME), &
                                    procedure_t(PROCEDURE_NAME))
                        else
                            allocate(maybe_components, source = &
                                    [(fallible_chemical_component_t(maybe_elements(i)%value_()), i = 1, num_elements)])
                            if (any(maybe_components%failed())) then
                                fallible_chemical%errors_ = error_list_t( &
                                        pack(maybe_components%errors(), maybe_components%failed()), &
                                        module_t(MODULE_NAME), &
                                        procedure_t(PROCEDURE_NAME))
                                fallible_chemical%messages_ = message_list_t( &
                                        maybe_components%messages(), &
                                        module_t(MODULE_NAME), &
                                        procedure_t(PROCEDURE_NAME))
                            else
                                fallible_chemical = fallible_chemical_t( &
                                        fallible_chemical_t( &
                                                maybe_symbol%chemical_symbol(), &
                                                maybe_components%chemical_component()), &
                                        module_t(MODULE_NAME), &
                                        procedure_t(PROCEDURE_NAME))
                                fallible_chemical%messages_ = message_list_t( &
                                        maybe_components%messages(), &
                                        module_t(MODULE_NAME), &
                                        procedure_t(PROCEDURE_NAME))
                            end if
                        end if
                    class default
                        fallible_chemical%errors_ = error_list_t(fatal_t( &
                                INVALID_ARGUMENT, &
                                module_t(MODULE_NAME), &
                                procedure_t(PROCEDURE_NAME), &
                                "elements must be an array"))
                    end select
                end if
            end if
        end if
    end function

    function get_natural_c(symbol) result(fallible_chemical)
        character(len=*), intent(in) :: symbol
        type(fallible_chemical_t) :: fallible_chemical

        select case (symbol)
        case ("Ar", "Ar1")
            fallible_chemical%chemical_ = natural_argon_gas()
        case ("He", "He1")
            fallible_chemical%chemical_ = natural_helium_gas()
        case ("H2")
            fallible_chemical%chemical_ = natural_hydrogen_gas()
        case ("Kr", "Kr1")
            fallible_chemical%chemical_ = natural_krypton_gas()
        case ("N2")
            fallible_chemical%chemical_ = natural_nitrogen_gas()
        case ("O2")
            fallible_chemical%chemical_ = natural_oxygen_gas()
        case ("H2O", "H2O1", "water")
            fallible_chemical%chemical_ = natural_water()
        case ("Xe", "Xe1")
            fallible_chemical%chemical_ = natural_xenon_gas()
        case default
            fallible_chemical%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t("get_natural_c"), &
                    "No natural composition available for " // symbol))
        end select
    end function

    function get_natural_s(symbol) result(fallible_chemical)
        type(varying_string), intent(in) :: symbol
        type(fallible_chemical_t) :: fallible_chemical

        fallible_chemical = fallible_chemical_t( &
                get_natural(char(symbol)), &
                module_t(MODULE_NAME), &
                procedure_t("get_natural_s"))
    end function

    pure function failed(self)
        class(fallible_chemical_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function chemical(self)
        class(fallible_chemical_t), intent(in) :: self
        type(chemical_t) :: chemical

        chemical = self%chemical_
    end function

    function messages(self)
        class(fallible_chemical_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    function errors(self)
        class(fallible_chemical_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
