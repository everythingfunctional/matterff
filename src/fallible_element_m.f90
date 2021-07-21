module fallible_element_m
    use element_m, only: &
            element_t, &
            from_atom_fractions_unsafe, &
            from_weight_fractions_unsafe, &
            natural_hydrogen, &
            natural_helium, &
            natural_lithium, &
            natural_beryllium, &
            natural_boron, &
            natural_carbon, &
            natural_nitrogen, &
            natural_oxygen, &
            natural_argon, &
            natural_krypton, &
            natural_xenon
    use element_component_m, only: element_component_t
    use element_symbol_m, only: element_symbol_t
    use erloff, only: &
            error_list_t, fatal_t, message_list_t, module_t, procedure_t
    use fallible_element_component_m, only: &
            fallible_element_component_t, from_json_value
    use fallible_element_components_m, only: fallible_element_components_t
    use iso_varying_string, only: varying_string, char
    use jsonff, only: &
            fallible_json_value_t, json_array_t, json_object_t, json_string_t
    use matterff_utilities_m, only: INVALID_ARGUMENT

    implicit none
    private
    public :: &
            fallible_element_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors, &
            from_atom_fractions, &
            from_weight_fractions, &
            get_natural

    type :: fallible_element_t
        private
        type(element_t) :: element_
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: element
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface combine_by_atom_factors
        module procedure combine_elements_by_atom_factors
    end interface

    interface combine_by_weight_factors
        module procedure combine_elements_by_weight_factors
    end interface

    interface fallible_element_t
        module procedure from_fallible_element
        module procedure from_json
    end interface

    interface from_atom_fractions
        module procedure element_from_atom_fractions
    end interface

    interface from_weight_fractions
        module procedure element_from_weight_fractions
    end interface

    interface get_natural
        module procedure get_natural_c
        module procedure get_natural_s
    end interface

    character(len=*), parameter :: MODULE_NAME = "fallible_element_m"
contains
    function combine_elements_by_atom_factors( &
            element1, factor1, element2, factor2) result(combined)
        type(element_t), intent(in) :: element1
        double precision, intent(in) :: factor1
        type(element_t), intent(in) :: element2
        double precision, intent(in) :: factor2
        type(fallible_element_t) :: combined

        associate( &
                components1 => element1%components(), &
                components2 => element2%components())
            combined = fallible_element_t( &
                    from_atom_fractions( &
                            element1%symbol(), &
                            element_component_t( &
                                    [components1%isotope(), components2%isotope()], &
                                    [components1%fraction_()*factor1, components2%fraction_()*factor2])), &
                    module_t(MODULE_NAME), &
                    procedure_t("combine_elements_by_atom_factors"))
        end associate
    end function

    function combine_elements_by_weight_factors( &
            element1, factor1, element2, factor2) result(combined)
        type(element_t), intent(in) :: element1
        double precision, intent(in) :: factor1
        type(element_t), intent(in) :: element2
        double precision, intent(in) :: factor2
        type(fallible_element_t) :: combined

        associate( &
                components1 => element1%components(), &
                components2 => element2%components())
            combined = fallible_element_t( &
                    from_weight_fractions( &
                            element1%symbol(), &
                            element_component_t( &
                                    [components1%isotope(), components2%isotope()], &
                                    [ element1%weight_fraction(components1%isotope())*factor1 &
                                    , element2%weight_fraction(components2%isotope())*factor2 &
                                    ])), &
                    module_t(MODULE_NAME), &
                    procedure_t("combine_elements_by_weight_factors"))
        end associate
    end function

    function element_from_atom_fractions( &
            symbol, components) result(new_fallible_element)
        type(element_symbol_t), intent(in) :: symbol
        type(element_component_t), intent(in) :: components(:)
        type(fallible_element_t) :: new_fallible_element

        character(len=*), parameter :: PROCEDURE_NAME = "element_from_atom_fractions"
        type(fallible_element_components_t) :: fixed_components

        fixed_components = fallible_element_components_t(symbol, components)
        new_fallible_element%messages_ = message_list_t( &
                fixed_components%messages(), &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME))
        if (fixed_components%failed()) then
            new_fallible_element%errors_ = error_list_t( &
                    fixed_components%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            new_fallible_element%element_ = from_atom_fractions_unsafe( &
                    symbol, fixed_components%components())
        end if
    end function

    function element_from_weight_fractions( &
            symbol, components) result(new_fallible_element)
        type(element_symbol_t), intent(in) :: symbol
        type(element_component_t), intent(in) :: components(:)
        type(fallible_element_t) :: new_fallible_element

        character(len=*), parameter :: PROCEDURE_NAME = "element_from_weight_fractions"
        type(fallible_element_components_t) :: fixed_components

        fixed_components = fallible_element_components_t(symbol, components)
        new_fallible_element%messages_ = message_list_t( &
                fixed_components%messages(), &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME))
        if (fixed_components%failed()) then
            new_fallible_element%errors_ = error_list_t( &
                    fixed_components%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            new_fallible_element%element_ = from_weight_fractions_unsafe( &
                    symbol, fixed_components%components())
        end if
    end function

    function from_fallible_element( &
            fallible_element, module_, procedure_) result(new_fallible_element)
        type(fallible_element_t), intent(in) :: fallible_element
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_element_t) :: new_fallible_element

        new_fallible_element%messages_ = message_list_t( &
                fallible_element%messages_, module_, procedure_)
        if (fallible_element%failed()) then
            new_fallible_element%errors_ = error_list_t( &
                    fallible_element%errors_, module_, procedure_)
        else
            new_fallible_element%element_ = fallible_element%element_
        end if
    end function

    function from_json(json) result(fallible_element)
        type(json_object_t), intent(in) :: json
        type(fallible_element_t) :: fallible_element

        character(len=*), parameter :: PROCEDURE_NAME = "from_json"
        type(element_symbol_t) :: element_symbol
        type(fallible_json_value_t), allocatable :: failed_isotopes(:)
        integer :: i
        type(fallible_json_value_t) :: maybe_atom_fractions
        type(fallible_element_component_t), allocatable :: maybe_components(:)
        type(fallible_json_value_t) :: maybe_element
        type(fallible_json_value_t), allocatable :: maybe_isotopes(:)
        type(fallible_json_value_t) :: maybe_natural
        type(fallible_json_value_t) :: maybe_weight_fractions
        integer :: num_isotopes

        maybe_element = json%get_element("element")
        if (maybe_element%failed()) then
            maybe_natural = json%get_element("natural")
            if (maybe_natural%failed()) then
                fallible_element%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "element composition must either have element and" &
                        // " atom fractions or weight fractions, or be natural"))
            else
                select type (element_string => maybe_natural%value_())
                type is (json_string_t)
                    fallible_element = fallible_element_t( &
                            get_natural(element_string%get_value()), &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME))
                class default
                    fallible_element%errors_ = error_list_t(fatal_t( &
                            INVALID_ARGUMENT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "natural must be a string"))
                end select
            end if
        else
            select type (element_string => maybe_element%value_())
            type is (json_string_t)
                element_symbol = element_symbol_t(element_string%get_value())
                maybe_atom_fractions = json%get_element("atom fractions")
                if (maybe_atom_fractions%failed()) then
                    maybe_weight_fractions = json%get_element("weight fractions")
                    if (maybe_weight_fractions%failed()) then
                        fallible_element%errors_ = error_list_t(fatal_t( &
                                INVALID_ARGUMENT, &
                                module_t(MODULE_NAME), &
                                procedure_t(PROCEDURE_NAME), &
                                "element composition must either have" &
                                // " atom fractions or weight fractions"))
                    else
                        select type (fractions_array => maybe_weight_fractions%value_())
                        type is (json_array_t)
                            num_isotopes = fractions_array%length()
                            maybe_isotopes = [(fractions_array%get_element(i), i = 1, num_isotopes)]
                            if (any([(maybe_isotopes(i)%failed(), i = 1, num_isotopes)])) then
                                failed_isotopes = pack(maybe_isotopes, [(maybe_isotopes(i)%failed(), i = 1, num_isotopes)])
                                fallible_element%errors_ = error_list_t( &
                                        [(failed_isotopes(i)%errors(), i = 1, size(failed_isotopes))], &
                                        module_t(MODULE_NAME),&
                                        procedure_t(PROCEDURE_NAME))
                            else
                                allocate(maybe_components, source = &
                                        [(from_json_value(maybe_isotopes(i)%value_()), i = 1, num_isotopes)])
                                if (any(maybe_components%failed())) then
                                    fallible_element%errors_ = error_list_t( &
                                            pack(maybe_components%errors(), maybe_components%failed()), &
                                            module_t(MODULE_NAME), &
                                            procedure_t(PROCEDURE_NAME))
                                else
                                    fallible_element = fallible_element_t( &
                                            from_weight_fractions( &
                                                    element_symbol, &
                                                    maybe_components%element_component()), &
                                            module_t(MODULE_NAME), &
                                            procedure_t(PROCEDURE_NAME))
                                end if
                            end if
                        class default
                            fallible_element%errors_ = error_list_t(fatal_t( &
                                    INVALID_ARGUMENT, &
                                    module_t(MODULE_NAME), &
                                    procedure_t(PROCEDURE_NAME), &
                                    "atom fractions must be an array"))
                        end select
                    end if
                else
                    select type (fractions_array => maybe_atom_fractions%value_())
                    type is (json_array_t)
                        num_isotopes = fractions_array%length()
                        maybe_isotopes = [(fractions_array%get_element(i), i = 1, num_isotopes)]
                        if (any([(maybe_isotopes(i)%failed(), i = 1, num_isotopes)])) then
                            failed_isotopes = pack(maybe_isotopes, [(maybe_isotopes(i)%failed(), i = 1, num_isotopes)])
                            fallible_element%errors_ = error_list_t( &
                                    [(failed_isotopes(i)%errors(), i = 1, size(failed_isotopes))], &
                                    module_t(MODULE_NAME),&
                                    procedure_t(PROCEDURE_NAME))
                        else
                            allocate(maybe_components, source = &
                                    [(from_json_value(maybe_isotopes(i)%value_()), i = 1, num_isotopes)])
                            if (any(maybe_components%failed())) then
                                fallible_element%errors_ = error_list_t( &
                                        pack(maybe_components%errors(), maybe_components%failed()), &
                                        module_t(MODULE_NAME), &
                                        procedure_t(PROCEDURE_NAME))
                            else
                                fallible_element = fallible_element_t( &
                                        from_atom_fractions( &
                                                element_symbol, &
                                                maybe_components%element_component()), &
                                        module_t(MODULE_NAME), &
                                        procedure_t(PROCEDURE_NAME))
                            end if
                        end if
                    class default
                        fallible_element%errors_ = error_list_t(fatal_t( &
                                INVALID_ARGUMENT, &
                                module_t(MODULE_NAME), &
                                procedure_t(PROCEDURE_NAME), &
                                "atom fractions must be an array"))
                    end select
                end if
            class default
                fallible_element%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "element identifier must be a string"))
            end select
        end if
    end function

    function get_natural_c(symbol) result(fallible_element)
        character(len=*), intent(in) :: symbol
        type(fallible_element_t) :: fallible_element

        select case (symbol)
        case ("H")
            fallible_element%element_ = natural_hydrogen()
        case ("He")
            fallible_element%element_ = natural_helium()
        case ("Li")
            fallible_element%element_ = natural_lithium()
        case ("Be")
            fallible_element%element_ = natural_beryllium()
        case ("B")
            fallible_element%element_ = natural_boron()
        case ("C")
            fallible_element%element_ = natural_carbon()
        case ("N")
            fallible_element%element_ = natural_nitrogen()
        case ("O")
            fallible_element%element_ = natural_oxygen()
        case ("Ar")
            fallible_element%element_ = natural_argon()
        case ("Kr")
            fallible_element%element_ = natural_krypton()
        case ("Xe")
            fallible_element%element_ = natural_xenon()
        case default
            fallible_element%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t("get_natural_c"), &
                    "No natural composition available for " // symbol))
        end select
    end function

    function get_natural_s(symbol) result(fallible_element)
        type(varying_string), intent(in) :: symbol
        type(fallible_element_t) :: fallible_element

        fallible_element = fallible_element_t( &
                get_natural(char(symbol)), &
                module_t(MODULE_NAME), &
                procedure_t("get_natural_s"))
    end function

    pure function failed(self)
        class(fallible_element_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function element(self)
        class(fallible_element_t), intent(in) :: self
        type(element_t) :: element

        element = self%element_
    end function

    function messages(self)
        class(fallible_element_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    function errors(self)
        class(fallible_element_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
