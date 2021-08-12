module matterff_fallible_material_m
    use erloff, only: &
            error_list_t, fatal_t, message_list_t, module_t, procedure_t
    use iso_varying_string, only: varying_string, operator(//), char
    use jsonff, only: &
            fallible_json_value_t, json_array_t, json_object_t, json_string_t
    use matterff_fallible_material_component_m, only: &
            fallible_material_component_t
    use matterff_fallible_material_components_m, only: &
            fallible_material_components_t
    use matterff_material_m, only: &
            material_t, &
            from_atom_fractions_unsafe, &
            from_weight_fractions_unsafe
    use matterff_material_component_m, only: material_component_t
    use matterff_utilities_m, only: INVALID_ARGUMENT

    implicit none
    private
    public :: &
            fallible_material_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors, &
            from_atom_fractions, &
            from_weight_fractions

    type :: fallible_material_t
        private
        type(material_t) :: material_
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: material
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface combine_by_atom_factors
        module procedure combine_materials_by_atom_factors
    end interface

    interface combine_by_weight_factors
        module procedure combine_materials_by_weight_factors
    end interface

    interface fallible_material_t
        module procedure from_errors
        module procedure from_fallible_material
        module procedure from_json
    end interface

    interface from_atom_fractions
        module procedure material_from_atom_fractions
        module procedure material_from_fallible_atom_fractions
    end interface

    interface from_weight_fractions
        module procedure material_from_weight_fractions
        module procedure material_from_fallible_weight_fractions
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_material_m"
contains
    function combine_materials_by_atom_factors( &
            material1, factor1, material2, factor2) result(combined)
        type(material_t), intent(in) :: material1
        double precision, intent(in) :: factor1
        type(material_t), intent(in) :: material2
        double precision, intent(in) :: factor2
        type(fallible_material_t) :: combined

        associate( &
                components1 => material1%components(), &
                components2 => material2%components())
            combined = fallible_material_t( &
                    from_atom_fractions( &
                            material_component_t( &
                                    [components1%chemical(), components2%chemical()], &
                                    [components1%fraction_()*factor1, components2%fraction_()*factor2])), &
                    module_t(MODULE_NAME), &
                    procedure_t("combine_materials_by_atom_factors"))
        end associate
    end function

    function combine_materials_by_weight_factors( &
            material1, factor1, material2, factor2) result(combined)
        type(material_t), intent(in) :: material1
        double precision, intent(in) :: factor1
        type(material_t), intent(in) :: material2
        double precision, intent(in) :: factor2
        type(fallible_material_t) :: combined

        associate( &
                components1 => material1%components(), &
                components2 => material2%components())
            associate( &
                    chemicals1 => components1%chemical(), &
                    chemicals2 => components2%chemical())
                combined = fallible_material_t( &
                        from_weight_fractions( &
                                material_component_t( &
                                        [chemicals1, chemicals2], &
                                        [ material1%weight_fraction(chemicals1%symbol())*factor1 &
                                        , material2%weight_fraction(chemicals2%symbol())*factor2 &
                                        ])), &
                        module_t(MODULE_NAME), &
                        procedure_t("combine_materials_by_weight_factors"))
            end associate
        end associate
    end function

    function material_from_atom_fractions(components) result(new_fallible_material)
        type(material_component_t), intent(in) :: components(:)
        type(fallible_material_t) :: new_fallible_material

        character(len=*), parameter :: PROCEDURE_NAME = "material_from_atom_fractions"
        type(fallible_material_components_t) :: fixed_components

        fixed_components = fallible_material_components_t(components)
        new_fallible_material%messages_ = message_list_t( &
                fixed_components%messages(), &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME))
        if (fixed_components%failed()) then
            new_fallible_material%errors_ = error_list_t( &
                    fixed_components%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            new_fallible_material%material_ = from_atom_fractions_unsafe( &
                    fixed_components%components())
        end if
    end function

    function material_from_weight_fractions(components) result(new_fallible_material)
        type(material_component_t), intent(in) :: components(:)
        type(fallible_material_t) :: new_fallible_material

        character(len=*), parameter :: PROCEDURE_NAME = "material_from_weight_fractions"
        type(fallible_material_components_t) :: fixed_components

        fixed_components = fallible_material_components_t(components)
        new_fallible_material%messages_ = message_list_t( &
                fixed_components%messages(), &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME))
        if (fixed_components%failed()) then
            new_fallible_material%errors_ = error_list_t( &
                    fixed_components%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            new_fallible_material%material_ = from_weight_fractions_unsafe( &
                    fixed_components%components())
        end if
    end function

    function material_from_fallible_atom_fractions( &
            maybe_components, module_, procedure_) result(fallible_material)
        type(fallible_material_components_t), intent(in) :: maybe_components
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_material_t) :: fallible_material

        fallible_material%messages_ = message_list_t( &
                maybe_components%messages(), module_, procedure_)
        if (maybe_components%failed()) then
            fallible_material%errors_ = error_list_t( &
                    maybe_components%errors(), module_, procedure_)
        else
            fallible_material = fallible_material_t( &
                    from_atom_fractions(maybe_components%components()), &
                    module_, &
                    procedure_)
        end if
    end function

    function material_from_fallible_weight_fractions( &
            maybe_components, module_, procedure_) result(fallible_material)
        type(fallible_material_components_t), intent(in) :: maybe_components
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_material_t) :: fallible_material

        fallible_material%messages_ = message_list_t( &
                maybe_components%messages(), module_, procedure_)
        if (maybe_components%failed()) then
            fallible_material%errors_ = error_list_t( &
                    maybe_components%errors(), module_, procedure_)
        else
            fallible_material = fallible_material_t( &
                    from_weight_fractions(maybe_components%components()), &
                    module_, &
                    procedure_)
        end if
    end function

    function from_errors(errors) result(fallible_material)
        type(error_list_t), intent(in) :: errors
        type(fallible_material_t) :: fallible_material

        fallible_material%errors_ = errors
    end function

    function from_fallible_material( &
            fallible_material, module_, procedure_) result(new_fallible_material)
        type(fallible_material_t), intent(in) :: fallible_material
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_material_t) :: new_fallible_material

        new_fallible_material%messages_ = message_list_t( &
                fallible_material%messages_, module_, procedure_)
        if (fallible_material%failed()) then
            new_fallible_material%errors_ = error_list_t( &
                    fallible_material%errors_, module_, procedure_)
        else
            new_fallible_material%material_ = fallible_material%material_
        end if
    end function

    function from_json(json) result(fallible_material)
        type(json_object_t), intent(in) :: json
        type(fallible_material_t) :: fallible_material

        character(len=*), parameter :: PROCEDURE_NAME = "from_json"
        type(fallible_json_value_t) :: maybe_atom_fractions
        type(fallible_json_value_t) :: maybe_weight_fractions

        maybe_atom_fractions = json%get_element("atom fractions")
        if (maybe_atom_fractions%failed()) then
            maybe_weight_fractions = json%get_element("weight fractions")
            if (maybe_weight_fractions%failed()) then
                fallible_material%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "material composition must either have" &
                        // " atom fractions or weight fractions"))
            else
                select type (fractions_array => maybe_weight_fractions%value_())
                type is (json_array_t)
                    fallible_material = from_weight_fractions( &
                            fallible_material_components_t(fractions_array), &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME))
                class default
                    fallible_material%errors_ = error_list_t(fatal_t( &
                            INVALID_ARGUMENT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "weight fractions must be an array, but was: " // fractions_array%to_compact_string()))
                end select
            end if
        else
            select type (fractions_array => maybe_atom_fractions%value_())
            type is (json_array_t)
                fallible_material = from_atom_fractions( &
                        fallible_material_components_t(fractions_array), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            class default
                fallible_material%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "atom fractions must be an array, but was: " // fractions_array%to_compact_string()))
            end select
        end if
    end function

    pure function failed(self)
        class(fallible_material_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function material(self)
        class(fallible_material_t), intent(in) :: self
        type(material_t) :: material

        material = self%material_
    end function

    function messages(self)
        class(fallible_material_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    function errors(self)
        class(fallible_material_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
