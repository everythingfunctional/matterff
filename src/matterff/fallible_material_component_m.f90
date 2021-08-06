module matterff_fallible_material_component_m
    use erloff, only: &
            error_list_t, fatal_t, message_list_t, module_t, procedure_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: &
            fallible_json_value_t, json_number_t, json_object_t, json_value_t
    use matterff_fallible_chemical_m, only: fallible_chemical_t
    use matterff_material_component_m, only: material_component_t
    use matterff_utilities_m, only: INVALID_ARGUMENT

    implicit none
    private
    public :: fallible_material_component_t

    type :: fallible_material_component_t
        private
        type(material_component_t) :: material_component_
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: material_component
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface fallible_material_component_t
        module procedure from_fallible_material_component
        module procedure from_json_value
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_material_component_m"
contains
    function from_fallible_material_component( &
            fallible_material_component, &
            module_, &
            procedure_) &
            result(new_fallible_material_component)
        type(fallible_material_component_t), intent(in) :: fallible_material_component
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_material_component_t) :: new_fallible_material_component

        new_fallible_material_component%messages_ = message_list_t( &
                fallible_material_component%messages_, module_, procedure_)
        if (fallible_material_component%failed()) then
            new_fallible_material_component%errors_ = error_list_t(&
                    fallible_material_component%errors_, module_, procedure_)
        else
            new_fallible_material_component%material_component_ = &
                    fallible_material_component%material_component_
        end if
    end function

    function from_json_object(json) result(new_fallible_material_component)
        type(json_object_t), intent(in) :: json
        type(fallible_material_component_t) :: new_fallible_material_component

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_object"
        type(fallible_json_value_t) :: maybe_fraction
        type(fallible_chemical_t) :: maybe_chemical

        maybe_chemical = fallible_chemical_t(json)
        new_fallible_material_component%messages_ = message_list_t( &
                maybe_chemical%messages(), &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME))
        if (maybe_chemical%failed()) then
            new_fallible_material_component%errors_ = error_list_t( &
                    maybe_chemical%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            maybe_fraction = json%get_element("fraction")
            if (maybe_fraction%failed()) then
                new_fallible_material_component%errors_ = error_list_t( &
                        maybe_fraction%errors(), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            else
                select type (fraction => maybe_fraction%value_())
                type is (json_number_t)
                    new_fallible_material_component%material_component_= &
                            material_component_t(maybe_chemical%chemical(), fraction%get_value())
                class default
                    new_fallible_material_component%errors_ = error_list_t(fatal_t( &
                            INVALID_ARGUMENT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "fraction must be a number"))
                end select
            end if
        end if
    end function

    function from_json_value(json) result(new_fallible_material_component)
        class(json_value_t), intent(in) :: json
        type(fallible_material_component_t) :: new_fallible_material_component

        character(len=*), parameter :: PROCEDURE_NAME = "from_json_value"

        select type (json)
        type is (json_object_t)
            new_fallible_material_component = fallible_material_component_t( &
                    from_json_object(json), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        class default
            new_fallible_material_component%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "material component must be an object, but was" // json%to_compact_string()))
        end select
    end function

    elemental function failed(self)
        class(fallible_material_component_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    elemental function material_component(self)
        class(fallible_material_component_t), intent(in) :: self
        type(material_component_t) :: material_component

        material_component = self%material_component_
    end function

    impure elemental function messages(self)
        class(fallible_material_component_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    impure elemental function errors(self)
        class(fallible_material_component_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
