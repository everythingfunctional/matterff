module matterff_fallible_matter_m
    use erloff, only: &
            error_list_t, &
            fatal_t, &
            internal_t, &
            message_list_t, &
            module_t, &
            procedure_t
    use iso_varying_string, only: operator(//)
    use jsonff, only: fallible_json_value_t, json_object_t, json_string_t
    use matterff_fallible_material_m, only: fallible_material_t
    use matterff_material_m, only: material_t
    use matterff_matter_m, only: matter_t, matter_unsafe
    use matterff_utilities_m, only: INVALID_ARGUMENT
    use quaff, only: &
            amount_t, &
            fallible_amount_t, &
            fallible_mass_t, &
            mass_t, &
            operator(/), &
            operator(.unit.), &
            parse_amount, &
            parse_mass, &
            MOLS

    implicit none
    private
    public :: fallible_matter_t

    type :: fallible_matter_t
        private
        type(matter_t) :: matter_
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: matter
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface fallible_matter_t
        module procedure with_amount
        module procedure with_fallible_amount
        module procedure with_mass
        module procedure with_fallible_mass
        module procedure from_fallible_matter
        module procedure from_json
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_matter_m"
contains
    function with_amount(amount, material)
        type(amount_t), intent(in) :: amount
        type(material_t), intent(in) :: material
        type(fallible_matter_t) :: with_amount

        if (amount < (0.0d0.unit.MOLS)) then
            with_amount%errors_ = error_list_t(internal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t("with_amount"), &
                    "Cannot have negative matter"))
        else
            with_amount%matter_ = matter_unsafe(amount, material)
        end if
    end function

    function with_fallible_amount(maybe_amount, maybe_material, module_, procedure_)
        type(fallible_amount_t), intent(in) :: maybe_amount
        type(fallible_material_t), intent(in) :: maybe_material
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_matter_t) :: with_fallible_amount

        with_fallible_amount%messages_ = message_list_t( &
                maybe_material%messages(), module_, procedure_)
        if (any([maybe_amount%failed(), maybe_material%failed()])) then
                with_fallible_amount%errors_ = error_list_t(&
                        [maybe_amount%errors(), maybe_material%errors()], &
                        module_, &
                        procedure_)
        else
            with_fallible_amount = fallible_matter_t( &
                    fallible_matter_t(maybe_amount%amount(), maybe_material%material()), &
                    module_, &
                    procedure_)
        end if
    end function

    function with_mass(mass, material)
        type(mass_t), intent(in) :: mass
        type(material_t), intent(in) :: material
        type(fallible_matter_t) :: with_mass

        with_mass = fallible_matter_t( &
                fallible_matter_t(mass / material%molar_mass(), material), &
                module_t(MODULE_NAME), &
                procedure_t("with_mass"))
    end function

    function with_fallible_mass(maybe_mass, maybe_material, module_, procedure_)
        type(fallible_mass_t), intent(in) :: maybe_mass
        type(fallible_material_t), intent(in) :: maybe_material
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_matter_t) :: with_fallible_mass

        with_fallible_mass%messages_ = message_list_t( &
                maybe_material%messages(), module_, procedure_)
        if (any([maybe_mass%failed(), maybe_material%failed()])) then
            with_fallible_mass%errors_ = error_list_t(&
                    [maybe_mass%errors(), maybe_material%errors()], &
                    module_, &
                    procedure_)
        else
            with_fallible_mass = fallible_matter_t( &
                    fallible_matter_t(maybe_mass%mass(), maybe_material%material()), &
                    module_, &
                    procedure_)
        end if
    end function

    function from_fallible_matter(fallible_matter, module_, procedure_) result(new_fallible_matter)
        type(fallible_matter_t), intent(in) :: fallible_matter
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_matter_t) :: new_fallible_matter

        new_fallible_matter%messages_ = message_list_t( &
                fallible_matter%messages_, module_, procedure_)
        if (fallible_matter%failed()) then
            new_fallible_matter%errors_ = error_list_t( &
                    fallible_matter%errors_, module_, procedure_)
        else
            new_fallible_matter%matter_ = fallible_matter%matter_
        end if
    end function

    function from_json(json)
        type(json_object_t), intent(in) :: json
        type(fallible_matter_t) :: from_json

        character(len=*), parameter :: PROCEDURE_NAME = "from_json"
        type(fallible_json_value_t) :: maybe_amount_string
        type(fallible_json_value_t) :: maybe_mass_string

        maybe_amount_string = json%get_element("amount")
        if (maybe_amount_string%failed()) then
            maybe_mass_string = json%get_element("mass")
            if (maybe_mass_string%failed()) then
                from_json%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "matter must have amount or mass"))
            else
                select type (mass_string => maybe_mass_string%value_())
                type is (json_string_t)
                    from_json = fallible_matter_t( &
                            parse_mass(mass_string%get_value()), &
                            extract_material(json), &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME))
                class default
                    from_json%errors_ = error_list_t(fatal_t( &
                            INVALID_ARGUMENT, &
                            module_t(MODULE_NAME), &
                            procedure_t(PROCEDURE_NAME), &
                            "mass must be a string, but was: " // mass_string%to_compact_string()))
                end select
            end if
        else
            select type (amount_string => maybe_amount_string%value_())
            type is (json_string_t)
                from_json = fallible_matter_t( &
                        parse_amount(amount_string%get_value()), &
                        extract_material(json), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            class default
                from_json%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "amount must be a string, but was: " // amount_string%to_compact_string()))
            end select
        end if
    end function

    pure function failed(self)
        class(fallible_matter_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function matter(self)
        class(fallible_matter_t), intent(in) :: self
        type(matter_t) :: matter

        matter = self%matter_
    end function

    function messages(self)
        class(fallible_matter_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    function errors(self)
        class(fallible_matter_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    function extract_material(json) result(maybe_material)
        type(json_object_t), intent(in) :: json
        type(fallible_material_t) :: maybe_material

        character(len=*), parameter :: PROCEDURE_NAME = "extract_material"
        type(fallible_json_value_t) :: maybe_material_object

        maybe_material_object = json%get_element("material")
        if (maybe_material_object%failed()) then
            maybe_material = fallible_material_t(error_list_t( &
                    maybe_material_object%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME)))
        else
            select type (material_object => maybe_material_object%value_())
            type is (json_object_t)
                maybe_material = fallible_material_t( &
                        fallible_material_t(material_object), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            class default
                maybe_material = fallible_material_t(error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "material must be an object, but was: " // material_object%to_compact_string())))
            end select
        end if
    end function
end module
