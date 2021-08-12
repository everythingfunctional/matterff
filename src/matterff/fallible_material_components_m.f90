module matterff_fallible_material_components_m
    use erloff, only: &
            error_list_t, &
            info_t, &
            internal_t, &
            message_list_t, &
            module_t, &
            procedure_t
    use jsonff, only: json_array_t
    use matterff_fallible_material_component_m, only: fallible_material_component_t
    use matterff_material_component_m, only: material_component_t
    use matterff_utilities_m, only: &
            operator(.sumsTo.), INVALID_ARGUMENT, NORMALIZED_FRACTIONS

    implicit none
    private
    public :: fallible_material_components_t

    type :: fallible_material_components_t
        private
        type(material_component_t), allocatable :: components_(:)
        type(message_list_t) :: messages_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: components
        procedure, public :: messages
        procedure, public :: errors
    end type

    interface fallible_material_components_t
        module procedure check_consistency
        module procedure from_json
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_material_components_m"
contains
    function check_consistency(components) result(fallible_components)
        type(material_component_t), intent(in) :: components(:)
        type(fallible_material_components_t) :: fallible_components

        character(len=*), parameter :: PROCEDURE_NAME = "check_consistency"

        associate(fractions => components%fraction_())
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
                    allocate(fallible_components%components_, source = material_component_t( &
                            components%chemical(), fractions / sum(fractions)))
                end if
            else
                fallible_components%errors_ = error_list_t(internal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "All fractions must be greater than 0."))
            end if
        end associate
    end function

    function from_json(json) result(fallible_material_components)
        type(json_array_t), intent(in) :: json
        type(fallible_material_components_t) :: fallible_material_components

        associate(maybe_components => fallible_material_component_t(json%get_elements()))
            associate(failures => maybe_components%failed())
                if (any(failures)) then
                    fallible_material_components%errors_ = error_list_t( &
                            pack(maybe_components%errors(), failures), &
                            module_t(MODULE_NAME), &
                            procedure_t("from_json"))
                else
                    allocate(fallible_material_components%components_, source = &
                            maybe_components%material_component())
                end if
            end associate
        end associate
    end function

    pure function failed(self)
        class(fallible_material_components_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function components(self)
        class(fallible_material_components_t), intent(in) :: self
        type(material_component_t), allocatable :: components(:)

        components = self%components_
    end function

    function messages(self)
        class(fallible_material_components_t), intent(in) :: self
        type(message_list_t) :: messages

        messages = self%messages_
    end function

    function errors(self)
        class(fallible_material_components_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
