module matterff_chemical_symbol_component_m
    use iso_varying_string, only: varying_string, operator(//)
    use jsonff, only: &
            json_object_t, json_integer_t, json_member_unsafe, json_string_unsafe
    use matterff_element_symbol_m, only: element_symbol_t
    use strff, only: to_string

    implicit none
    private
    public :: chemical_symbol_component_t

    type :: chemical_symbol_component_t
        private
        type(element_symbol_t) :: element_
        integer :: multiple_
    contains
        private
        procedure, public :: element
        procedure :: chemical_symbol_component_equals
        generic, public :: operator(==) => chemical_symbol_component_equals
        procedure, public :: to_json
        procedure, public :: to_string => symbol_to_string
    end type

    interface chemical_symbol_component_t
        module procedure constructor
    end interface
contains
    elemental function constructor( &
            element, multiple) result(new_chemical_symbol_component)
        type(element_symbol_t), intent(in) :: element
        integer, intent(in) :: multiple
        type(chemical_symbol_component_t) :: new_chemical_symbol_component

        new_chemical_symbol_component%element_ = element
        new_chemical_symbol_component%multiple_ = multiple
    end function

    elemental function element(self)
        class(chemical_symbol_component_t), intent(in) :: self
        type(element_symbol_t) :: element

        element = self%element_
    end function

    elemental function chemical_symbol_component_equals(lhs, rhs)
        class(chemical_symbol_component_t), intent(in) :: lhs
        type(chemical_symbol_component_t), intent(in) :: rhs
        logical :: chemical_symbol_component_equals

        chemical_symbol_component_equals = &
                lhs%element_ == rhs%element_ &
                .and. lhs%multiple_ == rhs%multiple_
    end function

    impure elemental function to_json(self) result(json)
        class(chemical_symbol_component_t), intent(in) :: self
        type(json_object_t) :: json

        json = json_object_t([ &
                json_member_unsafe("element", json_string_unsafe(self%element_%to_string())), &
                json_member_unsafe("multiple", json_integer_t(self%multiple_))])
    end function

    elemental function symbol_to_string(self) result(string)
        class(chemical_symbol_component_t), intent(in) :: self
        type(varying_string) :: string

        if (self%multiple_ == 1) then
            string = self%element_%to_string()
        else
            string = self%element_%to_string() // to_string(self%multiple_)
        end if
    end function
end module
