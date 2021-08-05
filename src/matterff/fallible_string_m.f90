module matterff_fallible_string_m
    use erloff, only: error_list_t, module_t, procedure_t
    use iso_varying_string, only: varying_string

    implicit none
    private
    public :: fallible_string_t

    type :: fallible_string_t
        private
        type(error_list_t) :: errors_
        type(varying_string) :: value__
    contains
        private
        procedure, public :: failed
        procedure, public :: errors
        procedure, public :: value_
    end type

    interface fallible_string_t
        module procedure from_errors
        module procedure from_fallible_string
        module procedure from_string
    end interface
contains
    function from_errors(errors) result(fallible_string)
        type(error_list_t), intent(in) :: errors
        type(fallible_string_t) :: fallible_string

        fallible_string%errors_ = errors
    end function

    function from_fallible_string( &
            fallible_string, module_, procedure_) result(new_fallible_string)
        type(fallible_string_t), intent(in) :: fallible_string
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_string_t) :: new_fallible_string

        if (fallible_string%failed()) then
            new_fallible_string%errors_ = error_list_t( &
                    fallible_string%errors_, module_, procedure_)
        else
            new_fallible_string%value__ = fallible_string%value__
        end if
    end function

    function from_string(value_) result(fallible_string)
        type(varying_string), intent(in) :: value_
        type(fallible_string_t) :: fallible_string

        fallible_string%value__ = value_
    end function

    pure function failed(self)
        class(fallible_string_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function errors(self)
        class(fallible_string_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    pure function value_(self)
        class(fallible_string_t), intent(in) :: self
        type(varying_string) :: value_

        value_ = self%value__
    end function
end module
