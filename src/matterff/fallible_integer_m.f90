module matterff_fallible_integer_m
    use erloff, only: error_list_t, module_t, procedure_t

    implicit none
    private
    public :: fallible_integer_t

    type :: fallible_integer_t
        private
        type(error_list_t) :: errors_
        integer :: value__
    contains
        private
        procedure, public :: failed
        procedure, public :: errors
        procedure, public :: value_
    end type

    interface fallible_integer_t
        module procedure from_errors
        module procedure from_fallible_integer
        module procedure from_integer
    end interface
contains
    function from_errors(errors) result(fallible_integer)
        type(error_list_t), intent(in) :: errors
        type(fallible_integer_t) :: fallible_integer

        fallible_integer%errors_ = errors
    end function

    function from_fallible_integer( &
            fallible_integer, module_, procedure_) result(new_fallible_integer)
        type(fallible_integer_t), intent(in) :: fallible_integer
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_integer_t) :: new_fallible_integer

        if (fallible_integer%failed()) then
            new_fallible_integer%errors_ = error_list_t( &
                    fallible_integer%errors_, module_, procedure_)
        else
            new_fallible_integer%value__ = fallible_integer%value__
        end if
    end function

    function from_integer(value_) result(fallible_integer)
        integer, intent(in) :: value_
        type(fallible_integer_t) :: fallible_integer

        fallible_integer%value__ = value_
    end function

    pure function failed(self)
        class(fallible_integer_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function errors(self)
        class(fallible_integer_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    pure function value_(self)
        class(fallible_integer_t), intent(in) :: self
        integer :: value_

        value_ = self%value__
    end function
end module
