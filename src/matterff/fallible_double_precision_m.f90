module matterff_fallible_double_precision_m
    use erloff, only: error_list_t, module_t, procedure_t

    implicit none
    private
    public :: fallible_double_precision_t

    type :: fallible_double_precision_t
        private
        type(error_list_t) :: errors_
        double precision :: value__
    contains
        private
        procedure, public :: failed
        procedure, public :: errors
        procedure, public :: value_
    end type

    interface fallible_double_precision_t
        module procedure from_double_precision
        module procedure from_errors
        module procedure from_fallible_double_precision
    end interface
contains
    function from_double_precision(value_) result(fallible_double_precision)
        double precision, intent(in) :: value_
        type(fallible_double_precision_t) :: fallible_double_precision

        fallible_double_precision%value__ = value_
    end function

    function from_errors(errors) result(fallible_double_precision)
        type(error_list_t), intent(in) :: errors
        type(fallible_double_precision_t) :: fallible_double_precision

        fallible_double_precision%errors_ = errors
    end function

    function from_fallible_double_precision( &
            fallible_double_precision, &
            module_, &
            procedure_) &
            result(new_fallible_double_precision)
        type(fallible_double_precision_t), intent(in) :: fallible_double_precision
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_double_precision_t) :: new_fallible_double_precision

        if (fallible_double_precision%failed()) then
            new_fallible_double_precision%errors_ = error_list_t( &
                    fallible_double_precision%errors_, module_, procedure_)
        else
            new_fallible_double_precision%value__ = fallible_double_precision%value__
        end if
    end function

    pure function failed(self)
        class(fallible_double_precision_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    function errors(self)
        class(fallible_double_precision_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function

    pure function value_(self)
        class(fallible_double_precision_t), intent(in) :: self
        double precision :: value_

        value_ = self%value__
    end function
end module
