module matterff_fallible_element_symbol_m
    use erloff, only: error_list_t
    use matterff_element_symbol_m, only: element_symbol_t

    implicit none
    private
    public :: fallible_element_symbol_t

    type :: fallible_element_symbol_t
        private
        type(element_symbol_t) :: symbol_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: errors
        procedure, public :: symbol
    end type

    interface fallible_element_symbol_t
        module procedure from_symbol
        module procedure from_errors
    end interface
contains
    function from_symbol(symbol) result(fallible_element_symbol)
        type(element_symbol_t), intent(in) :: symbol
        type(fallible_element_symbol_t) :: fallible_element_symbol

        fallible_element_symbol%symbol_ = symbol
    end function

    function from_errors(errors) result(fallible_element_symbol)
        type(error_list_t), intent(in) :: errors
        type(fallible_element_symbol_t) :: fallible_element_symbol

        fallible_element_symbol%errors_ = errors
    end function

    pure function failed(self)
        class(fallible_element_symbol_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function symbol(self)
        class(fallible_element_symbol_t), intent(in) :: self
        type(element_symbol_t) :: symbol

        symbol = self%symbol_
    end function

    function errors(self)
        class(fallible_element_symbol_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
