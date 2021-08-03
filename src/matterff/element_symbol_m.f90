module matterff_element_symbol_m
    use iso_varying_string, only: varying_string, assignment(=), char

    implicit none
    private
    public :: &
            element_symbol_t, &
            H, &
            He, &
            Li, &
            Be, &
            B, &
            C, &
            N, &
            O, &
            Ar, &
            Kr, &
            Xe

    type, public :: element_symbol_t
        private
        character(len=2) :: symbol
    contains
        private
        procedure :: element_symbol_equals
        generic, public :: operator(==) => element_symbol_equals
        procedure, public :: to_string
    end type

    interface element_symbol_t
        module procedure constructor_c
        module procedure constructor_s
    end interface

    type(element_symbol_t), parameter :: H = element_symbol_t("H ")
    type(element_symbol_t), parameter :: He = element_symbol_t("He")
    type(element_symbol_t), parameter :: Li = element_symbol_t("Li")
    type(element_symbol_t), parameter :: Be = element_symbol_t("Be")
    type(element_symbol_t), parameter :: B = element_symbol_t("B ")
    type(element_symbol_t), parameter :: C = element_symbol_t("C ")
    type(element_symbol_t), parameter :: N = element_symbol_t("N ")
    type(element_symbol_t), parameter :: O = element_symbol_t("O ")
    type(element_symbol_t), parameter :: Ar = element_symbol_t("Ar")
    type(element_symbol_t), parameter :: Kr = element_symbol_t("Kr")
    type(element_symbol_t), parameter :: Xe = element_symbol_t("Xe")
contains
    pure function constructor_c(symbol) result(new_element_symbol)
        character(len=*), intent(in) :: symbol
        type(element_symbol_t) :: new_element_symbol

        new_element_symbol%symbol = symbol
    end function

    pure function constructor_s(symbol) result(new_element_symbol)
        type(varying_string), intent(in) :: symbol
        type(element_symbol_t) :: new_element_symbol

        new_element_symbol = element_symbol_t(char(symbol))
    end function

    elemental function element_symbol_equals(lhs, rhs)
        class(element_symbol_t), intent(in) :: lhs
        type(element_symbol_t), intent(in) :: rhs
        logical :: element_symbol_equals

        element_symbol_equals = lhs%symbol == rhs%symbol
    end function

    elemental function to_string(self) result(string)
        class(element_symbol_t), intent(in) :: self
        type(varying_string) :: string

        string = trim(self%symbol)
    end function
end module
