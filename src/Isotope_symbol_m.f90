module Isotope_symbol_m
    use Element_symbol_m, only: &
            ElementSymbol_t, H, He, Li, Be, B, C, N, O, Ar, Kr, Xe
    use iso_varying_string, only: VARYING_STRING, operator(//)
    use strff, only: toString

    implicit none
    private

    type, public :: IsotopeSymbol_t
        private
        type(ElementSymbol_t) :: element
        integer :: mass_number
    contains
        private
        procedure, public :: is
        procedure :: isotopeSymbolEquals
        generic, public :: operator(==) => isotopeSymbolEquals
        procedure, public :: toString => isotopeSymbolToString
    end type IsotopeSymbol_t

    type(IsotopeSymbol_t), parameter, public :: H_1_SYM = IsotopeSymbol_t(H, 1)
    type(IsotopeSymbol_t), parameter, public :: H_2_SYM = IsotopeSymbol_t(H, 2)
    type(IsotopeSymbol_t), parameter, public :: H_3_SYM = IsotopeSymbol_t(H, 3)

    type(IsotopeSymbol_t), parameter, public :: He_3_SYM = IsotopeSymbol_t(He, 3)
    type(IsotopeSymbol_t), parameter, public :: He_4_SYM = IsotopeSymbol_t(He, 4)

    type(IsotopeSymbol_t), parameter, public :: Li_6_SYM = IsotopeSymbol_t(Li, 6)
    type(IsotopeSymbol_t), parameter, public :: Li_7_SYM = IsotopeSymbol_t(Li, 7)

    type(IsotopeSymbol_t), parameter, public :: Be_9_SYM = IsotopeSymbol_t(Be, 9)

    type(IsotopeSymbol_t), parameter, public :: B_10_SYM = IsotopeSymbol_t(B, 10)
    type(IsotopeSymbol_t), parameter, public :: B_11_SYM = IsotopeSymbol_t(B, 11)

    type(IsotopeSymbol_t), parameter, public :: C_12_SYM = IsotopeSymbol_t(C, 12)
    type(IsotopeSymbol_t), parameter, public :: C_13_SYM = IsotopeSymbol_t(C, 13)

    type(IsotopeSymbol_t), parameter, public :: N_14_SYM = IsotopeSymbol_t(N, 14)
    type(IsotopeSymbol_t), parameter, public :: N_15_SYM = IsotopeSymbol_t(N, 15)

    type(IsotopeSymbol_t), parameter, public :: O_16_SYM = IsotopeSymbol_t(O, 16)
    type(IsotopeSymbol_t), parameter, public :: O_17_SYM = IsotopeSymbol_t(O, 17)
    type(IsotopeSymbol_t), parameter, public :: O_18_SYM = IsotopeSymbol_t(O, 18)

    type(IsotopeSymbol_t), parameter, public :: Ar_36_SYM = IsotopeSymbol_t(Ar, 36)
    type(IsotopeSymbol_t), parameter, public :: Ar_38_SYM = IsotopeSymbol_t(Ar, 38)
    type(IsotopeSymbol_t), parameter, public :: Ar_40_SYM = IsotopeSymbol_t(Ar, 40)

    type(IsotopeSymbol_t), parameter, public :: Kr_78_SYM = IsotopeSymbol_t(Kr, 78)
    type(IsotopeSymbol_t), parameter, public :: Kr_80_SYM = IsotopeSymbol_t(Kr, 80)
    type(IsotopeSymbol_t), parameter, public :: Kr_82_SYM = IsotopeSymbol_t(Kr, 82)
    type(IsotopeSymbol_t), parameter, public :: Kr_83_SYM = IsotopeSymbol_t(Kr, 83)
    type(IsotopeSymbol_t), parameter, public :: Kr_84_SYM = IsotopeSymbol_t(Kr, 84)
    type(IsotopeSymbol_t), parameter, public :: Kr_86_SYM = IsotopeSymbol_t(Kr, 86)

    type(IsotopeSymbol_t), parameter, public :: Xe_124_SYM = IsotopeSymbol_t(Xe, 124)
    type(IsotopeSymbol_t), parameter, public :: Xe_126_SYM = IsotopeSymbol_t(Xe, 126)
    type(IsotopeSymbol_t), parameter, public :: Xe_128_SYM = IsotopeSymbol_t(Xe, 128)
    type(IsotopeSymbol_t), parameter, public :: Xe_129_SYM = IsotopeSymbol_t(Xe, 129)
    type(IsotopeSymbol_t), parameter, public :: Xe_130_SYM = IsotopeSymbol_t(Xe, 130)
    type(IsotopeSymbol_t), parameter, public :: Xe_131_SYM = IsotopeSymbol_t(Xe, 131)
    type(IsotopeSymbol_t), parameter, public :: Xe_132_SYM = IsotopeSymbol_t(Xe, 132)
    type(IsotopeSymbol_t), parameter, public :: Xe_134_SYM = IsotopeSymbol_t(Xe, 134)
    type(IsotopeSymbol_t), parameter, public :: Xe_136_SYM = IsotopeSymbol_t(Xe, 136)
contains
    elemental function is(self, element)
        class(IsotopeSymbol_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        logical :: is

        is = self%element == element
    end function is

    elemental function isotopeSymbolEquals(lhs, rhs)
        class(IsotopeSymbol_t), intent(in) :: lhs
        type(IsotopeSymbol_t), intent(in) :: rhs
        logical :: isotopeSymbolEquals

        isotopeSymbolEquals = &
                lhs%element == rhs%element &
                .and. lhs%mass_number == rhs%mass_number
    end function isotopeSymbolEquals

    elemental function isotopeSymbolToString(self) result(string)
        class(IsotopeSymbol_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%element%toString() // "-" // toString(self%mass_number)
    end function isotopeSymbolToString
end module Isotope_symbol_m
