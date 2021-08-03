module matterff_isotope_symbol_m
    use iso_varying_string, only: varying_string, operator(//)
    use matterff_element_symbol_m, only: &
            element_symbol_t, H, He, Li, Be, B, C, N, O, Ar, Kr, Xe
    use strff, only: to_string

    implicit none
    private
    public :: &
            isotope_symbol_t, &
            H_1_SYM, &
            H_2_SYM, &
            H_3_SYM, &
            He_3_SYM, &
            He_4_SYM, &
            Li_6_SYM, &
            Li_7_SYM, &
            Be_9_SYM, &
            B_10_SYM, &
            B_11_SYM, &
            C_12_SYM, &
            C_13_SYM, &
            N_14_SYM, &
            N_15_SYM, &
            O_16_SYM, &
            O_17_SYM, &
            O_18_SYM, &
            Ar_36_SYM, &
            Ar_38_SYM, &
            Ar_40_SYM, &
            Kr_78_SYM, &
            Kr_80_SYM, &
            Kr_82_SYM, &
            Kr_83_SYM, &
            Kr_84_SYM, &
            Kr_85_SYM, &
            Kr_86_SYM, &
            Xe_124_SYM, &
            Xe_126_SYM, &
            Xe_128_SYM, &
            Xe_129_SYM, &
            Xe_130_SYM, &
            Xe_131_SYM, &
            Xe_132_SYM, &
            Xe_134_SYM, &
            Xe_136_SYM

    type :: isotope_symbol_t
        private
        type(element_symbol_t) :: element
        integer :: mass_number
    contains
        private
        procedure, public :: is
        procedure :: isotope_symbol_equals
        generic, public :: operator(==) => isotope_symbol_equals
        procedure, public :: to_string => isotope_symbol_to_string
    end type

    type(isotope_symbol_t), parameter :: H_1_SYM = isotope_symbol_t(H, 1)
    type(isotope_symbol_t), parameter :: H_2_SYM = isotope_symbol_t(H, 2)
    type(isotope_symbol_t), parameter :: H_3_SYM = isotope_symbol_t(H, 3)

    type(isotope_symbol_t), parameter :: He_3_SYM = isotope_symbol_t(He, 3)
    type(isotope_symbol_t), parameter :: He_4_SYM = isotope_symbol_t(He, 4)

    type(isotope_symbol_t), parameter :: Li_6_SYM = isotope_symbol_t(Li, 6)
    type(isotope_symbol_t), parameter :: Li_7_SYM = isotope_symbol_t(Li, 7)

    type(isotope_symbol_t), parameter :: Be_9_SYM = isotope_symbol_t(Be, 9)

    type(isotope_symbol_t), parameter :: B_10_SYM = isotope_symbol_t(B, 10)
    type(isotope_symbol_t), parameter :: B_11_SYM = isotope_symbol_t(B, 11)

    type(isotope_symbol_t), parameter :: C_12_SYM = isotope_symbol_t(C, 12)
    type(isotope_symbol_t), parameter :: C_13_SYM = isotope_symbol_t(C, 13)

    type(isotope_symbol_t), parameter :: N_14_SYM = isotope_symbol_t(N, 14)
    type(isotope_symbol_t), parameter :: N_15_SYM = isotope_symbol_t(N, 15)

    type(isotope_symbol_t), parameter :: O_16_SYM = isotope_symbol_t(O, 16)
    type(isotope_symbol_t), parameter :: O_17_SYM = isotope_symbol_t(O, 17)
    type(isotope_symbol_t), parameter :: O_18_SYM = isotope_symbol_t(O, 18)

    type(isotope_symbol_t), parameter :: Ar_36_SYM = isotope_symbol_t(Ar, 36)
    type(isotope_symbol_t), parameter :: Ar_38_SYM = isotope_symbol_t(Ar, 38)
    type(isotope_symbol_t), parameter :: Ar_40_SYM = isotope_symbol_t(Ar, 40)

    type(isotope_symbol_t), parameter :: Kr_78_SYM = isotope_symbol_t(Kr, 78)
    type(isotope_symbol_t), parameter :: Kr_80_SYM = isotope_symbol_t(Kr, 80)
    type(isotope_symbol_t), parameter :: Kr_82_SYM = isotope_symbol_t(Kr, 82)
    type(isotope_symbol_t), parameter :: Kr_83_SYM = isotope_symbol_t(Kr, 83)
    type(isotope_symbol_t), parameter :: Kr_84_SYM = isotope_symbol_t(Kr, 84)
    type(isotope_symbol_t), parameter :: Kr_85_SYM = isotope_symbol_t(Kr, 85)
    type(isotope_symbol_t), parameter :: Kr_86_SYM = isotope_symbol_t(Kr, 86)

    type(isotope_symbol_t), parameter :: Xe_124_SYM = isotope_symbol_t(Xe, 124)
    type(isotope_symbol_t), parameter :: Xe_126_SYM = isotope_symbol_t(Xe, 126)
    type(isotope_symbol_t), parameter :: Xe_128_SYM = isotope_symbol_t(Xe, 128)
    type(isotope_symbol_t), parameter :: Xe_129_SYM = isotope_symbol_t(Xe, 129)
    type(isotope_symbol_t), parameter :: Xe_130_SYM = isotope_symbol_t(Xe, 130)
    type(isotope_symbol_t), parameter :: Xe_131_SYM = isotope_symbol_t(Xe, 131)
    type(isotope_symbol_t), parameter :: Xe_132_SYM = isotope_symbol_t(Xe, 132)
    type(isotope_symbol_t), parameter :: Xe_134_SYM = isotope_symbol_t(Xe, 134)
    type(isotope_symbol_t), parameter :: Xe_136_SYM = isotope_symbol_t(Xe, 136)
contains
    elemental function is(self, element)
        class(isotope_symbol_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        logical :: is

        is = self%element == element
    end function

    elemental function isotope_symbol_equals(lhs, rhs)
        class(isotope_symbol_t), intent(in) :: lhs
        type(isotope_symbol_t), intent(in) :: rhs
        logical :: isotope_symbol_equals

        isotope_symbol_equals = &
                lhs%element == rhs%element &
                .and. lhs%mass_number == rhs%mass_number
    end function

    elemental function isotope_symbol_to_string(self) result(string)
        class(isotope_symbol_t), intent(in) :: self
        type(varying_string) :: string

        string = self%element%to_string() // "-" // to_string(self%mass_number)
    end function
end module
