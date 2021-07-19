module isotope_m
    use element_symbol_m, only: element_symbol_t
    use iso_varying_string, only: varying_string
    use isotope_symbol_m, only: &
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
            Kr_85_SYM, &
            Kr_84_SYM, &
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
    use jsonff, only: &
            json_number_t, json_object_t, json_member_unsafe, json_string_unsafe
    use quaff, only: molar_mass_t
    use strff, only: to_string

    implicit none
    private
    public :: &
            isotope_t, &
            find, &
            H_1, &
            H_2, &
            H_3, &
            He_3, &
            He_4, &
            Li_6, &
            Li_7, &
            Be_9, &
            B_10, &
            B_11, &
            C_12, &
            C_13, &
            N_14, &
            N_15, &
            O_16, &
            O_17, &
            O_18, &
            Ar_36, &
            Ar_38, &
            Ar_40, &
            Kr_78, &
            Kr_80, &
            Kr_82, &
            Kr_83, &
            Kr_84, &
            Kr_85, &
            Kr_86, &
            Xe_124, &
            Xe_126, &
            Xe_128, &
            Xe_129, &
            Xe_130, &
            Xe_131, &
            Xe_132, &
            Xe_134, &
            Xe_136

    type :: isotope_t
        private
        type(isotope_symbol_t), public :: symbol
        type(molar_mass_t), public :: atomic_mass
    contains
        private
        procedure, public :: is
        procedure, public :: to_json_with_fraction
        procedure, public :: to_string => isotope_to_string
    end type

    interface find
        module procedure find_isotope
    end interface

    ! Atomic masses are taken from the 17th Edition of the Chart of Nuclides
    ! Where atomic mass is not provided for an isotope in the table, the
    ! atomic mass is taken from:
    ! [1] https://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl
    ! [2] https://en.wikipedia.org/wiki/Krypton-85
    type(isotope_t), parameter :: H_1 = isotope_t(H_1_SYM, molar_mass_t(kilograms_per_mol = 1.0078250321d-3))
    type(isotope_t), parameter :: H_2 = isotope_t(H_2_SYM, molar_mass_t(kilograms_per_mol = 2.0141017779d-3))
    type(isotope_t), parameter :: H_3 = isotope_t(H_3_SYM, molar_mass_t(kilograms_per_mol = 3.0160492779d-3)) ! [1]

    type(isotope_t), parameter :: He_3 = isotope_t(He_3_SYM, molar_mass_t(kilograms_per_mol = 3.016029319d-3))
    type(isotope_t), parameter :: He_4 = isotope_t(He_4_SYM, molar_mass_t(kilograms_per_mol = 4.0026032542d-3))

    type(isotope_t), parameter :: Li_6 = isotope_t(Li_6_SYM, molar_mass_t(kilograms_per_mol = 6.01512279d-3))
    type(isotope_t), parameter :: Li_7 = isotope_t(Li_7_SYM, molar_mass_t(kilograms_per_mol = 7.0160045d-3))

    type(isotope_t), parameter :: Be_9 = isotope_t(Be_9_SYM, molar_mass_t(kilograms_per_mol = 9.0121822d-3))

    type(isotope_t), parameter :: B_10 = isotope_t(B_10_SYM, molar_mass_t(kilograms_per_mol = 10.012937d-3))
    type(isotope_t), parameter :: B_11 = isotope_t(B_11_SYM, molar_mass_t(kilograms_per_mol = 11.0090354d-3))

    type(isotope_t), parameter :: C_12 = isotope_t(C_12_SYM, molar_mass_t(kilograms_per_mol = 12.0d-3))
    type(isotope_t), parameter :: C_13 = isotope_t(C_13_SYM, molar_mass_t(kilograms_per_mol = 13.003354838d-3))

    type(isotope_t), parameter :: N_14 = isotope_t(N_14_SYM, molar_mass_t(kilograms_per_mol = 14.003074005d-3))
    type(isotope_t), parameter :: N_15 = isotope_t(N_15_SYM, molar_mass_t(kilograms_per_mol = 15.000108898d-3))

    type(isotope_t), parameter :: O_16 = isotope_t(O_16_SYM, molar_mass_t(kilograms_per_mol = 15.9949146196d-3))
    type(isotope_t), parameter :: O_17 = isotope_t(O_17_SYM, molar_mass_t(kilograms_per_mol = 16.9991317d-3))
    type(isotope_t), parameter :: O_18 = isotope_t(O_18_SYM, molar_mass_t(kilograms_per_mol = 17.999161d-3))

    type(isotope_t), parameter :: Ar_36 = isotope_t(Ar_36_SYM, molar_mass_t(kilograms_per_mol = 35.96754511d-3))
    type(isotope_t), parameter :: Ar_38 = isotope_t(Ar_38_SYM, molar_mass_t(kilograms_per_mol = 37.9627324d-3))
    type(isotope_t), parameter :: Ar_40 = isotope_t(Ar_40_SYM, molar_mass_t(kilograms_per_mol = 39.962383123d-3))

    type(isotope_t), parameter :: Kr_78 = isotope_t(Kr_78_SYM, molar_mass_t(kilograms_per_mol = 77.920365d-3))
    type(isotope_t), parameter :: Kr_80 = isotope_t(Kr_80_SYM, molar_mass_t(kilograms_per_mol = 79.916379d-3))
    type(isotope_t), parameter :: Kr_82 = isotope_t(Kr_82_SYM, molar_mass_t(kilograms_per_mol = 81.913484d-3))
    type(isotope_t), parameter :: Kr_83 = isotope_t(Kr_83_SYM, molar_mass_t(kilograms_per_mol = 82.914136d-3))
    type(isotope_t), parameter :: Kr_84 = isotope_t(Kr_84_SYM, molar_mass_t(kilograms_per_mol = 83.911507d-3))
    type(isotope_t), parameter :: Kr_85 = isotope_t(Kr_85_SYM, molar_mass_t(kilograms_per_mol = 84.9125273d-3)) ! [2]
    type(isotope_t), parameter :: Kr_86 = isotope_t(Kr_86_SYM, molar_mass_t(kilograms_per_mol = 85.9106107d-3))

    type(isotope_t), parameter :: Xe_124 = isotope_t(Xe_124_SYM, molar_mass_t(kilograms_per_mol = 123.905893d-3))
    type(isotope_t), parameter :: Xe_126 = isotope_t(Xe_126_SYM, molar_mass_t(kilograms_per_mol = 125.90427d-3))
    type(isotope_t), parameter :: Xe_128 = isotope_t(Xe_128_SYM, molar_mass_t(kilograms_per_mol = 127.903531d-3))
    type(isotope_t), parameter :: Xe_129 = isotope_t(Xe_129_SYM, molar_mass_t(kilograms_per_mol = 128.904779d-3))
    type(isotope_t), parameter :: Xe_130 = isotope_t(Xe_130_SYM, molar_mass_t(kilograms_per_mol = 129.903508d-3))
    type(isotope_t), parameter :: Xe_131 = isotope_t(Xe_131_SYM, molar_mass_t(kilograms_per_mol = 130.905082d-3))
    type(isotope_t), parameter :: Xe_132 = isotope_t(Xe_132_SYM, molar_mass_t(kilograms_per_mol = 131.904153d-3))
    type(isotope_t), parameter :: Xe_134 = isotope_t(Xe_134_SYM, molar_mass_t(kilograms_per_mol = 133.905394d-3))
    type(isotope_t), parameter :: Xe_136 = isotope_t(Xe_136_SYM, molar_mass_t(kilograms_per_mol = 135.90722d-3))
contains
    elemental function is(self, element)
        class(isotope_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        logical :: is

        is = self%symbol%is(element)
    end function

    function to_json_with_fraction(self, fraction) result(json)
        class(isotope_t), intent(in) :: self
        double precision, intent(in) :: fraction
        type(json_object_t) :: json

        json = json_object_t([ &
                json_member_unsafe("fraction", json_number_t(fraction)), &
                json_member_unsafe("isotope", json_string_unsafe(self%symbol%to_string())), &
                json_member_unsafe("atomic mass", json_string_unsafe(self%atomic_mass%to_string()))])
    end function

    elemental function isotope_to_string(self) result(string)
        class(isotope_t), intent(in) :: self
        type(varying_string) :: string

        string = self%symbol%to_string()
    end function

    pure function find_isotope(symbol, isotopes) result(position)
        type(isotope_symbol_t), intent(in) :: symbol
        type(isotope_t), intent(in) :: isotopes(:)
        integer :: position

        integer :: i

        position = 0
        do i = 1, size(isotopes)
            if (isotopes(i)%symbol == symbol) then
                position = i
                exit
            end if
        end do
    end function
end module
