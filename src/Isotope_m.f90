module Isotope_m
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: ErrorList_t, Fatal, Module_, Procedure_, UNKNOWN_TYPE_TYPE
    use iso_varying_string, only: VARYING_STRING, operator(//), char
    use Isotope_symbol_m, only: &
            IsotopeSymbol_t, &
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
            JsonElement_t, &
            JsonMember_t, &
            JsonObject_t, &
            JsonString_t, &
            jsonMemberUnsafe, &
            jsonNumber, &
            jsonObject, &
            jsonStringUnsafe
    use quaff, only: MolarMass_t
    use strff, only: toString
    use Utilities_m, only: INVALID_ARGUMENT_TYPE

    implicit none
    private

    type, public :: Isotope_t
        private
        type(IsotopeSymbol_t), public :: symbol
        type(MolarMass_t), public :: atomic_mass
    contains
        private
        procedure, public :: is
        procedure, public :: toJsonWithFraction
        procedure, public :: toString => isotopeToString
    end type Isotope_t

    interface find
        module procedure findIsotope
    end interface find

    interface fromJson
        module procedure isotopeFromJson
    end interface fromJson

    interface fromString
        module procedure fromStringC
        module procedure fromStringS
    end interface fromString

    ! Atomic masses are taken from the 17th Edition of the Chart of Nuclides
    ! Where atomic mass is not provided for an isotope in the table, the
    ! atomic mass is taken from:
    ! [1] https://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl
    type(Isotope_t), parameter, public :: H_1 = Isotope_t(H_1_SYM, MolarMass_t(kilograms_per_mol = 1.0078250321d-3))
    type(Isotope_t), parameter, public :: H_2 = Isotope_t(H_2_SYM, MolarMass_t(kilograms_per_mol = 2.0141017779d-3))
    type(Isotope_t), parameter, public :: H_3 = Isotope_t(H_3_SYM, MolarMass_t(kilograms_per_mol = 3.0160492779d-3)) ! [1]

    type(Isotope_t), parameter, public :: He_3 = Isotope_t(He_3_SYM, MolarMass_t(kilograms_per_mol = 3.016029319d-3))
    type(Isotope_t), parameter, public :: He_4 = Isotope_t(He_4_SYM, MolarMass_t(kilograms_per_mol = 4.0026032542d-3))

    type(Isotope_t), parameter, public :: Li_6 = Isotope_t(Li_6_SYM, MolarMass_t(kilograms_per_mol = 6.01512279d-3))
    type(Isotope_t), parameter, public :: Li_7 = Isotope_t(Li_7_SYM, MolarMass_t(kilograms_per_mol = 7.0160045d-3))

    type(Isotope_t), parameter, public :: Be_9 = Isotope_t(Be_9_SYM, MolarMass_t(kilograms_per_mol = 9.0121822d-3))

    type(Isotope_t), parameter, public :: B_10 = Isotope_t(B_10_SYM, MolarMass_t(kilograms_per_mol = 10.012937d-3))
    type(Isotope_t), parameter, public :: B_11 = Isotope_t(B_11_SYM, MolarMass_t(kilograms_per_mol = 11.0090354d-3))

    type(Isotope_t), parameter, public :: C_12 = Isotope_t(C_12_SYM, MolarMass_t(kilograms_per_mol = 12.0d-3))
    type(Isotope_t), parameter, public :: C_13 = Isotope_t(C_13_SYM, MolarMass_t(kilograms_per_mol = 13.003354838d-3))

    type(Isotope_t), parameter, public :: N_14 = Isotope_t(N_14_SYM, MolarMass_t(kilograms_per_mol = 14.003074005d-3))
    type(Isotope_t), parameter, public :: N_15 = Isotope_t(N_15_SYM, MolarMass_t(kilograms_per_mol = 15.000108898d-3))

    type(Isotope_t), parameter, public :: O_16 = Isotope_t(O_16_SYM, MolarMass_t(kilograms_per_mol = 15.9949146196d-3))
    type(Isotope_t), parameter, public :: O_17 = Isotope_t(O_17_SYM, MolarMass_t(kilograms_per_mol = 16.9991317d-3))
    type(Isotope_t), parameter, public :: O_18 = Isotope_t(O_18_SYM, MolarMass_t(kilograms_per_mol = 17.999161d-3))

    type(Isotope_t), parameter, public :: Ar_36 = Isotope_t(Ar_36_SYM, MolarMass_t(kilograms_per_mol = 35.96754511d-3))
    type(Isotope_t), parameter, public :: Ar_38 = Isotope_t(Ar_38_SYM, MolarMass_t(kilograms_per_mol = 37.9627324d-3))
    type(Isotope_t), parameter, public :: Ar_40 = Isotope_t(Ar_40_SYM, MolarMass_t(kilograms_per_mol = 39.962383123d-3))

    type(Isotope_t), parameter, public :: Xe_124 = Isotope_t(Xe_124_SYM, MolarMass_t(kilograms_per_mol = 123.905893d-3))
    type(Isotope_t), parameter, public :: Xe_126 = Isotope_t(Xe_126_SYM, MolarMass_t(kilograms_per_mol = 125.90427d-3))
    type(Isotope_t), parameter, public :: Xe_128 = Isotope_t(Xe_128_SYM, MolarMass_t(kilograms_per_mol = 127.903531d-3))
    type(Isotope_t), parameter, public :: Xe_129 = Isotope_t(Xe_129_SYM, MolarMass_t(kilograms_per_mol = 128.904779d-3))
    type(Isotope_t), parameter, public :: Xe_130 = Isotope_t(Xe_130_SYM, MolarMass_t(kilograms_per_mol = 129.903508d-3))
    type(Isotope_t), parameter, public :: Xe_131 = Isotope_t(Xe_131_SYM, MolarMass_t(kilograms_per_mol = 130.905082d-3))
    type(Isotope_t), parameter, public :: Xe_132 = Isotope_t(Xe_132_SYM, MolarMass_t(kilograms_per_mol = 131.904153d-3))
    type(Isotope_t), parameter, public :: Xe_134 = Isotope_t(Xe_134_SYM, MolarMass_t(kilograms_per_mol = 133.905394d-3))
    type(Isotope_t), parameter, public :: Xe_136 = Isotope_t(Xe_136_SYM, MolarMass_t(kilograms_per_mol = 135.90722d-3))

    character(len=*), parameter :: MODULE_NAME = "Isotope_m"

    public :: find, fromJson, fromString, getIsotope
contains
    pure subroutine getIsotope(element_symbol, mass_number, errors, isotope)
        character(len=2), intent(in) :: element_symbol
        integer, intent(in) :: mass_number
        type(ErrorList_t), intent(out) :: errors
        type(Isotope_t), intent(out) :: isotope

        character(len=*), parameter :: PROCEDURE_NAME = "getIsotope"

        select case (element_symbol)
        case ("H ")
            select case (mass_number)
            case (1)
                isotope = H_1
            case (2)
                isotope = H_2
            case (3)
                isotope = H_3
            case default
                goto 99
            end select
        case ("He")
            select case (mass_number)
            case (3)
                isotope = He_3
            case (4)
                isotope = He_4
            case default
                goto 99
            end select
        case ("Li")
            select case (mass_number)
            case (6)
                isotope = Li_6
            case (7)
                isotope = Li_7
            case default
                goto 99
            end select
        case ("Be")
            select case (mass_number)
            case (9)
                isotope = Be_9
            case default
                goto 99
            end select
        case ("B ")
            select case (mass_number)
            case (10)
                isotope = B_10
            case (11)
                isotope = B_11
            case default
                goto 99
            end select
        case ("C ")
            select case (mass_number)
            case (12)
                isotope = C_12
            case (13)
                isotope = C_13
            case default
                goto 99
            end select
        case ("N ")
            select case (mass_number)
            case (14)
                isotope = N_14
            case (15)
                isotope = N_15
            case default
                goto 99
            end select
        case ("O ")
            select case (mass_number)
            case (16)
                isotope = O_16
            case (17)
                isotope = O_17
            case (18)
                isotope = O_18
            case default
                goto 99
            end select
        case default
            goto 99
        end select
        return
        99 call errors%appendError(Fatal( &
                INVALID_ARGUMENT_TYPE, &
                Module_(MODULE_NAME), &
                Procedure_(PROCEDURE_NAME), &
                "Unknown Isotope: " // trim(element_symbol) // "-" // toString(mass_number)))
    end subroutine getIsotope

    pure subroutine isotopeFromJson(json, errors, isotope)
        type(JsonObject_t), intent(in) :: json
        type(ErrorList_t), intent(out) :: errors
        type(Isotope_t), intent(out) :: isotope

        character(len=*), parameter :: PROCEDURE_NAME = "isotopeFromJson"
        type(ErrorList_t) :: errors_
        type(JsonElement_t) :: isotope_element

        call json%getElement("isotope", errors_, isotope_element)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            select type (isotope_string => isotope_element%element)
            type is (JsonString_t)
                call fromString(isotope_string%getValue(), errors_, isotope)
                call errors%appendErrors( &
                        errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
            class default
                call errors%appendError(Fatal( &
                        UNKNOWN_TYPE_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "Expected to get a JsonString but got" // isotope_string%toCompactString()))
            end select
        end if
    end subroutine isotopeFromJson

    pure subroutine fromStringC(string, errors, isotope)
        character(len=*), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Isotope_t), intent(out) :: isotope

        character(len=*), parameter :: PROCEDURE_NAME = "fromStringC"
        character(len=2) :: element_symbol
        type(ErrorList_t) :: errors_
        integer :: hyphen_position
        integer :: mass_number
        character(len=3) :: mass_number_part
        integer :: status

        hyphen_position = index(string, "-")
        if (hyphen_position > 0) then
            element_symbol = string(1:hyphen_position-1)
            mass_number_part = string(hyphen_position+1:)
            read(mass_number_part, *, iostat=status) mass_number
            if (status == 0) then
                call getIsotope(element_symbol, mass_number, errors_, isotope)
                call errors%appendErrors( &
                        errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
            else
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "Unable to read mass number in: " // string))
            end if
        else
            call errors%appendError(Fatal( &
                    INVALID_ARGUMENT_TYPE, &
                    Module_(MODULE_NAME), &
                    Procedure_(PROCEDURE_NAME), &
                    "No '-' between element and mass number: " // string))
        end if
    end subroutine fromStringC

    pure subroutine fromStringS(string, errors, isotope)
        type(VARYING_STRING), intent(in) :: string
        type(ErrorList_t), intent(out) :: errors
        type(Isotope_t), intent(out) :: isotope

        type(ErrorList_t) :: errors_

        call fromString(char(string), errors_, isotope)
        call errors%appendErrors( &
                errors_, Module_(MODULE_NAME), Procedure_("fromStringS"))
    end subroutine fromStringS

    elemental function is(self, element)
        class(Isotope_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        logical :: is

        is = self%symbol%is(element)
    end function is

    pure function toJsonWithFraction(self, fraction) result(json)
        class(Isotope_t), intent(in) :: self
        double precision, intent(in) :: fraction
        type(JsonObject_t) :: json

        type(JsonMember_t) :: members(3)

        members(1) = jsonMemberUnsafe("fraction", jsonNumber(fraction))
        members(2) = jsonMemberUnsafe("isotope", jsonStringUnsafe(self%symbol%toString()))
        members(3) = jsonMemberUnsafe("atomicMass", jsonStringUnsafe(self%atomic_mass%toString()))
        json = jsonObject(members)
    end function toJsonWithFraction

    elemental function isotopeToString(self) result(string)
        class(Isotope_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = self%symbol%toString()
    end function isotopeToString

    pure function findIsotope(symbol, isotopes) result(position)
        type(IsotopeSymbol_t), intent(in) :: symbol
        type(Isotope_t), intent(in) :: isotopes(:)
        integer :: position

        integer :: i

        position = 0
        do i = 1, size(isotopes)
            if (isotopes(i)%symbol == symbol) then
                position = i
                exit
            end if
        end do
    end function findIsotope
end module Isotope_m
