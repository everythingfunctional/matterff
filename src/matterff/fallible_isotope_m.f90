module matterff_fallible_isotope_m
    use erloff, only: error_list_t, fatal_t, module_t, procedure_t, UNKNOWN_TYPE
    use iso_varying_string, only: varying_string, operator(//), char
    use jsonff, only: fallible_json_value_t, json_object_t, json_string_t
    use matterff_isotope_m, only: &
            isotope_t, &
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
    use matterff_utilities_m, only: INVALID_ARGUMENT
    use strff, only: to_string

    implicit none
    private
    public :: fallible_isotope_t

    type :: fallible_isotope_t
        private
        type(isotope_t) :: isotope_
        type(error_list_t) :: errors_
    contains
        private
        procedure, public :: failed
        procedure, public :: isotope
        procedure, public :: errors
    end type

    interface fallible_isotope_t
        module procedure from_json
        module procedure from_string
        module procedure from_character
        module procedure from_element_symbol_and_mass_number
        module procedure from_fallible_isotope
    end interface

    character(len=*), parameter :: MODULE_NAME = "matterff_fallible_isotope_m"
contains
    function from_json(json) result(new_fallible_isotope)
        type(json_object_t), intent(in) :: json
        type(fallible_isotope_t) :: new_fallible_isotope

        character(len=*), parameter :: PROCEDURE_NAME = "from_json"
        type(fallible_json_value_t) :: maybe_isotope_string

        maybe_isotope_string = json%get_element("isotope")
        if (maybe_isotope_string%failed()) then
            new_fallible_isotope%errors_ = error_list_t( &
                    maybe_isotope_string%errors(), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
        else
            select type (isotope_string => maybe_isotope_string%value_())
            type is (json_string_t)
                new_fallible_isotope = fallible_isotope_t( &
                        fallible_isotope_t(isotope_string%get_value()), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            class default
                new_fallible_isotope%errors_ = error_list_t(fatal_t( &
                        UNKNOWN_TYPE, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Expected to get a json_string_t but got" // isotope_string%to_compact_string()))
            end select
        end if
    end function

    function from_string(string) result(new_fallible_isotope)
        type(varying_string), intent(in) :: string
        type(fallible_isotope_t) :: new_fallible_isotope

        new_fallible_isotope = fallible_isotope_t( &
                fallible_isotope_t(char(string)), &
                module_t(MODULE_NAME), &
                procedure_t("from_string"))
    end function

    function from_character(string) result(new_fallible_isotope)
        character(len=*), intent(in) :: string
        type(fallible_isotope_t) :: new_fallible_isotope

        character(len=*), parameter :: PROCEDURE_NAME = "from_character"
        character(len=2) :: element_symbol
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
                new_fallible_isotope = fallible_isotope_t(&
                        fallible_isotope_t(element_symbol, mass_number), &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME))
            else
                new_fallible_isotope%errors_ = error_list_t(fatal_t( &
                        INVALID_ARGUMENT, &
                        module_t(MODULE_NAME), &
                        procedure_t(PROCEDURE_NAME), &
                        "Unable to read mass number in: " // string))
            end if
        else
            new_fallible_isotope%errors_ = error_list_t(fatal_t( &
                    INVALID_ARGUMENT, &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    "No '-' between element and mass number: " // string))
        end if
    end function

    function from_element_symbol_and_mass_number( &
            element_symbol, mass_number) result(new_fallible_isotope)
        character(len=2), intent(in) :: element_symbol
        integer, intent(in) :: mass_number
        type(fallible_isotope_t) :: new_fallible_isotope

        select case (element_symbol)
        case ("H ")
            select case (mass_number)
            case (1)
                new_fallible_isotope%isotope_ = H_1
            case (2)
                new_fallible_isotope%isotope_ = H_2
            case (3)
                new_fallible_isotope%isotope_ = H_3
            case default
                goto 99
            end select
        case ("He")
            select case (mass_number)
            case (3)
                new_fallible_isotope%isotope_ = He_3
            case (4)
                new_fallible_isotope%isotope_ = He_4
            case default
                goto 99
            end select
        case ("Li")
            select case (mass_number)
            case (6)
                new_fallible_isotope%isotope_ = Li_6
            case (7)
                new_fallible_isotope%isotope_ = Li_7
            case default
                goto 99
            end select
        case ("Be")
            select case (mass_number)
            case (9)
                new_fallible_isotope%isotope_ = Be_9
            case default
                goto 99
            end select
        case ("B ")
            select case (mass_number)
            case (10)
                new_fallible_isotope%isotope_ = B_10
            case (11)
                new_fallible_isotope%isotope_ = B_11
            case default
                goto 99
            end select
        case ("C ")
            select case (mass_number)
            case (12)
                new_fallible_isotope%isotope_ = C_12
            case (13)
                new_fallible_isotope%isotope_ = C_13
            case default
                goto 99
            end select
        case ("N ")
            select case (mass_number)
            case (14)
                new_fallible_isotope%isotope_ = N_14
            case (15)
                new_fallible_isotope%isotope_ = N_15
            case default
                goto 99
            end select
        case ("O ")
            select case (mass_number)
            case (16)
                new_fallible_isotope%isotope_ = O_16
            case (17)
                new_fallible_isotope%isotope_ = O_17
            case (18)
                new_fallible_isotope%isotope_ = O_18
            case default
                goto 99
            end select
        case ("Ar")
            select case (mass_number)
            case (36)
                new_fallible_isotope%isotope_ = Ar_36
            case (38)
                new_fallible_isotope%isotope_ = Ar_38
            case (40)
                new_fallible_isotope%isotope_ = Ar_40
            case default
                goto 99
            end select
        case ("Kr")
            select case (mass_number)
            case (78)
                new_fallible_isotope%isotope_ = Kr_78
            case (80)
                new_fallible_isotope%isotope_ = Kr_80
            case (82)
                new_fallible_isotope%isotope_ = Kr_82
            case (83)
                new_fallible_isotope%isotope_ = Kr_83
            case (84)
                new_fallible_isotope%isotope_ = Kr_84
            case (86)
                new_fallible_isotope%isotope_ = Kr_86
            case default
                goto 99
            end select
        case ("Xe")
            select case (mass_number)
            case (124)
                new_fallible_isotope%isotope_ = Xe_124
            case (126)
                new_fallible_isotope%isotope_ = Xe_126
            case (128)
                new_fallible_isotope%isotope_ = Xe_128
            case (129)
                new_fallible_isotope%isotope_ = Xe_129
            case (130)
                new_fallible_isotope%isotope_ = Xe_130
            case (131)
                new_fallible_isotope%isotope_ = Xe_131
            case (132)
                new_fallible_isotope%isotope_ = Xe_132
            case (134)
                new_fallible_isotope%isotope_ = Xe_134
            case (136)
                new_fallible_isotope%isotope_ = Xe_136
            case default
                goto 99
            end select
        case default
            goto 99
        end select
        return
        99 new_fallible_isotope%errors_ = error_list_t(fatal_t( &
                INVALID_ARGUMENT, &
                module_t(MODULE_NAME), &
                procedure_t("from_element_symbol_and_mass_number"), &
                "Unknown Isotope: " // trim(element_symbol) // "-" // to_string(mass_number)))
    end function

    function from_fallible_isotope( &
            fallible_isotope, module_, procedure_) result(new_fallible_isotope)
        type(fallible_isotope_t), intent(in) :: fallible_isotope
        type(module_t), intent(in) :: module_
        type(procedure_t), intent(in) :: procedure_
        type(fallible_isotope_t) :: new_fallible_isotope

        if (fallible_isotope%failed()) then
            new_fallible_isotope%errors_ = error_list_t( &
                    fallible_isotope%errors_, module_, procedure_)
        else
            new_fallible_isotope%isotope_ = fallible_isotope%isotope_
        end if
    end function

    pure function failed(self)
        class(fallible_isotope_t), intent(in) :: self
        logical :: failed

        failed = self%errors_%has_any()
    end function

    pure function isotope(self)
        class(fallible_isotope_t), intent(in) :: self
        type(isotope_t) :: isotope

        isotope = self%isotope_
    end function

    function errors(self)
        class(fallible_isotope_t), intent(in) :: self
        type(error_list_t) :: errors

        errors = self%errors_
    end function
end module
