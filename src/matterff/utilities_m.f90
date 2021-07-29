module matterff_utilities_m
    use erloff, only: message_type_t
    use quaff_utilities_m, only: operator(.safeEq.)

    implicit none
    private
    public :: &
            operator(.sumsTo.), &
            INVALID_ARGUMENT, &
            MISMATCH, &
            NORMALIZED_FRACTIONS

    interface operator(.sumsTo.)
        module procedure sums_to
    end interface

    type(message_type_t), parameter :: INVALID_ARGUMENT = message_type_t("Invalid Argument")
    type(message_type_t), parameter :: MISMATCH = message_type_t("Mismatch")
    type(message_type_t), parameter :: NORMALIZED_FRACTIONS = message_type_t("Normalized Fractions")
contains
    pure function sums_to(numbers, expected)
        double precision, intent(in) :: numbers(:)
        double precision, intent(in) :: expected
        logical :: sums_to

        sums_to = sum(numbers).safeEq.expected
    end function
end module
