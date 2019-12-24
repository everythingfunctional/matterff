module Utilities_m
    use erloff, only: MessageType_t
    use Miscellaneous_m, only: operator(.safeEq.)

    implicit none
    private

    interface operator(.sumsTo.)
        module procedure sumsTo
    end interface operator(.sumsTo.)

    type(MessageType_t), parameter, public :: INVALID_ARGUMENT = MessageType_t("Invalid Argument")
    type(MessageType_t), parameter, public :: MISMATCH_TYPE = MessageType_t("Mismatch")
    type(MessageType_t), parameter, public :: NORMALIZED_FRACTIONS = MessageType_t("Normalized Fractions")

    public :: operator(.sumsTo.)
contains
    pure function sumsTo(numbers, expected)
        double precision, intent(in) :: numbers(:)
        double precision, intent(in) :: expected
        logical :: sumsTo

        sumsTo = sum(numbers).safeEq.expected
    end function sumsTo
end module Utilities_m
