module Utilities_m
    use erloff, only: MessageType_t

    implicit none
    private

    type(MessageType_t), parameter, public :: MISMATCH_TYPE = MessageType_t("Mismatch")
end module Utilities_m
