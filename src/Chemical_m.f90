module Chemical_m
    use Chemical_component_m, only: ChemicalComponent_t
    use Chemical_symbol_m, only: ChemicalSymbol_t
    use erloff, only: ErrorList_t, MessageList_t, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use strff, only: join
    use Utilities_m, only: MISMATCH_TYPE

    implicit none
    private

    type, public :: Chemical_t
        private
        type(ChemicalSymbol_t) :: symbol
        type(ChemicalComponent_t), allocatable :: components(:)
    end type Chemical_t

    character(len=*), parameter :: MODULE_NAME = "Chemical_m"

    public :: makeChemical
contains
    pure subroutine makeChemical(symbol, components, messages, errors, chemical)
        type(ChemicalSymbol_t), intent(in) :: symbol
        type(ChemicalComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Chemical_t), intent(out) :: chemical

        character(len=*), parameter :: PROCEDURE_NAME = "makeChemical"

        if (all(symbol%includes(components%element%symbol))) then
        else
            call errors%appendError(Internal( &
                    MISMATCH_TYPE, &
                    Module_(MODULE_NAME), &
                    Procedure_(PROCEDURE_NAME), &
                    "Attempted to create a chemical with an element it doesn't" &
                    // " contain. Chemical: " // symbol%toString() &
                    // ", Elements: [" // join(components%element%symbol%toString(), ", ") // "]"))
        end if
    end subroutine makeChemical
end module Chemical_m
