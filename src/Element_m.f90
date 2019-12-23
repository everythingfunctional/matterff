module Element_m
    use Element_component_m, only: ElementComponent_t
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: ErrorList_t, MessageList_t, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use strff, only: join
    use Utilities_m, only: MISMATCH_TYPE

    implicit none
    private

    type, public :: Element_t
        private
        type(ElementSymbol_t) :: symbol
        type(ElementComponent_t), allocatable :: components(:)
    contains
        private
        procedure, public :: atomFraction
        procedure, public :: weightFraction
    end type Element_t

    character(len=*), parameter :: MODULE_NAME = "Element_m"

    public :: fromAtomFractions
contains
    pure subroutine fromAtomFractions(symbol, components, messages, errors, element)
        type(ElementSymbol_t), intent(in) :: symbol
        type(ElementComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Element_t), intent(out) :: element

        character(len=*), parameter :: PROCEDURE_NAME = "fromAtomFractions"

        if (all(components%isotope%is(symbol))) then
            element%symbol = symbol
        else
            call errors%appendError(Internal( &
                    MISMATCH_TYPE, &
                    Module_(MODULE_NAME), &
                    Procedure_(PROCEDURE_NAME), &
                    "Attempted to create an element with an isotope not of that element." &
                    // " Element: " // symbol%toString() // ", Isotopes: [" &
                    // join(components%isotope%toString(), ", ") // "]"))
        end if
    end subroutine fromAtomFractions

    elemental function atomFraction(self, isotope)
        class(Element_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: atomFraction

        associate(a => self, b => isotope)
        end associate

        atomFraction = 0.0d0
    end function atomFraction

    elemental function weightFraction(self, isotope)
        class(Element_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: weightFraction

        associate(a => self, b => isotope)
        end associate

        weightFraction = 0.0d0
    end function weightFraction
end module Element_m
