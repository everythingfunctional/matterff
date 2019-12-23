module Element_m
    use Element_component_m, only: ElementComponent_t
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: ErrorList_t, MessageList_t, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: Isotope_t
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use strff, only: join
    use Utilities_m, only: INVALID_ARGUMENT, MISMATCH_TYPE

    implicit none
    private

    type, public :: Element_t
        private
        type(ElementSymbol_t) :: symbol
        type(ElementComponent_t), allocatable :: components(:)
    contains
        private
        procedure :: atomFractionFromIsotope
        procedure :: atomFractionFromSymbol
        generic, public :: atomFraction => &
                atomFractionFromIsotope, atomFractionFromSymbol
        procedure :: weightFractionFromIsotope
        procedure :: weightFractionFromSymbol
        generic, public :: weightFraction => &
                weightFractionFromIsotope, weightFractionFromSymbol
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
            if (all(components%fraction > 0.0d0)) then
                element%symbol = symbol
                allocate(element%components, source = components)
            else
                call errors%appendError(Internal( &
                        INVALID_ARGUMENT, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "All fractions must be greater than 0."))
            end if
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

    elemental function atomFractionFromIsotope(self, isotope) result(atom_fraction)
        class(Element_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = self%atomFraction(isotope%symbol)
    end function atomFractionFromIsotope

    elemental function atomFractionFromSymbol(self, isotope) result(atom_fraction)
        class(Element_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: atom_fraction

        associate(a => isotope)
        end associate

        if (allocated(self%components)) then
            atom_fraction = self%components(1)%fraction
        else
            atom_fraction = 0.0d0
        end if
    end function atomFractionFromSymbol

    elemental function weightFractionFromIsotope(self, isotope) result(weight_fraction)
        class(Element_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = self%weightFraction(isotope%symbol)
    end function weightFractionFromIsotope

    elemental function weightFractionFromSymbol(self, isotope) result(weight_fraction)
        class(Element_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: weight_fraction

        associate(a => isotope)
        end associate

        if (allocated(self%components)) then
            weight_fraction = self%components(1)%fraction
        else
            weight_fraction = 0.0d0
        end if
    end function weightFractionFromSymbol
end module Element_m
