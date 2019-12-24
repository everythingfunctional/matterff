module Element_m
    use Element_component_m, only: ElementComponent_t
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: ErrorList_t, MessageList_t, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: Isotope_t, find
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use quaff, only: MolarMass_t, sum
    use strff, only: join
    use Utilities_m, only: INVALID_ARGUMENT, MISMATCH_TYPE

    implicit none
    private

    type, public :: Element_t
        private
        integer :: num_components = 0
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
                element%num_components = size(components)
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

        integer :: position

        if (self%num_components > 0) then
            position = find(isotope, self%components%isotope)
            if (position > 0) then
                atom_fraction = self%components(position)%fraction
            else
                atom_fraction = 0.0d0
            end if
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

        type(MolarMass_t) :: masses(self%num_components)
        integer :: position

        if (self%num_components > 0) then
            position = find(isotope, self%components%isotope)
            if (position > 0) then
                masses = self%components%fraction * self%components%isotope%atomic_mass
                weight_fraction = masses(position) / sum(masses)
            else
                weight_fraction = 0.0d0
            end if
        else
            weight_fraction = 0.0d0
        end if
    end function weightFractionFromSymbol
end module Element_m
