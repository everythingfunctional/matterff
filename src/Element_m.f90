module Element_m
    use Element_component_m, only: ElementComponent_t, ElementComponent
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: &
            ErrorList_t, MessageList_t, Info, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: Isotope_t, find
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use quaff, only: MolarMass_t, sum
    use strff, only: join
    use Utilities_m, only: &
            operator(.sumsTo.), &
            INVALID_ARGUMENT_TYPE, &
            MISMATCH_TYPE, &
            NORMALIZED_FRACTIONS_TYPE

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

    public :: fromAtomFractions, fromAtomFractionsUnsafe
contains
    pure subroutine fromAtomFractions(symbol, components, messages, errors, element)
        type(ElementSymbol_t), intent(in) :: symbol
        type(ElementComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Element_t), intent(out) :: element

        character(len=*), parameter :: PROCEDURE_NAME = "fromAtomFractions"
        type(ErrorList_t) :: errors_
        type(ElementComponent_t) :: fixed_components(size(components))
        type(MessageList_t) :: messages_

        call errorChecking(symbol, components, messages_, errors_, fixed_components)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            call fromAtomFractionsUnsafe(symbol, fixed_components, element)
        end if
    end subroutine fromAtomFractions

    pure subroutine errorChecking(symbol, components, messages, errors, fixed_components)
        type(ElementSymbol_t), intent(in) :: symbol
        type(ElementComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(ElementComponent_t), intent(out) :: fixed_components(size(components))

        character(len=*), parameter :: PROCEDURE_NAME = "errorChecking"

        if (all(components%isotope%is(symbol))) then
            if (all(components%fraction > 0.0d0)) then
                if (components%fraction.sumsTo.1.0d0) then
                    fixed_components = components
                else
                    call messages%appendMessage(Info( &
                            NORMALIZED_FRACTIONS_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "Attempted to create composition with component" &
                            // " fractions that did not sum to 1.0."))
                    fixed_components = ElementComponent( &
                            components%isotope, &
                            components%fraction / sum(components%fraction))
                end if
            else
                call errors%appendError(Internal( &
                        INVALID_ARGUMENT_TYPE, &
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
    end subroutine errorChecking

    pure subroutine fromAtomFractionsUnsafe(symbol, components, element)
        type(ElementSymbol_t), intent(in) :: symbol
        type(ElementComponent_t), intent(in) :: components(:)
        type(Element_t), intent(out) :: element

        element%symbol = symbol
        call combineDuplicates(components, element%components)
        element%num_components = size(element%components)
    end subroutine fromAtomFractionsUnsafe

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

    pure subroutine combineDuplicates(inputs, combined)
        type(ElementComponent_t), intent(in) :: inputs(:)
        type(ElementComponent_t), allocatable, intent(out) :: combined(:)

        integer :: duplicate_position
        integer :: i
        integer :: new_num_components
        integer :: num_inputs
        integer :: prev_num_components
        type(ElementComponent_t), allocatable :: working_components(:)

        num_inputs = size(inputs)
        allocate(combined(1))
        combined(1) = inputs(1)
        do i = 2, num_inputs
            duplicate_position = find(inputs(i)%isotope%symbol, combined%isotope)
            if (duplicate_position == 0) then
                prev_num_components = size(combined)
                new_num_components = prev_num_components + 1
                allocate(working_components(prev_num_components))
                working_components(:) = combined(:)
                deallocate(combined)
                allocate(combined(new_num_components))
                combined(1:prev_num_components) = working_components(:)
                deallocate(working_components)
                combined(new_num_components) = inputs(i)
            else
                combined(duplicate_position)%fraction = &
                        combined(duplicate_position)%fraction + inputs(i)%fraction
            end if
        end do
    end subroutine combineDuplicates
end module Element_m
