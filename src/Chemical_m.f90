module Chemical_m
    use Chemical_component_m, only: ChemicalComponent_t
    use Chemical_symbol_m, only: ChemicalSymbol_t
    use Element_m, only: Element_t, combineByAtomFactorsUnsafe, find
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: ErrorList_t, MessageList_t, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use quaff, only: MolarMass_t, sum
    use strff, only: join
    use Utilities_m, only: MISMATCH_TYPE

    implicit none
    private

    type, public :: Chemical_t
        private
        type(ChemicalSymbol_t) :: symbol
        type(ChemicalComponent_t), allocatable :: components(:)
    contains
        private
        procedure :: atomFractionElement
        generic, public :: atomFraction => atomFractionElement
        procedure :: weightFractionElement
        generic, public :: weightFraction => weightFractionElement
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
            chemical%symbol = symbol
            call combineDuplicates(components, chemical%components)
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

    elemental function atomFractionElement(self, element) result(atom_fraction)
        class(Chemical_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        double precision :: atom_fraction

        integer :: position

        position = find(element, self%components%element)
        if (position > 0) then
            atom_fraction = &
                    self%components(position)%multiplier &
                    / sum(self%components%multiplier)
        else
            atom_fraction = 0.0d0
        end if
    end function atomFractionElement

    elemental function weightFractionElement(self, element) result(weight_fraction)
        class(Chemical_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        double precision :: weight_fraction

        type(MolarMass_t) :: masses(size(self%components))
        integer :: position

        position = find(element, self%components%element)
        if (position > 0) then
            masses = &
                    (self%components%multiplier / sum(self%components%multiplier)) &
                    * self%components%element%atomicMass()
            weight_fraction = masses(position) / sum(masses)
        else
            weight_fraction = 0.0d0
        end if
    end function weightFractionElement

    pure subroutine combineDuplicates(inputs, combined)
        type(ChemicalComponent_t), intent(in) :: inputs(:)
        type(ChemicalComponent_t), allocatable, intent(out) :: combined(:)

        integer :: duplicate_position
        integer :: i
        integer :: new_num_components
        double precision :: normalizer
        integer :: num_inputs
        integer :: prev_num_components
        type(Element_t) :: working_element
        type(ChemicalComponent_t), allocatable :: working_components(:)

        num_inputs = size(inputs)
        allocate(combined(1))
        combined(1) = inputs(1)
        do i = 2, num_inputs
            duplicate_position = find(inputs(i)%element%symbol, combined%element)
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
                normalizer = inputs(i)%multiplier + combined(duplicate_position)%multiplier
                working_element = combined(duplicate_position)%element
                call combineByAtomFactorsUnsafe( &
                        working_element, &
                        combined(duplicate_position)%multiplier / normalizer, &
                        inputs(i)%element, &
                        inputs(i)%multiplier / normalizer, &
                        combined(duplicate_position)%element)
                combined(duplicate_position)%multiplier = &
                        combined(duplicate_position)%multiplier + inputs(i)%multiplier
            end if
        end do
    end subroutine combineDuplicates
end module Chemical_m
