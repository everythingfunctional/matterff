module Element_m
    use Element_component_m, only: ElementComponent_t, ElementComponent
    use Element_symbol_m, only: ElementSymbol_t, H, He, Li, Be, B, C, N, O
    use erloff, only: &
            ErrorList_t, MessageList_t, Info, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: &
            Isotope_t, &
            find, &
            H_1, &
            H_2, &
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
            O_18
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use quaff, only: Amount_t, Mass_t, MolarMass_t, operator(/), sum
    use strff, only: join
    use Utilities_m, only: &
            operator(.sumsTo.), &
            INVALID_ARGUMENT_TYPE, &
            MISMATCH_TYPE, &
            NORMALIZED_FRACTIONS_TYPE

    implicit none
    private

    ! It is assumed throughout this module that an Element will never be
    ! uninitialized or empty. Any attempt to create an empty element or use an
    ! element that has not been intialized will likely result in a memory error
    type, public :: Element_t
        private
        type(ElementSymbol_t), public :: symbol
        type(ElementComponent_t), allocatable :: components(:)
    contains
        private
        procedure :: atomFractionIsotope
        procedure :: atomFractionSymbol
        generic, public :: atomFraction => &
                atomFractionIsotope, atomFractionSymbol
        procedure, public :: atomicMass
        procedure :: weightFractionIsotope
        procedure :: weightFractionSymbol
        generic, public :: weightFraction => &
                weightFractionIsotope, weightFractionSymbol
    end type Element_t

    interface combineByAtomFactors
        module procedure combineElementsByAtomFactors
    end interface combineByAtomFactors

    interface combineByAtomFactorsUnsafe
        module procedure combineElementsByAtomFactorsUnsafe
    end interface combineByAtomFactorsUnsafe

    interface combineByWeightFactors
        module procedure combineElementsByWeightFactors
    end interface combineByWeightFactors

    interface combineByWeightFactorsUnsafe
        module procedure combineElementsByWeightFactorsUnsafe
    end interface combineByWeightFactorsUnsafe

    interface find
        module procedure findElement
    end interface find

    interface fromAtomFractions
        module procedure elementFromAtomFractions
    end interface fromAtomFractions

    interface fromAtomFractionsUnsafe
        module procedure elementFromAtomFractionsUnsafe
    end interface fromAtomFractionsUnsafe

    interface fromWeightFractions
        module procedure elementFromWeightFractions
    end interface fromWeightFractions

    interface fromWeightFractionsUnsafe
        module procedure elementFromWeightFractionsUnsafe
    end interface fromWeightFractionsUnsafe

    character(len=*), parameter :: MODULE_NAME = "Element_m"

    public :: &
            combineByAtomFactors, &
            combineByAtomFactorsUnsafe, &
            combineByWeightFactors, &
            combineByWeightFactorsUnsafe, &
            find, &
            fromAtomFractions, &
            fromAtomFractionsUnsafe, &
            fromWeightFractions, &
            fromWeightFractionsUnsafe, &
            naturalHydrogen, &
            naturalHelium, &
            naturalLithium, &
            naturalBeryllium, &
            naturalBoron, &
            naturalCarbon, &
            naturalNitrogen, &
            naturalOxygen
contains
    pure subroutine combineElementsByAtomFactors( &
            element1, factor1, element2, factor2, messages, errors, combined)
        type(Element_t), intent(in) :: element1
        double precision, intent(in) :: factor1
        type(Element_t), intent(in) :: element2
        double precision, intent(in) :: factor2
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Element_t), intent(out) :: combined

        character(len=*), parameter :: PROCEDURE_NAME = "combineElementsByAtomFactors"
        type(ErrorList_t) :: errors_
        type(MessageList_t) :: messages_

        call fromAtomFractions( &
                element1%symbol, &
                ElementComponent( &
                        [element1%components%isotope, element2%components%isotope], &
                        [element1%components%fraction * factor1, element2%components%fraction * factor2]), &
                messages_, &
                errors_, &
                combined)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        call errors%appendErrors( &
                errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
    end subroutine combineElementsByAtomFactors

    pure subroutine combineElementsByAtomFactorsUnsafe( &
            element1, factor1, element2, factor2, combined)
        type(Element_t), intent(in) :: element1
        double precision, intent(in) :: factor1
        type(Element_t), intent(in) :: element2
        double precision, intent(in) :: factor2
        type(Element_t), intent(out) :: combined

        call fromAtomFractionsUnsafe( &
                element1%symbol, &
                ElementComponent( &
                        [element1%components%isotope, element2%components%isotope], &
                        [element1%components%fraction * factor1, element2%components%fraction * factor2]), &
                combined)
    end subroutine combineElementsByAtomFactorsUnsafe

    pure subroutine combineElementsByWeightFactors( &
            element1, factor1, element2, factor2, messages, errors, combined)
        type(Element_t), intent(in) :: element1
        double precision, intent(in) :: factor1
        type(Element_t), intent(in) :: element2
        double precision, intent(in) :: factor2
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Element_t), intent(out) :: combined

        character(len=*), parameter :: PROCEDURE_NAME = "combineElementsByWeightFactors"
        type(ErrorList_t) :: errors_
        type(MessageList_t) :: messages_

        call fromWeightFractions( &
                element1%symbol, &
                ElementComponent( &
                        [element1%components%isotope, element2%components%isotope], &
                        [element1%weightFraction(element1%components%isotope%symbol) * factor1, &
                        element2%weightFraction(element2%components%isotope%symbol) * factor2]), &
                messages_, &
                errors_, &
                combined)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        call errors%appendErrors( &
                errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
    end subroutine combineElementsByWeightFactors

    pure subroutine combineElementsByWeightFactorsUnsafe( &
            element1, factor1, element2, factor2, combined)
        type(Element_t), intent(in) :: element1
        double precision, intent(in) :: factor1
        type(Element_t), intent(in) :: element2
        double precision, intent(in) :: factor2
        type(Element_t), intent(out) :: combined

        call fromWeightFractionsUnsafe( &
                element1%symbol, &
                ElementComponent( &
                        [element1%components%isotope, element2%components%isotope], &
                        [element1%weightFraction(element1%components%isotope%symbol) * factor1, &
                        element2%weightFraction(element2%components%isotope%symbol) * factor2]), &
                combined)
    end subroutine combineElementsByWeightFactorsUnsafe

    pure subroutine elementFromAtomFractions( &
            symbol, components, messages, errors, element)
        type(ElementSymbol_t), intent(in) :: symbol
        type(ElementComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Element_t), intent(out) :: element

        character(len=*), parameter :: PROCEDURE_NAME = "elementFromAtomFractions"
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
    end subroutine elementFromAtomFractions

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

    pure subroutine elementFromAtomFractionsUnsafe(symbol, components, element)
        type(ElementSymbol_t), intent(in) :: symbol
        type(ElementComponent_t), intent(in) :: components(:)
        type(Element_t), intent(out) :: element

        element%symbol = symbol
        call combineDuplicates(components, element%components)
    end subroutine elementFromAtomFractionsUnsafe

    pure subroutine elementFromWeightFractions( &
            symbol, components, messages, errors, element)
        type(ElementSymbol_t), intent(in) :: symbol
        type(ElementComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Element_t), intent(out) :: element

        character(len=*), parameter :: PROCEDURE_NAME = "elementFromWeightFractions"
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
            call fromWeightFractionsUnsafe(symbol, fixed_components, element)
        end if
    end subroutine elementFromWeightFractions

    pure subroutine elementFromWeightFractionsUnsafe(symbol, components, element)
        type(ElementSymbol_t), intent(in) :: symbol
        type(ElementComponent_t), intent(in) :: components(:)
        type(Element_t), intent(out) :: element

        type(Mass_t), parameter :: ONE_KILOGRAM = Mass_t(kilograms = 1.0d0)
        type(Amount_t) :: amounts(size(components))

        amounts = components%fraction * ONE_KILOGRAM / components%isotope%atomic_mass
        call fromAtomFractionsUnsafe( &
                symbol, &
                ElementComponent(components%isotope, amounts / sum(amounts)), &
                element)
    end subroutine elementFromWeightFractionsUnsafe

    ! Atomic fractions are taken from the 17th Edition of the Chart of Nuclides
    pure function naturalHydrogen()
        type(Element_t) :: naturalHydrogen

        naturalHydrogen%symbol = H
        allocate(naturalHydrogen%components, source = &
                [ElementComponent(H_1, 0.999885d0), &
                 ElementComponent(H_2, 0.000115d0)])
    end function naturalHydrogen

    pure function naturalHelium()
        type(Element_t) :: naturalHelium

        naturalHelium%symbol = He
        allocate(naturalHelium%components, source = &
                [ElementComponent(He_3, 0.000001344d0), &
                 ElementComponent(He_4, 0.99999866d0)])
    end function naturalHelium

    pure function naturalLithium()
        type(Element_t) :: naturalLithium

        naturalLithium%symbol = Li
        allocate(naturalLithium%components, source = &
                [ElementComponent(Li_6, 0.0759d0), &
                 ElementComponent(Li_7, 0.9241d0)])
    end function naturalLithium

    pure function naturalBeryllium()
        type(Element_t) :: naturalBeryllium

        naturalBeryllium%symbol = Be
        allocate(naturalBeryllium%components, source = &
                [ElementComponent(Be_9, 1.0d0)])
    end function naturalBeryllium

    pure function naturalBoron()
        type(Element_t) :: naturalBoron

        naturalBoron%symbol = B
        allocate(naturalBoron%components, source = &
                [ElementComponent(B_10, 0.199d0), &
                 ElementComponent(B_11, 0.801d0)])
    end function naturalBoron

    pure function naturalCarbon()
        type(Element_t) :: naturalCarbon

        naturalCarbon%symbol = C
        allocate(naturalCarbon%components, source = &
                [ElementComponent(C_12, 0.9893d0), &
                 ElementComponent(C_13, 0.0107d0)])
    end function naturalCarbon

    pure function naturalNitrogen()
        type(Element_t) :: naturalNitrogen

        naturalNitrogen%symbol = N
        allocate(naturalNitrogen%components, source = &
                [ElementComponent(N_14, 0.99636d0), &
                 ElementComponent(N_15, 0.00364d0)])
    end function naturalNitrogen

    pure function naturalOxygen()
        type(Element_t) :: naturalOxygen

        naturalOxygen%symbol = O
        allocate(naturalOxygen%components, source = &
                [ElementComponent(O_16, 0.99757d0), &
                 ElementComponent(O_17, 0.00038d0), &
                 ElementComponent(O_18, 0.00205d0)])
    end function naturalOxygen

    elemental function atomFractionIsotope(self, isotope) result(atom_fraction)
        class(Element_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = self%atomFraction(isotope%symbol)
    end function atomFractionIsotope

    elemental function atomFractionSymbol(self, isotope) result(atom_fraction)
        class(Element_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: atom_fraction

        integer :: position

        position = find(isotope, self%components%isotope)
        if (position > 0) then
            atom_fraction = self%components(position)%fraction
        else
            atom_fraction = 0.0d0
        end if
    end function atomFractionSymbol

    elemental function atomicMass(self)
        class(Element_t), intent(in) :: self
        type(MolarMass_t) :: atomicMass

        atomicMass = sum(self%components%fraction * self%components%isotope%atomic_mass)
    end function atomicMass

    elemental function weightFractionIsotope(self, isotope) result(weight_fraction)
        class(Element_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = self%weightFraction(isotope%symbol)
    end function weightFractionIsotope

    elemental function weightFractionSymbol(self, isotope) result(weight_fraction)
        class(Element_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: weight_fraction

        type(MolarMass_t) :: masses(size(self%components))
        integer :: position

        position = find(isotope, self%components%isotope)
        if (position > 0) then
            masses = self%components%fraction * self%components%isotope%atomic_mass
            weight_fraction = masses(position) / sum(masses)
        else
            weight_fraction = 0.0d0
        end if
    end function weightFractionSymbol

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

    pure function findElement(symbol, elements) result(position)
        type(ElementSymbol_t), intent(in) :: symbol
        type(Element_t), intent(in) :: elements(:)
        integer :: position

        integer :: i

        position = 0
        do i = 1, size(elements)
            if (elements(i)%symbol == symbol) then
                position = i
                exit
            end if
        end do
    end function findElement
end module Element_m
