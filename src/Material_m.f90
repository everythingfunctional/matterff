module Material_m
    use Chemical_m, only: Chemical_t, combineByAtomFactorsUnsafe, find
    use Chemical_symbol_m, only: ChemicalSymbol_t
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: &
            ErrorList_t, MessageList_t, Info, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: Isotope_t
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use Material_component_m, only: MaterialComponent_t, MaterialComponent
    use quaff, only: Amount_t, Mass_t, MolarMass_t, operator(/), sum
    use strff, only: join
    use Utilities_m, only: &
            operator(.sumsTo.), &
            INVALID_ARGUMENT_TYPE, &
            MISMATCH_TYPE, &
            NORMALIZED_FRACTIONS_TYPE

    implicit none
    private

    ! It is assumed throughout this module that a Material will never be
    ! uninitialized or empty. Any attempt to create an empty material or use a
    ! material that has not been intialized will likely result in a memory error
    type, public :: Material_t
        private
        type(MaterialComponent_t), allocatable :: components(:)
    contains
        private
        procedure :: atomFractionChemical
        procedure :: atomFractionElement
        procedure :: atomFractionIsotope
        procedure :: atomFractionIsotopeSymbol
        generic, public :: atomFraction => &
                atomFractionChemical, &
                atomFractionElement, &
                atomFractionIsotope, &
                atomFractionIsotopeSymbol
        procedure, public :: molarMass
        procedure :: weightFractionChemical
        procedure :: weightFractionElement
        procedure :: weightFractionIsotope
        procedure :: weightFractionIsotopeSymbol
        generic, public :: weightFraction => &
                weightFractionChemical, &
                weightFractionElement, &
                weightFractionIsotope, &
                weightFractionIsotopeSymbol
    end type Material_t

    interface combineByAtomFactors
        module procedure combineMaterialsByAtomFactors
    end interface combineByAtomFactors

    interface combineByAtomFactorsUnsafe
        module procedure combineMaterialsByAtomFactorsUnsafe
    end interface combineByAtomFactorsUnsafe

    interface combineByWeightFactors
        module procedure combineMaterialsByWeightFactors
    end interface combineByWeightFactors

    interface combineByWeightFactorsUnsafe
        module procedure combineMaterialsByWeightFactorsUnsafe
    end interface combineByWeightFactorsUnsafe

    interface fromAtomFractions
        module procedure materialFromAtomFractions
    end interface fromAtomFractions

    interface fromAtomFractionsUnsafe
        module procedure materialFromAtomFractionsUnsafe
    end interface fromAtomFractionsUnsafe

    interface fromWeightFractions
        module procedure materialFromWeightFractions
    end interface fromWeightFractions

    interface fromWeightFractionsUnsafe
        module procedure materialFromWeightFractionsUnsafe
    end interface fromWeightFractionsUnsafe

    character(len=*), parameter :: MODULE_NAME = "Material_m"

    public :: &
            combineByAtomFactors, &
            combineByAtomFactorsUnsafe, &
            combineByWeightFactors, &
            combineByWeightFactorsUnsafe, &
            fromAtomFractions, &
            fromAtomFractionsUnsafe, &
            fromWeightFractions, &
            fromWeightFractionsUnsafe
contains
    pure subroutine combineMaterialsByAtomFactors( &
            material1, factor1, material2, factor2, messages, errors, combined)
        type(Material_t), intent(in) :: material1
        double precision, intent(in) :: factor1
        type(Material_t), intent(in) :: material2
        double precision, intent(in) :: factor2
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Material_t), intent(out) :: combined

        character(len=*), parameter :: PROCEDURE_NAME = "combineMaterialsByAtomFactors"
        type(ErrorList_t) :: errors_
        type(MessageList_t) :: messages_

        call fromAtomFractions( &
                MaterialComponent( &
                        [material1%components%chemical, material2%components%chemical], &
                        [material1%components%fraction * factor1, material2%components%fraction * factor2]), &
                messages_, &
                errors_, &
                combined)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        call errors%appendErrors( &
                errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
    end subroutine combineMaterialsByAtomFactors

    pure subroutine combineMaterialsByAtomFactorsUnsafe( &
            material1, factor1, material2, factor2, combined)
        type(Material_t), intent(in) :: material1
        double precision, intent(in) :: factor1
        type(Material_t), intent(in) :: material2
        double precision, intent(in) :: factor2
        type(Material_t), intent(out) :: combined

        call fromAtomFractionsUnsafe( &
                MaterialComponent( &
                        [material1%components%chemical, material2%components%chemical], &
                        [material1%components%fraction * factor1, material2%components%fraction * factor2]), &
                combined)
    end subroutine combineMaterialsByAtomFactorsUnsafe

    pure subroutine combineMaterialsByWeightFactors( &
            material1, factor1, material2, factor2, messages, errors, combined)
        type(Material_t), intent(in) :: material1
        double precision, intent(in) :: factor1
        type(Material_t), intent(in) :: material2
        double precision, intent(in) :: factor2
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Material_t), intent(out) :: combined

        character(len=*), parameter :: PROCEDURE_NAME = "combineMaterialsByWeightFactors"
        type(ErrorList_t) :: errors_
        type(MessageList_t) :: messages_

        call fromWeightFractions( &
                MaterialComponent( &
                        [material1%components%chemical, material2%components%chemical], &
                        [material1%weightFraction(material1%components%chemical%symbol) * factor1, &
                        material2%weightFraction(material2%components%chemical%symbol) * factor2]), &
                messages_, &
                errors_, &
                combined)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        call errors%appendErrors( &
                errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
    end subroutine combineMaterialsByWeightFactors

    pure subroutine combineMaterialsByWeightFactorsUnsafe( &
            material1, factor1, material2, factor2, combined)
        type(Material_t), intent(in) :: material1
        double precision, intent(in) :: factor1
        type(Material_t), intent(in) :: material2
        double precision, intent(in) :: factor2
        type(Material_t), intent(out) :: combined

        call fromWeightFractionsUnsafe( &
                MaterialComponent( &
                        [material1%components%chemical, material2%components%chemical], &
                        [material1%weightFraction(material1%components%chemical%symbol) * factor1, &
                        material2%weightFraction(material2%components%chemical%symbol) * factor2]), &
                combined)
    end subroutine combineMaterialsByWeightFactorsUnsafe

    pure subroutine materialFromAtomFractions( &
            components, messages, errors, material)
        type(MaterialComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Material_t), intent(out) :: material

        character(len=*), parameter :: PROCEDURE_NAME = "materialFromAtomFractions"
        type(ErrorList_t) :: errors_
        type(MaterialComponent_t) :: fixed_components(size(components))
        type(MessageList_t) :: messages_

        call errorChecking(components, messages_, errors_, fixed_components)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            call fromAtomFractionsUnsafe(fixed_components, material)
        end if
    end subroutine materialFromAtomFractions

    pure subroutine errorChecking(components, messages, errors, fixed_components)
        type(MaterialComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(MaterialComponent_t), intent(out) :: fixed_components(size(components))

        character(len=*), parameter :: PROCEDURE_NAME = "errorChecking"

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
                fixed_components = MaterialComponent( &
                        components%chemical, &
                        components%fraction / sum(components%fraction))
            end if
        else
            call errors%appendError(Internal( &
                    INVALID_ARGUMENT_TYPE, &
                    Module_(MODULE_NAME), &
                    Procedure_(PROCEDURE_NAME), &
                    "All fractions must be greater than 0."))
        end if
    end subroutine errorChecking

    pure subroutine materialFromAtomFractionsUnsafe(components, material)
        type(MaterialComponent_t), intent(in) :: components(:)
        type(Material_t), intent(out) :: material

        call combineDuplicates(components, material%components)
    end subroutine materialFromAtomFractionsUnsafe

    pure subroutine materialFromWeightFractions( &
            components, messages, errors, material)
        type(MaterialComponent_t), intent(in) :: components(:)
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Material_t), intent(out) :: material

        character(len=*), parameter :: PROCEDURE_NAME = "materialFromWeightFractions"
        type(ErrorList_t) :: errors_
        type(MaterialComponent_t) :: fixed_components(size(components))
        type(MessageList_t) :: messages_

        call errorChecking(components, messages_, errors_, fixed_components)
        call messages%appendMessages( &
                messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            call fromWeightFractionsUnsafe(fixed_components, material)
        end if
    end subroutine materialFromWeightFractions

    pure subroutine materialFromWeightFractionsUnsafe(components, material)
        type(MaterialComponent_t), intent(in) :: components(:)
        type(Material_t), intent(out) :: material

        type(Mass_t), parameter :: ONE_KILOGRAM = Mass_t(kilograms = 1.0d0)
        type(Amount_t) :: amounts(size(components))

        amounts = components%fraction * ONE_KILOGRAM / components%chemical%molarMass()
        call fromAtomFractionsUnsafe( &
                MaterialComponent(components%chemical, amounts / sum(amounts)), &
                material)
    end subroutine materialFromWeightFractionsUnsafe

    elemental function atomFractionChemical(self, chemical) result(atom_fraction)
        class(Material_t), intent(in) :: self
        type(ChemicalSymbol_t), intent(in) :: chemical
        double precision :: atom_fraction

        integer :: position

        position = find(chemical, self%components%chemical)
        if (position > 0) then
            atom_fraction = self%components(position)%fraction
        else
            atom_fraction = 0.0d0
        end if
    end function atomFractionChemical

    elemental function atomFractionElement(self, element) result(atom_fraction)
        class(Material_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        double precision :: atom_fraction

        atom_fraction = sum( &
                self%components%chemical%atomFraction(element) &
                * self%components%fraction)
    end function atomFractionElement

    elemental function atomFractionIsotope(self, isotope) result(atom_fraction)
        class(Material_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = self%atomFraction(isotope%symbol)
    end function atomFractionIsotope

    elemental function atomFractionIsotopeSymbol(self, isotope) result(atom_fraction)
        class(Material_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = sum( &
                self%components%chemical%atomFraction(isotope) &
                * self%components%fraction)
    end function atomFractionIsotopeSymbol

    elemental function molarMass(self)
        class(Material_t), intent(in) :: self
        type(MolarMass_t) :: molarMass

        molarMass = sum(self%components%fraction * self%components%chemical%molarMass())
    end function molarMass

    elemental function weightFractionChemical(self, chemical) result(weight_fraction)
        class(Material_t), intent(in) :: self
        type(ChemicalSymbol_t), intent(in) :: chemical
        double precision :: weight_fraction

        type(MolarMass_t) :: masses(size(self%components))
        integer :: position

        position = find(chemical, self%components%chemical)
        if (position > 0) then
            masses = self%components%fraction * self%components%chemical%molarMass()
            weight_fraction = masses(position) / sum(masses)
        else
            weight_fraction = 0.0d0
        end if
    end function weightFractionChemical

    elemental function weightFractionElement(self, element) result(weight_fraction)
        class(Material_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        double precision :: weight_fraction

        weight_fraction = sum( &
                self%weightFraction(self%components%chemical%symbol) &
                * self%components%chemical%weightFraction(element))
    end function weightFractionElement

    elemental function weightFractionIsotope(self, isotope) result(weight_fraction)
        class(Material_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = self%weightFraction(isotope%symbol)
    end function weightFractionIsotope

    elemental function weightFractionIsotopeSymbol(self, isotope) result(weight_fraction)
        class(Material_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = sum( &
                self%weightFraction(self%components%chemical%symbol) &
                * self%components%chemical%weightFraction(isotope))
    end function weightFractionIsotopeSymbol

    pure subroutine combineDuplicates(inputs, combined)
        type(MaterialComponent_t), intent(in) :: inputs(:)
        type(MaterialComponent_t), allocatable, intent(out) :: combined(:)

        integer :: duplicate_position
        integer :: i
        integer :: new_num_components
        double precision :: normalizer
        integer :: num_inputs
        integer :: prev_num_components
        type(Chemical_t) :: working_chemical
        type(MaterialComponent_t), allocatable :: working_components(:)

        num_inputs = size(inputs)
        allocate(combined(1))
        combined(1) = inputs(1)
        do i = 2, num_inputs
            duplicate_position = find(inputs(i)%chemical%symbol, combined%chemical)
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
                normalizer = inputs(i)%fraction + combined(duplicate_position)%fraction
                working_chemical = combined(duplicate_position)%chemical
                call combineByAtomFactorsUnsafe( &
                        working_chemical, &
                        combined(duplicate_position)%fraction / normalizer, &
                        inputs(i)%chemical, &
                        inputs(i)%fraction / normalizer, &
                        combined(duplicate_position)%chemical)
                combined(duplicate_position)%fraction = &
                        combined(duplicate_position)%fraction + inputs(i)%fraction
            end if
        end do
    end subroutine combineDuplicates
end module Material_m
