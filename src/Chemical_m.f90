module Chemical_m
    use Chemical_component_m, only: ChemicalComponent_t, ChemicalComponent
    use Chemical_symbol_m, only: &
            ChemicalSymbol_t, hydrogenGasSymbol, heliumGasSymbol
    use Element_m, only: &
            Element_t, &
            combineByAtomFactorsUnsafe, &
            find, &
            naturalHydrogen, &
            naturalHelium
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: ErrorList_t, Internal, Module_, Procedure_
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: Isotope_t
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use quaff, only: Amount_t, Mass_t, MolarMass_t, operator(/), sum
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
        procedure :: atomFractionIsotope
        procedure :: atomFractionIsotopeSymbol
        generic, public :: atomFraction => &
                atomFractionElement, &
                atomFractionIsotope, &
                atomFractionIsotopeSymbol
        procedure, public :: molarMass
        procedure :: weightFractionElement
        procedure :: weightFractionIsotope
        procedure :: weightFractionIsotopeSymbol
        generic, public :: weightFraction => &
                weightFractionElement, &
                weightFractionIsotope, &
                weightFractionIsotopeSymbol
    end type Chemical_t

    interface combineByAtomFactors
        module procedure combineChemicalsByAtomFactors
    end interface combineByAtomFactors

    interface combineByAtomFactorsUnsafe
        module procedure combineChemicalsByAtomFactorsUnsafe
    end interface combineByAtomFactorsUnsafe

    interface combineByWeightFactors
        module procedure combineChemicalsByWeightFactors
    end interface combineByWeightFactors

    interface combineByWeightFactorsUnsafe
        module procedure combineChemicalsByWeightFactorsUnsafe
    end interface combineByWeightFactorsUnsafe

    character(len=*), parameter :: MODULE_NAME = "Chemical_m"

    public :: &
            combineByAtomFactors, &
            combineByAtomFactorsUnsafe, &
            combineByWeightFactors, &
            combineByWeightFactorsUnsafe, &
            makeChemical, &
            makeChemicalUnsafe, &
            naturalHydrogenGas, &
            naturalHeliumGas
contains
    pure subroutine combineChemicalsByAtomFactors( &
            chemical1, factor1, chemical2, factor2, errors, combined)
        type(Chemical_t), intent(in) :: chemical1
        double precision, intent(in) :: factor1
        type(Chemical_t), intent(in) :: chemical2
        double precision, intent(in) :: factor2
        type(ErrorList_t), intent(out) :: errors
        type(Chemical_t), intent(out) :: combined

        character(len=*), parameter :: PROCEDURE_NAME = "combineChemicalsByAtomFactors"
        type(ErrorList_t) :: errors_
        double precision :: normalizer

        call combineErrorCheck(chemical1, chemical2, errors_)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            normalizer = factor1 + factor2
            call combineByAtomFactorsUnsafe( &
                    chemical1, factor1 / normalizer, chemical2, factor2 / normalizer, combined)
        end if
    end subroutine combineChemicalsByAtomFactors

    pure subroutine combineErrorCheck(chemical1, chemical2, errors)
        type(Chemical_t), intent(in) :: chemical1
        type(Chemical_t), intent(in) :: chemical2
        type(ErrorList_t), intent(out) :: errors

        character(len=*), parameter :: PROCEDURE_NAME = "combineErrorCheck"

        if (.not. chemical1%symbol == chemical2%symbol) then
            call errors%appendError(Internal( &
                    MISMATCH_TYPE, &
                    Module_(MODULE_NAME), &
                    Procedure_(PROCEDURE_NAME), &
                    "Attempted to combine different chemicals: " &
                    // chemical1%symbol%toString() // " and " // chemical2%symbol%toString()))
        end if
    end subroutine combineErrorCheck

    pure subroutine combineChemicalsByAtomFactorsUnsafe( &
            chemical1, factor1, chemical2, factor2, combined)
        type(Chemical_t), intent(in) :: chemical1
        double precision, intent(in) :: factor1
        type(Chemical_t), intent(in) :: chemical2
        double precision, intent(in) :: factor2
        type(Chemical_t), intent(out) :: combined

        call makeChemicalUnsafe( &
                chemical1%symbol, &
                ChemicalComponent( &
                        [chemical1%components%element, chemical2%components%element], &
                        [chemical1%components%multiplier * factor1, &
                            chemical2%components%multiplier * factor2]), &
                combined)
    end subroutine combineChemicalsByAtomFactorsUnsafe

    pure subroutine combineChemicalsByWeightFactors( &
            chemical1, factor1, chemical2, factor2, errors, combined)
        type(Chemical_t), intent(in) :: chemical1
        double precision, intent(in) :: factor1
        type(Chemical_t), intent(in) :: chemical2
        double precision, intent(in) :: factor2
        type(ErrorList_t), intent(out) :: errors
        type(Chemical_t), intent(out) :: combined

        character(len=*), parameter :: PROCEDURE_NAME = "combineChemicalsByWeightFactors"
        type(ErrorList_t) :: errors_

        call combineErrorCheck(chemical1, chemical2, errors_)
        if (errors_%hasAny()) then
            call errors%appendErrors( &
                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
        else
            call combineByWeightFactorsUnsafe( &
                    chemical1, factor1, chemical2, factor2, combined)
        end if
    end subroutine combineChemicalsByWeightFactors

    pure subroutine combineChemicalsByWeightFactorsUnsafe( &
            chemical1, factor1, chemical2, factor2, combined)
        type(Chemical_t), intent(in) :: chemical1
        double precision, intent(in) :: factor1
        type(Chemical_t), intent(in) :: chemical2
        double precision, intent(in) :: factor2
        type(Chemical_t), intent(out) :: combined

        type(Mass_t), parameter :: ONE_KILOGRAM = Mass_t(kilograms = 1.0d0)
        type(Amount_t) :: amounts(2)
        type(Amount_t) :: total_amount

        amounts(1) = factor1 * ONE_KILOGRAM / chemical1%molarMass()
        amounts(2) = factor2 * ONE_KILOGRAM / chemical2%molarMass()
        total_amount = sum(amounts)
        call combineByAtomFactorsUnsafe( &
                chemical1, &
                amounts(1) / total_amount, &
                chemical2, &
                amounts(2) / total_amount, &
                combined)
    end subroutine combineChemicalsByWeightFactorsUnsafe

    pure subroutine makeChemical(symbol, components, errors, chemical)
        type(ChemicalSymbol_t), intent(in) :: symbol
        type(ChemicalComponent_t), intent(in) :: components(:)
        type(ErrorList_t), intent(out) :: errors
        type(Chemical_t), intent(out) :: chemical

        character(len=*), parameter :: PROCEDURE_NAME = "makeChemical"

        if (all(symbol%includes(components%element%symbol))) then
            call makeChemicalUnsafe(symbol, components, chemical)
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

    pure subroutine makeChemicalUnsafe(symbol, components, chemical)
        type(ChemicalSymbol_t), intent(in) :: symbol
        type(ChemicalComponent_t), intent(in) :: components(:)
        type(Chemical_t), intent(out) :: chemical

        chemical%symbol = symbol
        call combineDuplicates(components, chemical%components)
    end subroutine makeChemicalUnsafe

    pure function naturalHydrogenGas()
        type(Chemical_t) :: naturalHydrogenGas

        naturalHydrogenGas%symbol = hydrogenGasSymbol()
        allocate(naturalHydrogenGas%components(1))
        naturalHydrogenGas%components(1) = ChemicalComponent(naturalHydrogen(), 2.0d0)
    end function naturalHydrogenGas

    pure function naturalHeliumGas()
        type(Chemical_t) :: naturalHeliumGas

        naturalHeliumGas%symbol = heliumGasSymbol()
        allocate(naturalHeliumGas%components(1))
        naturalHeliumGas%components(1) = ChemicalComponent(naturalHelium(), 1.0d0)
    end function naturalHeliumGas

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

    elemental function atomFractionIsotope(self, isotope) result(atom_fraction)
        class(Chemical_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = self%atomFraction(isotope%symbol)
    end function atomFractionIsotope

    elemental function atomFractionIsotopeSymbol( &
            self, isotope) result(atom_fraction)
        class(Chemical_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: atom_fraction

        atom_fraction = sum( &
                (self%components%multiplier / sum(self%components%multiplier)) &
                * self%components%element%atomFraction(isotope))
    end function atomFractionIsotopeSymbol

    elemental function molarMass(self)
        class(Chemical_t), intent(in) :: self
        type(MolarMass_t) :: molarMass

        molarMass = sum(self%components%multiplier * self%components%element%atomicMass())
    end function molarMass

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

    elemental function weightFractionIsotope(self, isotope) result(weight_fraction)
        class(Chemical_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = self%weightFraction(isotope%symbol)
    end function weightFractionIsotope

    elemental function weightFractionIsotopeSymbol( &
            self, isotope) result(weight_fraction)
        class(Chemical_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: weight_fraction

        weight_fraction = sum( &
                self%weightFraction(self%components%element%symbol) &
                * self%components%element%weightFraction(isotope))
    end function weightFractionIsotopeSymbol

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
