module Material_m
    use Chemical_m, only: &
            Chemical_t, &
            combineByAtomFactorsUnsafe, &
            find, &
            naturalArgonGas, &
            naturalHeliumGas, &
            naturalHydrogenGas, &
            naturalKryptonGas, &
            naturalOxygenGas, &
            naturalNitrogenGas, &
            naturalWater, &
            naturalXenonGas
    use Chemical_symbol_m, only: ChemicalSymbol_t
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: &
            ErrorList_t, &
            MessageList_t, &
            Fatal, &
            Info, &
            Internal, &
            Module_, &
            Procedure_
    use iso_varying_string, only: operator(//)
    use Isotope_m, only: Isotope_t
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use jsonff, only: &
            JsonArray_t, &
            JsonElement_t, &
            JsonMember_t, &
            JsonObject_t, &
            JsonArray, &
            JsonElement, &
            JsonMemberUnsafe, &
            JsonObject
    use Material_component_m, only: &
            MaterialComponent_t, MaterialComponent, fromJson
    use matterff_Utilities_m, only: &
            operator(.sumsTo.), &
            INVALID_ARGUMENT_TYPE, &
            MISMATCH_TYPE, &
            NORMALIZED_FRACTIONS_TYPE
    use quaff, only: Amount_t, Mass_t, MolarMass_t, operator(/), sum
    use strff, only: join

    implicit none
    private

    ! It is assumed throughout this module that a Material will never be
    ! uninitialized or empty. Any attempt to create an empty material or use a
    ! material that has not been intialized will likely result in a memory error
    type, public :: Material_t
        private
        type(MaterialComponent_t), public, allocatable :: components(:)
    contains
        private
        procedure :: amountElement
        procedure :: amountIsotope
        generic, public :: amount => amountElement, amountIsotope
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
        procedure, public :: toJson
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

    interface fromJson
        module procedure materialFromJson
    end interface fromJson

    character(len=*), parameter :: MODULE_NAME = "Material_m"

    public :: &
            combineByAtomFactors, &
            combineByAtomFactorsUnsafe, &
            combineByWeightFactors, &
            combineByWeightFactorsUnsafe, &
            fromAtomFractions, &
            fromAtomFractionsUnsafe, &
            fromWeightFractions, &
            fromWeightFractionsUnsafe, &
            fromJson, &
            pureNaturalArgonGas, &
            pureNaturalHeliumGas, &
            pureNaturalHydrogenGas, &
            pureNaturalKryptonGas, &
            pureNaturalNitrogenGas, &
            pureNaturalOxygenGas, &
            pureNaturalWater, &
            pureNaturalXenonGas
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
        double precision :: component_fractions(size(components))

        component_fractions = components%fraction
        if (all(component_fractions > 0.0d0)) then
            if (component_fractions.sumsTo.1.0d0) then
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
                        component_fractions / sum(component_fractions))
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

    pure subroutine materialFromJson(json, messages, errors, material)
        type(JsonObject_t), intent(in) :: json
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Material_t), intent(out) :: material

        character(len=*), parameter :: PROCEDURE_NAME = "materialFromJson"
        type(JsonElement_t) :: chemical_element
        type(MaterialComponent_t), allocatable :: components(:)
        type(ErrorList_t) :: errors_
        type(JsonElement_t) :: fractions_element
        integer :: i
        type(MessageList_t) :: messages_
        integer :: num_chemicals

        call json%getElement("atomFractions", errors_, fractions_element)
        if (errors_%hasAny()) then
            call json%getElement("weightFractions", errors_, fractions_element)
            if (errors_%hasAny()) then
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "material must either have" &
                        // " atomFractions or weightFractions"))
            else
                ! Create from weight fractions here
                select type (fractions_array => fractions_element%element)
                type is (JsonArray_t)
                    num_chemicals = fractions_array%length()
                    allocate(components(num_chemicals))
                    do i = 1, num_chemicals
                        call fractions_array%getElement(i, errors_, chemical_element)
                        if (errors_%hasAny()) then
                            call errors%appendErrors( &
                                    errors_, &
                                    Module_(MODULE_NAME), &
                                    Procedure_(PROCEDURE_NAME))
                            return
                        else
                            select type (chemical => chemical_element%element)
                            type is (JsonObject_t)
                                call fromJson(chemical, messages_, errors_, components(i))
                                call messages%appendMessages( &
                                        messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                                if (errors_%hasAny()) then
                                    call errors%appendErrors( &
                                            errors_, &
                                            Module_(MODULE_NAME), &
                                            Procedure_(PROCEDURE_NAME))
                                    return
                                end if
                            class default
                                call errors%appendError(Fatal( &
                                        INVALID_ARGUMENT_TYPE, &
                                        Module_(MODULE_NAME), &
                                        Procedure_(PROCEDURE_NAME), &
                                        "weightFractions array must all be objects"))
                                return
                            end select
                        end if
                    end do
                    call fromWeightFractions( &
                            components, &
                            messages_, &
                            errors_, &
                            material)
                    call messages%appendMessages( &
                            messages_, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME))
                    call errors%appendErrors( &
                            errors_, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME))
                class default
                    call errors%appendError(Fatal( &
                            INVALID_ARGUMENT_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "weightFractions must be an array"))
                end select
            end if
        else
            select type (fractions_array => fractions_element%element)
            type is (JsonArray_t)
                ! Create from atom fractions here
                num_chemicals = fractions_array%length()
                allocate(components(num_chemicals))
                do i = 1, num_chemicals
                    call fractions_array%getElement(i, errors_, chemical_element)
                    if (errors_%hasAny()) then
                        call errors%appendErrors( &
                                errors_, &
                                Module_(MODULE_NAME), &
                                Procedure_(PROCEDURE_NAME))
                        return
                    else
                        select type (chemical => chemical_element%element)
                        type is (JsonObject_t)
                            call fromJson(chemical, messages_, errors_, components(i))
                            call messages%appendMessages( &
                                    messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                            if (errors_%hasAny()) then
                                call errors%appendErrors( &
                                        errors_, &
                                        Module_(MODULE_NAME), &
                                        Procedure_(PROCEDURE_NAME))
                                return
                            end if
                        class default
                            call errors%appendError(Fatal( &
                                    INVALID_ARGUMENT_TYPE, &
                                    Module_(MODULE_NAME), &
                                    Procedure_(PROCEDURE_NAME), &
                                    "atomFractions array must all be objects"))
                            return
                        end select
                    end if
                end do
                call fromAtomFractions( &
                        components, &
                        messages_, &
                        errors_, &
                        material)
                call messages%appendMessages( &
                        messages_, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME))
                call errors%appendErrors( &
                        errors_, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME))
            class default
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "atomFractions must be an array"))
            end select
        end if
    end subroutine materialFromJson

    pure function pureNaturalArgonGas()
        type(Material_t) :: pureNaturalArgonGas

        allocate(pureNaturalArgonGas%components(1))
        pureNaturalArgonGas%components(1) = MaterialComponent(naturalArgonGas(), 1.0d0)
    end function pureNaturalArgonGas

    pure function pureNaturalHeliumGas()
        type(Material_t) :: pureNaturalHeliumGas

        allocate(pureNaturalHeliumGas%components(1))
        pureNaturalHeliumGas%components(1) = MaterialComponent(naturalHeliumGas(), 1.0d0)
    end function pureNaturalHeliumGas

    pure function pureNaturalHydrogenGas()
        type(Material_t) :: pureNaturalHydrogenGas

        allocate(pureNaturalHydrogenGas%components(1))
        pureNaturalHydrogenGas%components(1) = MaterialComponent(naturalHydrogenGas(), 1.0d0)
    end function pureNaturalHydrogenGas

    pure function pureNaturalKryptonGas()
        type(Material_t) :: pureNaturalKryptonGas

        allocate(pureNaturalKryptonGas%components(1))
        pureNaturalKryptonGas%components(1) = MaterialComponent(naturalKryptonGas(), 1.0d0)
    end function pureNaturalKryptonGas

    pure function pureNaturalNitrogenGas()
        type(Material_t) :: pureNaturalNitrogenGas

        allocate(pureNaturalNitrogenGas%components(1))
        pureNaturalNitrogenGas%components(1) = MaterialComponent(naturalNitrogenGas(), 1.0d0)
    end function pureNaturalNitrogenGas

    pure function pureNaturalOxygenGas()
        type(Material_t) :: pureNaturalOxygenGas

        allocate(pureNaturalOxygenGas%components(1))
        pureNaturalOxygenGas%components(1) = MaterialComponent(naturalOxygenGas(), 1.0d0)
    end function pureNaturalOxygenGas

    pure function pureNaturalWater()
        type(Material_t) :: pureNaturalWater

        allocate(pureNaturalWater%components(1))
        pureNaturalWater%components(1) = MaterialComponent(naturalWater(), 1.0d0)
    end function pureNaturalWater

    pure function pureNaturalXenonGas()
        type(Material_t) :: pureNaturalXenonGas

        allocate(pureNaturalXenonGas%components(1))
        pureNaturalXenonGas%components(1) = MaterialComponent(naturalXenonGas(), 1.0d0)
    end function pureNaturalXenonGas

    elemental function amountElement(self, total_amount, element) result(amount)
        class(Material_t), intent(in) :: self
        type(Amount_t), intent(in) :: total_amount
        type(ElementSymbol_t), intent(in) :: element
        type(Amount_t) :: amount

        amount = sum(self%components%chemical%amount(total_amount, element) * self%components%fraction)
    end function amountElement

    elemental function amountIsotope(self, total_amount, isotope) result(amount)
        class(Material_t), intent(in) :: self
        type(Amount_t), intent(in) :: total_amount
        type(Isotope_t), intent(in) :: isotope
        type(Amount_t) :: amount

        amount = sum(self%components%chemical%amount(total_amount, isotope) * self%components%fraction)
    end function amountIsotope

    elemental function atomFractionChemical(self, chemical) result(atom_fraction)
        class(Material_t), intent(in) :: self
        type(ChemicalSymbol_t), intent(in) :: chemical
        double precision :: atom_fraction

        type(Chemical_t) :: chemicals(size(self%components))
        integer :: position

        chemicals = self%components%chemical
        position = find(chemical, chemicals)
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

    pure function toJson(self) result(json)
        class(Material_t), intent(in) :: self
        type(JsonObject_t) :: json

        integer :: i
        type(JsonElement_t) :: chemicals(size(self%components))
        type(JsonMember_t) :: member

        do i = 1, size(self%components)
            chemicals(i) = jsonElement( &
                    self%components(i)%chemical%toJsonWithFraction( &
                            self%components(i)%fraction))
        end do
        member = JsonMemberUnsafe("atomFractions", JsonArray(chemicals))
        json = JsonObject([member])
    end function toJson

    elemental function weightFractionChemical(self, chemical) result(weight_fraction)
        class(Material_t), intent(in) :: self
        type(ChemicalSymbol_t), intent(in) :: chemical
        double precision :: weight_fraction

        type(Chemical_t) :: chemicals(size(self%components))
        type(MolarMass_t) :: masses(size(self%components))
        integer :: position

        chemicals = self%components%chemical
        position = find(chemical, chemicals)
        if (position > 0) then
            masses = self%components%fraction * chemicals%molarMass()
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

        type(Chemical_t), allocatable :: combined_chemicals(:)
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
            allocate(combined_chemicals(size(combined)))
            combined_chemicals = combined%chemical
            duplicate_position = find(inputs(i)%chemical%symbol, combined_chemicals)
            deallocate(combined_chemicals)
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
