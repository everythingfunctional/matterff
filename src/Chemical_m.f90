module Chemical_m
    use Chemical_component_m, only: &
            ChemicalComponent_t, ChemicalComponent, fromJson
    use Chemical_symbol_m, only: &
            ChemicalSymbol_t, &
            fromJson, &
            fromString, &
            argonGasSymbol, &
            hydrogenGasSymbol, &
            heliumGasSymbol, &
            nitrogenGasSymbol, &
            waterSymbol, &
            xenonGasSymbol
    use Element_m, only: &
            Element_t, &
            combineByAtomFactorsUnsafe, &
            find, &
            naturalHelium, &
            naturalHydrogen, &
            naturalNitrogen, &
            naturalOxygen, &
            naturalArgon, &
            naturalXenon
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: &
            ErrorList_t, MessageList_t, Fatal, Internal, Module_, Procedure_
    use iso_varying_string, only: VARYING_STRING, operator(//), char
    use Isotope_m, only: Isotope_t
    use Isotope_symbol_m, only: IsotopeSymbol_t
    use jsonff, only: &
            JsonArray_t, &
            JsonElement_t, &
            JsonMember_t, &
            JsonObject_t, &
            JsonString_t, &
            JsonArray, &
            JsonElement, &
            JsonMemberUnsafe, &
            JsonNumber, &
            JsonObject
    use quaff, only: Amount_t, Mass_t, MolarMass_t, operator(/), sum
    use strff, only: join
    use Utilities_m, only: INVALID_ARGUMENT_TYPE, MISMATCH_TYPE

    implicit none
    private

    ! It is assumed throughout this module that a Chemical will never be
    ! uninitialized or empty. Any attempt to create an empty chemical or use a
    ! chemical that has not been intialized will likely result in a memory error
    type, public :: Chemical_t
        private
        type(ChemicalSymbol_t), public :: symbol
        type(ChemicalComponent_t), allocatable :: components(:)
    contains
        private
        procedure :: amountElement
        procedure :: amountIsotope
        generic, public :: amount => amountElement, amountIsotope
        procedure :: atomFractionElement
        procedure :: atomFractionIsotope
        procedure :: atomFractionIsotopeSymbol
        generic, public :: atomFraction => &
                atomFractionElement, &
                atomFractionIsotope, &
                atomFractionIsotopeSymbol
        procedure, public :: molarMass
        procedure, public :: toJsonWithFraction
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

    interface getNatural
        module procedure getNaturalC
        module procedure getNaturalS
    end interface getNatural

    interface find
        module procedure findChemical
    end interface find

    interface fromJson
        module procedure chemicalFromJson
    end interface fromJson

    character(len=*), parameter :: MODULE_NAME = "Chemical_m"

    public :: &
            combineByAtomFactors, &
            combineByAtomFactorsUnsafe, &
            combineByWeightFactors, &
            combineByWeightFactorsUnsafe, &
            find, &
            fromJson, &
            makeChemical, &
            makeChemicalUnsafe, &
            naturalArgonGas, &
            naturalHeliumGas, &
            naturalHydrogenGas, &
            naturalNitrogenGas, &
            naturalWater, &
            naturalXenonGas
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
            if (all(components%multiplier > 0.0d0)) then
                call makeChemicalUnsafe(symbol, components, chemical)
            else
                call errors%appendError(Internal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "All multipliers must be greater than 0."))
            end if
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

    pure subroutine chemicalFromJson(json, messages, errors, chemical)
        type(JsonObject_t), intent(in) :: json
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Chemical_t), intent(out) :: chemical

        character(len=*), parameter :: PROCEDURE_NAME = "chemicalFromJson"
        type(JsonElement_t) :: component_element
        type(ChemicalComponent_t), allocatable :: components(:)
        type(JsonElement_t) :: elements_element
        type(ErrorList_t) :: errors_
        integer :: i
        type(MessageList_t) :: messages_
        integer :: num_components
        type(ChemicalSymbol_t) :: symbol
        type(JsonElement_t) :: symbol_element

        call json%getElement("chemical", errors_, symbol_element)
        if (errors_%hasAny()) then
            call json%getElement("natural", errors_, symbol_element)
            if (errors_%hasAny()) then
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "chemical must have chemical and elements or natural"))
            else
                select type (symbol => symbol_element%element)
                type is (JsonString_t)
                    call getNatural(symbol%getValue(), errors_, chemical)
                    call errors%appendErrors( &
                            errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                class default
                    call errors%appendError(Fatal( &
                            INVALID_ARGUMENT_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "natural must be a string"))
                end select
            end if
        else
            select type (symbol_array => symbol_element%element)
            type is (JsonArray_t)
                call fromJson(symbol_array, errors_, symbol)
                if (errors_%hasAny()) then
                    call errors%appendErrors( &
                            errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                    return
                end if
            type is (JsonString_t)
                call fromString(symbol_array%getValue(), errors_, symbol)
                if (errors_%hasAny()) then
                    call errors%appendErrors( &
                            errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                    return
                end if
            class default
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "chemical must be an array or a string"))
                return
            end select
            call json%getElement("elements", errors_, elements_element)
            if (errors_%hasAny()) then
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "chemical must contain list of element compositions"))
            else
                select type (elements_array => elements_element%element)
                type is (JsonArray_t)
                    num_components = elements_array%length()
                    allocate(components(num_components))
                    do i = 1, num_components
                        call elements_array%getElement(i, errors_, component_element)
                        if (errors_%hasAny()) then
                            call errors%appendErrors( &
                                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                            return
                        else
                            select type (element => component_element%element)
                            type is (JsonObject_t)
                                call fromJson(element, messages_, errors_, components(i))
                                call messages%appendMessages( &
                                        messages_, &
                                        Module_(MODULE_NAME), &
                                        Procedure_(PROCEDURE_NAME))
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
                                        "elements array must all be objects"))
                                return
                            end select
                        end if
                    end do
                    call makeChemical(symbol, components, errors_, chemical)
                    call errors%appendErrors( &
                            errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                class default
                    call errors%appendError(Fatal( &
                            INVALID_ARGUMENT_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "elements must be an array"))
                end select
            end if
        end if
    end subroutine chemicalFromJson

    pure subroutine getNaturalC(symbol, errors, chemical)
        character(len=*), intent(in) :: symbol
        type(ErrorList_t), intent(out) :: errors
        type(Chemical_t), intent(out) :: chemical

        select case (symbol)
        case ("Ar", "Ar1")
            chemical = naturalArgonGas()
        case ("He", "He1")
            chemical = naturalHeliumGas()
        case ("H2")
            chemical = naturalHydrogenGas()
        case ("N2")
            chemical = naturalNitrogenGas()
        case ("H2O", "H2O1", "water")
            chemical = naturalWater()
        case ("Xe", "Xe1")
            chemical = naturalXenonGas()
        case default
            call errors%appendError(Fatal( &
                    INVALID_ARGUMENT_TYPE, &
                    Module_(MODULE_NAME), &
                    Procedure_("getNaturalC"), &
                    "No natural composition available for " // symbol))
        end select
    end subroutine getNaturalC

    pure subroutine getNaturalS(symbol, errors, chemical)
        type(VARYING_STRING), intent(in) :: symbol
        type(ErrorList_t), intent(out) :: errors
        type(Chemical_t), intent(out) :: chemical

        type(ErrorList_t) :: errors_

        call getNatural(char(symbol), errors_, chemical)
        call errors%appendErrors( &
                errors_, Module_(MODULE_NAME), Procedure_("getNaturalS"))
    end subroutine getNaturalS

    pure function naturalArgonGas()
        type(Chemical_t) :: naturalArgonGas

        naturalArgonGas%symbol = argonGasSymbol()
        allocate(naturalArgonGas%components(1))
        naturalArgonGas%components(1) = ChemicalComponent(naturalArgon(), 1.0d0)
    end function naturalArgonGas

    pure function naturalHeliumGas()
        type(Chemical_t) :: naturalHeliumGas

        naturalHeliumGas%symbol = heliumGasSymbol()
        allocate(naturalHeliumGas%components(1))
        naturalHeliumGas%components(1) = ChemicalComponent(naturalHelium(), 1.0d0)
    end function naturalHeliumGas

    pure function naturalHydrogenGas()
        type(Chemical_t) :: naturalHydrogenGas

        naturalHydrogenGas%symbol = hydrogenGasSymbol()
        allocate(naturalHydrogenGas%components(1))
        naturalHydrogenGas%components(1) = ChemicalComponent(naturalHydrogen(), 2.0d0)
    end function naturalHydrogenGas

    pure function naturalNitrogenGas()
        type(Chemical_t) :: naturalNitrogenGas

        naturalNitrogenGas%symbol = nitrogenGasSymbol()
        allocate(naturalNitrogenGas%components(1))
        naturalNitrogenGas%components(1) = ChemicalComponent(naturalNitrogen(), 2.0d0)
    end function naturalNitrogenGas

    pure function naturalWater()
        type(Chemical_t) :: naturalWater

        naturalWater%symbol = waterSymbol()
        allocate(naturalWater%components(2))
        naturalWater%components(1) = ChemicalComponent(naturalHydrogen(), 2.0d0)
        naturalWater%components(2) = ChemicalComponent(naturalOxygen(), 1.0d0)
    end function naturalWater

    pure function naturalXenonGas()
        type(Chemical_t) :: naturalXenonGas

        naturalXenonGas%symbol = xenonGasSymbol()
        allocate(naturalXenonGas%components(1))
        naturalXenonGas%components(1) = ChemicalComponent(naturalXenon(), 1.0d0)
    end function naturalXenonGas

    elemental function amountElement(self, total_amount, element) result(amount)
        class(Chemical_t), intent(in) :: self
        type(Amount_t), intent(in) :: total_amount
        type(ElementSymbol_t), intent(in) :: element
        type(Amount_t) :: amount

        type(Amount_t), parameter :: ZERO = Amount_t(mols = 0.0d0)
        integer :: position

        position = find(element, self%components%element)
        if (position > 0) then
            amount = self%components(position)%multiplier * total_amount
        else
            amount = ZERO
        end if
    end function amountElement

    elemental function amountIsotope(self, total_amount, isotope) result(amount)
        class(Chemical_t), intent(in) :: self
        type(Amount_t), intent(in) :: total_amount
        type(Isotope_t), intent(in) :: isotope
        type(Amount_t) :: amount

        amount = sum(self%components%multiplier * self%components%element%atomFraction(isotope) * total_amount)
    end function amountIsotope

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

    pure function toJsonWithFraction(self, fraction) result(json)
        class(Chemical_t), intent(in) :: self
        double precision, intent(in) :: fraction
        type(JsonObject_t) :: json

        integer :: i
        type(JsonElement_t) :: elements(size(self%components))
        type(JsonMember_t) :: members(3)

        do i = 1, size(self%components)
            elements(i) = JsonElement( &
                    self%components(i)%element%toJsonWithMultiplier( &
                            self%components(i)%multiplier))
        end do
        members(1) = JsonMemberUnsafe("fraction", JsonNumber(fraction))
        members(2) = JsonMemberUnsafe("chemical", self%symbol%toJson())
        members(3) = JsonMemberUnsafe("elements", JsonArray(elements))
        json = JsonObject(members)
    end function toJsonWithFraction

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
        type(ChemicalComponent_t), allocatable :: working_components(:)
        type(Element_t) :: working_element

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

    pure function findChemical(symbol, chemicals) result(position)
        type(ChemicalSymbol_t), intent(in) :: symbol
        type(Chemical_t), intent(in) :: chemicals(:)
        integer :: position

        integer :: i

        position = 0
        do i = 1, size(chemicals)
            if (chemicals(i)%symbol == symbol) then
                position = i
                exit
            end if
        end do
    end function findChemical
end module Chemical_m
