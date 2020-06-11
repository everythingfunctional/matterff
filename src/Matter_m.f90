module Matter_m
    use Chemical_symbol_m, only: ChemicalSymbol_t
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: &
            ErrorList_t, MessageList_t, Fatal, Internal, Module_, Procedure_
    use Isotope_m, only: Isotope_t
    use jsonff, only: &
            JsonElement_t, &
            JsonMember_t, &
            JsonObject_t, &
            JsonString_t, &
            JsonMemberUnsafe, &
            JsonObject, &
            JsonStringUnsafe
    use Material_m, only: Material_t, combineByAtomFactorsUnsafe, fromJson
    use matterff_Utilities_m, only: INVALID_ARGUMENT_TYPE
    use quaff, only: Amount_t, Mass_t, operator(*), operator(/), fromString

    implicit none
    private

    type, public :: Matter_t
        private
        type(Amount_t) :: amount_
        type(Material_t) :: material
    contains
        private
        procedure :: totalAmount
        procedure :: amountChemical
        procedure :: amountElement
        procedure :: amountIsotope
        generic, public :: amount => &
                totalAmount, amountChemical, amountElement, amountIsotope
        procedure :: totalMass
        procedure :: massChemical
        procedure :: massElement
        procedure :: massIsotope
        generic, public :: mass => &
                totalMass, massChemical, massElement, massIsotope
        procedure, public :: toJson
    end type Matter_t

    interface operator(+)
        module procedure combineMatter
    end interface operator(+)

    interface createMatter
        module procedure createMatterWithAmount
        module procedure createMatterWithMass
    end interface createMatter

    interface createMatterUnsafe
        module procedure createMatterWithAmountUnsafe
        module procedure createMatterWithMassUnsafe
    end interface createMatterUnsafe

    interface fromJson
        module procedure matterFromJson
    end interface fromJson

    character(len=*), parameter :: MODULE_NAME = "Matter_m"

    public :: operator(+), createMatter, createMatterUnsafe, fromJson
contains
    pure subroutine createMatterWithAmount(amount, material, errors, matter)
        type(Amount_t), intent(in) :: amount
        type(Material_t), intent(in) :: material
        type(ErrorList_t), intent(out) :: errors
        type(Matter_t), intent(out) :: matter

        type(Amount_t), parameter :: ZERO = Amount_t(mols = 0.0d0)

        if (amount < ZERO) then
            call errors%appendError(Internal( &
                    INVALID_ARGUMENT_TYPE, &
                    Module_(MODULE_NAME), &
                    Procedure_("createMatterWithAmount"), &
                    "Cannot have negative matter."))
        else
            call createMatterUnsafe(amount, material, matter)
        end if
    end subroutine createMatterWithAmount

    pure subroutine createMatterWithMass(mass, material, errors, matter)
        type(Mass_t), intent(in) :: mass
        type(Material_t), intent(in) :: material
        type(ErrorList_t), intent(out) :: errors
        type(Matter_t), intent(out) :: matter

        type(ErrorList_t) :: errors_

        call createMatter(mass / material%molarMass(), material, errors_, matter)
        call errors%appendErrors( &
                errors_, Module_(MODULE_NAME), Procedure_("createMatterWithMass"))
    end subroutine createMatterWithMass

    pure subroutine createMatterWithAmountUnsafe(amount, material, matter)
        type(Amount_t), intent(in) :: amount
        type(Material_t), intent(in) :: material
        type(Matter_t), intent(out) :: matter

        matter%amount_ = amount
        matter%material = material
    end subroutine createMatterWithAmountUnsafe

    pure subroutine createMatterWithMassUnsafe(mass, material, matter)
        type(Mass_t), intent(in) :: mass
        type(Material_t), intent(in) :: material
        type(Matter_t), intent(out) :: matter

        call createMatterUnsafe(mass / material%molarMass(), material, matter)
    end subroutine createMatterWithMassUnsafe

    pure subroutine matterFromJson(json, messages, errors, matter)
        type(JsonObject_t), intent(in) :: json
        type(MessageList_t), intent(out) :: messages
        type(ErrorList_t), intent(out) :: errors
        type(Matter_t), intent(out) :: matter

        character(len=*), parameter :: PROCEDURE_NAME = "matterFromJson"
        type(Amount_t) :: amount
        type(JsonElement_t) :: amount_element
        type(ErrorList_t) :: errors_
        type(Mass_t) :: mass
        type(Material_t) :: material
        type(JsonElement_t) :: material_element
        type(MessageList_t) :: messages_

        call json%getElement("amount", errors_, amount_element)
        if (errors_%hasAny()) then
            call json%getElement("mass", errors_, amount_element)
            if (errors_%hasAny()) then
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "matter must have amount or mass"))
            else
                select type (mass_string => amount_element%element)
                type is (JsonString_t)
                    call fromString(mass_string%getValue(), errors_, mass)
                    if (errors%hasAny()) then
                        call errors%appendErrors( &
                                errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                    else
                        call json%getElement("material", errors_, material_element)
                        if (errors%hasAny()) then
                            call errors%appendErrors( &
                                    errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                        else
                            select type (material_object => material_element%element)
                            type is (JsonObject_t)
                                call fromJson(material_object, messages_, errors_, material)
                                call messages%appendMessages( &
                                        messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                                if (errors_%hasAny()) then
                                    call errors%appendErrors( &
                                            errors_, &
                                            Module_(MODULE_NAME), &
                                            Procedure_(PROCEDURE_NAME))
                                else
                                    call createMatter(mass, material, errors_, matter)
                                    call errors%appendErrors( &
                                            errors_, &
                                            Module_(MODULE_NAME), &
                                            Procedure_(PROCEDURE_NAME))
                                end if
                            class default
                                call errors%appendError(Fatal( &
                                        INVALID_ARGUMENT_TYPE, &
                                        Module_(MODULE_NAME), &
                                        Procedure_(PROCEDURE_NAME), &
                                        "material must be an object"))
                            end select
                        end if
                    end if
                class default
                    call errors%appendError(Fatal( &
                            INVALID_ARGUMENT_TYPE, &
                            Module_(MODULE_NAME), &
                            Procedure_(PROCEDURE_NAME), &
                            "mass must be a string"))
                end select
            end if
        else
            select type (amount_string => amount_element%element)
            type is (JsonString_t)
                call fromString(amount_string%getValue(), errors_, amount)
                if (errors_%hasAny()) then
                    call errors%appendErrors( &
                            errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                else
                    call json%getElement("material", errors_, material_element)
                    if (errors%hasAny()) then
                        call errors%appendErrors( &
                                errors_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                    else
                        select type (material_object => material_element%element)
                        type is (JsonObject_t)
                            call fromJson(material_object, messages_, errors_, material)
                            call messages%appendMessages( &
                                    messages_, Module_(MODULE_NAME), Procedure_(PROCEDURE_NAME))
                            if (errors_%hasAny()) then
                                call errors%appendErrors( &
                                        errors_, &
                                        Module_(MODULE_NAME), &
                                        Procedure_(PROCEDURE_NAME))
                            else
                                call createMatter(amount, material, errors_, matter)
                                call errors%appendErrors( &
                                        errors_, &
                                        Module_(MODULE_NAME), &
                                        Procedure_(PROCEDURE_NAME))
                            end if
                        class default
                            call errors%appendError(Fatal( &
                                    INVALID_ARGUMENT_TYPE, &
                                    Module_(MODULE_NAME), &
                                    Procedure_(PROCEDURE_NAME), &
                                    "material must be an object"))
                        end select
                    end if
                end if
            class default
                call errors%appendError(Fatal( &
                        INVALID_ARGUMENT_TYPE, &
                        Module_(MODULE_NAME), &
                        Procedure_(PROCEDURE_NAME), &
                        "amount must be a string"))
            end select
        end if
    end subroutine matterFromJson

    pure function combineMatter(matter1, matter2) result(combined)
        type(Matter_t), intent(in) :: matter1
        type(Matter_t), intent(in) :: matter2
        type(Matter_t) :: combined

        combined%amount_ = matter1%amount_ + matter2%amount_
        call combineByAtomFactorsUnsafe( &
                matter1%material, &
                matter1%amount_ / combined%amount_, &
                matter2%material, &
                matter2%amount_ / combined%amount_, &
                combined%material)
    end function combineMatter

    pure function totalAmount(self) result(amount)
        class(Matter_t), intent(in) :: self
        type(Amount_t) :: amount

        amount = self%amount_
    end function totalAmount

    pure function amountChemical(self, chemical) result(amount)
        class(Matter_t), intent(in) :: self
        type(ChemicalSymbol_t), intent(in) :: chemical
        type(Amount_t) :: amount

        amount = self%amount_ * self%material%atomFraction(chemical)
    end function amountChemical

    pure function amountElement(self, element) result(amount)
        class(Matter_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        type(Amount_t) :: amount

        amount = self%material%amount(self%amount_, element)
    end function amountElement

    pure function amountIsotope(self, isotope) result(amount)
        class(Matter_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        type(Amount_t) :: amount

        amount = self%material%amount(self%amount_, isotope)
    end function amountIsotope

    pure function totalMass(self) result(mass)
        class(Matter_t), intent(in) :: self
        type(Mass_t) :: mass

        mass = self%amount_ * self%material%molarMass()
    end function totalMass

    pure function massChemical(self, chemical) result(mass)
        class(Matter_t), intent(in) :: self
        type(ChemicalSymbol_t), intent(in) :: chemical
        type(Mass_t) :: mass

        mass = self%mass() * self%material%weightFraction(chemical)
    end function massChemical

    pure function massElement(self, element) result(mass)
        class(Matter_t), intent(in) :: self
        type(ElementSymbol_t), intent(in) :: element
        type(Mass_t) :: mass

        mass = self%mass() * self%material%weightFraction(element)
    end function massElement

    pure function massIsotope(self, isotope) result(mass)
        class(Matter_t), intent(in) :: self
        type(Isotope_t), intent(in) :: isotope
        type(Mass_t) :: mass

        mass = self%mass() * self%material%weightFraction(isotope)
    end function massIsotope

    pure function toJson(self) result(json)
        class(Matter_t), intent(in) :: self
        type(JsonObject_t) :: json

        type(JsonMember_t) :: members(2)

        members(1) = JsonMemberUnsafe("amount", JsonStringUnsafe(self%amount_%toString()))
        members(2) = JsonMemberUnsafe("material", self%material%toJson())
        json = JsonObject(members)
    end function toJson
end module Matter_m
