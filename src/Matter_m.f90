module Matter_m
    use Chemical_symbol_m, only: ChemicalSymbol_t
    use Element_symbol_m, only: ElementSymbol_t
    use erloff, only: ErrorList_t, Internal, Module_, Procedure_
    use Isotope_m, only: Isotope_t
    use Material_m, only: Material_t, combineByAtomFactorsUnsafe
    use quaff, only: Amount_t, Mass_t, operator(*), operator(/)
    use Utilities_m, only: INVALID_ARGUMENT_TYPE

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

    character(len=*), parameter :: MODULE_NAME = "Matter_m"

    public :: operator(+), createMatter, createMatterUnsafe
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
end module Matter_m
