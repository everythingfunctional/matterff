module Element_m
    use Element_component_m, only: ElementComponent_t
    use Isotope_symbol_m, only: IsotopeSymbol_t

    implicit none
    private

    type, public :: Element_t
        private
        type(ElementComponent_t), allocatable :: components(:)
    contains
        private
        procedure, public :: atomFraction
        procedure, public :: weightFraction
    end type Element_t
contains
    elemental function atomFraction(self, isotope)
        class(Element_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: atomFraction

        associate(a => self, b => isotope)
        end associate

        atomFraction = 0.0d0
    end function atomFraction

    elemental function weightFraction(self, isotope)
        class(Element_t), intent(in) :: self
        type(IsotopeSymbol_t), intent(in) :: isotope
        double precision :: weightFraction

        associate(a => self, b => isotope)
        end associate

        weightFraction = 0.0d0
    end function weightFraction
end module Element_m
