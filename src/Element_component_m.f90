module Element_component_m
    use Isotope_m, only: Isotope_t

    implicit none
    private

    type, public :: ElementComponent_t
        private
        type(Isotope_t) :: isotope
        double precision :: fraction
    end type ElementComponent_t

    public :: ElementComponent
contains
    elemental function ElementComponent(isotope, fraction)
        type(Isotope_t), intent(in) :: isotope
        double precision, intent(in) :: fraction
        type(ElementComponent_t) :: ElementComponent

        ElementComponent%isotope = isotope
        ElementComponent%fraction = fraction
    end function ElementComponent
end module Element_component_m
