module Chemical_component_m
    use Element_m, only: Element_t

    implicit none
    private

    type, public :: ChemicalComponent_t
        private
        type(Element_t), public :: element
        double precision, public :: multiplier
    end type ChemicalComponent_t

    public :: ChemicalComponent
contains
    elemental function ChemicalComponent(element, multiplier)
        type(Element_t), intent(in) :: element
        double precision, intent(in) :: multiplier
        type(ChemicalComponent_t) :: ChemicalComponent

        ChemicalComponent%element = element
        ChemicalComponent%multiplier = multiplier
    end function ChemicalComponent
end module Chemical_component_m
