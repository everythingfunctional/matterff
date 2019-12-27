module Material_component_m
    use Chemical_m, only: Chemical_t

    implicit none
    private

    type, public :: MaterialComponent_t
        private
        type(Chemical_t), public :: chemical
        double precision, public :: fraction
    end type MaterialComponent_t

    public :: MaterialComponent
contains
    elemental function MaterialComponent(chemical, fraction)
        type(Chemical_t), intent(in) :: chemical
        double precision, intent(in) :: fraction
        type(MaterialComponent_t) :: MaterialComponent

        MaterialComponent%chemical = chemical
        MaterialComponent%fraction = fraction
    end function MaterialComponent
end module Material_component_m
