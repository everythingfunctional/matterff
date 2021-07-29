module element_component_m
    use isotope_m, only: isotope_t, find

    implicit none
    private
    public :: element_component_t, combine_duplicates

    type :: element_component_t
        private
        type(isotope_t) :: isotope_
        double precision :: fraction__
    contains
        private
        procedure, public :: isotope
        procedure, public :: fraction_
    end type

    interface element_component_t
        module procedure constructor
    end interface
contains
    pure function combine_duplicates(components) result(combined)
        type(element_component_t), intent(in) :: components(:)
        type(element_component_t), allocatable :: combined(:)

        integer :: i

        associate(num_inputs => size(components))
            allocate(combined(1), source = [components(1)])
            do i = 2, num_inputs
                associate( &
                        duplicate_position => &
                                find(components(i)%isotope_%symbol(), combined%isotope()))
                    if (duplicate_position == 0) then
                        combined = [combined, components(i)]
                    else
                        combined(duplicate_position)%fraction__ = &
                                combined(duplicate_position)%fraction__ &
                                + components(i)%fraction__
                    end if
                end associate
            end do
        end associate
    end function

    elemental function constructor(isotope, fraction_) result(new_element_component)
        type(isotope_t), intent(in) :: isotope
        double precision, intent(in) :: fraction_
        type(element_component_t) :: new_element_component

        new_element_component%isotope_ = isotope
        new_element_component%fraction__ = fraction_
    end function

    elemental function isotope(self)
        class(element_component_t), intent(in) :: self
        type(isotope_t) :: isotope

        isotope = self%isotope_
    end function

    elemental function fraction_(self)
        class(element_component_t), intent(in) :: self
        double precision :: fraction_

        fraction_ = self%fraction__
    end function
end module
