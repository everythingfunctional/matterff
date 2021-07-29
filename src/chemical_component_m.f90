module chemical_component_m
    use element_m, only: element_t, combine_by_atom_factors_unsafe, find

    implicit none
    private
    public :: chemical_component_t, combine_duplicates

    type :: chemical_component_t
        private
        type(element_t) :: element_
        double precision :: multiplier_
    contains
        private
        procedure, public :: element
        procedure, public :: multiplier
    end type

    interface chemical_component_t
        module procedure constructor
    end interface
contains
    pure function combine_duplicates(components) result(combined)
        type(chemical_component_t), intent(in) :: components(:)
        type(chemical_component_t), allocatable :: combined(:)

        integer :: i

        associate(num_inputs => size(components))
            allocate(combined(1), source = [components(1)])
            do i = 2, num_inputs
                associate( &
                        duplicate_position => &
                                find(components(i)%element_%symbol(), combined%element()))
                    if (duplicate_position == 0) then
                        combined = [combined, components(i)]
                    else
                        associate(total_multiplier => &
                                components(i)%multiplier_ &
                                + combined(duplicate_position)%multiplier_)
                            combined(duplicate_position)%element_ = combine_by_atom_factors_unsafe( &
                                    combined(duplicate_position)%element_, &
                                    combined(duplicate_position)%multiplier_ / total_multiplier, &
                                    components(i)%element_, &
                                    components(i)%multiplier_ / total_multiplier)
                            combined(duplicate_position)%multiplier_ = total_multiplier
                        end associate
                    end if
                end associate
            end do
        end associate
    end function

    elemental function constructor( &
            element, multiplier) result(new_chemical_component)
        type(element_t), intent(in) :: element
        double precision, intent(in) :: multiplier
        type(chemical_component_t) :: new_chemical_component

        new_chemical_component%element_ = element
        new_chemical_component%multiplier_ = multiplier
    end function

    elemental function element(self)
        class(chemical_component_t), intent(in) :: self
        type(element_t) :: element

        element = self%element_
    end function

    elemental function multiplier(self)
        class(chemical_component_t), intent(in) :: self
        double precision :: multiplier

        multiplier = self%multiplier_
    end function
end module
