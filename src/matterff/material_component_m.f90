module matterff_material_component_m
    use matterff_chemical_m, only: &
            chemical_t, combine_by_atom_factors_unsafe, find

    implicit none
    private
    public :: material_component_t, combine_duplicates

    type :: material_component_t
        private
        type(chemical_t) :: chemical_
        double precision :: fraction__
    contains
        private
        procedure, public :: chemical
        procedure, public :: fraction_
    end type

    interface material_component_t
        module procedure constructor
    end interface
contains
    pure function combine_duplicates(components) result(combined)
        type(material_component_t), intent(in) :: components(:)
        type(material_component_t), allocatable :: combined(:)

        integer :: i

        associate(num_inputs => size(components))
            allocate(combined(1), source = [components(1)])
            do i = 2, num_inputs
                associate( &
                        duplicate_position => &
                                find(components(i)%chemical_%symbol(), combined%chemical()))
                    if (duplicate_position == 0) then
                        combined = [combined, components(i)]
                    else
                        associate(total_fraction => &
                                components(i)%fraction__ &
                                + combined(duplicate_position)%fraction__)
                            combined(duplicate_position)%chemical_ = combine_by_atom_factors_unsafe( &
                                    combined(duplicate_position)%chemical_, &
                                    combined(duplicate_position)%fraction__ / total_fraction, &
                                    components(i)%chemical_, &
                                    components(i)%fraction__ / total_fraction)
                            combined(duplicate_position)%fraction__ = total_fraction
                        end associate
                    end if
                end associate
            end do
        end associate
    end function

    elemental function constructor(chemical, fraction_) result(new_material_component)
        type(chemical_t), intent(in) :: chemical
        double precision, intent(in) :: fraction_
        type(material_component_t) :: new_material_component

        new_material_component%chemical_ = chemical
        new_material_component%fraction__ = fraction_
    end function

    elemental function chemical(self)
        class(material_component_t), intent(in) :: self
        type(chemical_t) :: chemical

        chemical = self%chemical_
    end function

    elemental function fraction_(self)
        class(material_component_t), intent(in) :: self
        double precision :: fraction_

        fraction_ = self%fraction__
    end function
end module
