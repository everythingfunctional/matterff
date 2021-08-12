module matterff_chemical_symbol_m
    use iso_varying_string, only: varying_string
    use jsonff, only: json_array_t, json_element_t
    use matterff_chemical_symbol_component_m, only: chemical_symbol_component_t
    use matterff_element_symbol_m, only: element_symbol_t, Ar, H, He, Kr, N, O, Xe
    use strff, only: join

    implicit none
    private
    public :: &
            chemical_symbol_t, &
            argon_gas_symbol, &
            helium_gas_symbol, &
            hydrogen_gas_symbol, &
            krypton_gas_symbol, &
            nitrogen_gas_symbol, &
            oxygen_gas_symbol, &
            water_symbol, &
            xenon_gas_symbol

    type :: chemical_symbol_t
        private
        type(chemical_symbol_component_t), allocatable :: components_(:)
    contains
        private
        procedure :: chemical_symbol_equals
        generic, public :: operator(==) => chemical_symbol_equals
        procedure, public :: includes
        procedure, public :: to_json
        procedure, public :: to_string
    end type

    interface operator(.in.)
        module procedure component_in_list
    end interface

    interface operator(.notIn.)
        module procedure component_not_in_list
    end interface

    interface operator(.allIn.)
        module procedure all_components_in_list
    end interface

    interface chemical_symbol_t
        module procedure constructor
    end interface
contains
    pure function constructor(components) result(new_chemical_symbol)
        type(chemical_symbol_component_t), intent(in) :: components(:)
        type(chemical_symbol_t) :: new_chemical_symbol

        allocate(new_chemical_symbol%components_, source = components)
    end function

    pure function argon_gas_symbol()
        type(chemical_symbol_t) :: argon_gas_symbol

        allocate(argon_gas_symbol%components_, source = &
                [chemical_symbol_component_t(Ar, 1)])
    end function

    pure function helium_gas_symbol()
        type(chemical_symbol_t) :: helium_gas_symbol

        allocate(helium_gas_symbol%components_, source = &
                [chemical_symbol_component_t(He, 1)])
    end function

    pure function hydrogen_gas_symbol()
        type(chemical_symbol_t) :: hydrogen_gas_symbol

        allocate(hydrogen_gas_symbol%components_, source = &
                [chemical_symbol_component_t(H, 2)])
    end function

    pure function krypton_gas_symbol()
        type(chemical_symbol_t) :: krypton_gas_symbol

        allocate(krypton_gas_symbol%components_, source = &
                [chemical_symbol_component_t(Kr, 1)])
    end function

    pure function nitrogen_gas_symbol()
        type(chemical_symbol_t) :: nitrogen_gas_symbol

        allocate(nitrogen_gas_symbol%components_, source = &
                [chemical_symbol_component_t(N, 2)])
    end function

    pure function oxygen_gas_symbol()
        type(chemical_symbol_t) :: oxygen_gas_symbol

        allocate(oxygen_gas_symbol%components_, source = &
                [chemical_symbol_component_t(O, 2)])
    end function

    pure function water_symbol()
        type(chemical_symbol_t) :: water_symbol

        allocate(water_symbol%components_, source = &
                [ chemical_symbol_component_t(H, 2) &
                , chemical_symbol_component_t(O, 1) &
                ])
    end function

    pure function xenon_gas_symbol()
        type(chemical_symbol_t) :: xenon_gas_symbol

        allocate(xenon_gas_symbol%components_, source = &
                [chemical_symbol_component_t(Xe, 1)])
    end function

    elemental function chemical_symbol_equals(lhs, rhs) result(are_equal)
        class(chemical_symbol_t), intent(in) :: lhs
        type(chemical_symbol_t), intent(in) :: rhs
        logical :: are_equal

        are_equal = &
                size(lhs%components_) == size(rhs%components_) &
                .and. (lhs%components_.allIn.rhs%components_) &
                .and. (rhs%components_.allIn.lhs%components_)
    end function

    elemental function includes(self, element)
        class(chemical_symbol_t), intent(in) :: self
        type(element_symbol_t), intent(in) :: element
        logical :: includes

        includes = any(self%components_%element() == element)
    end function

    function to_json(self) result(json)
        class(chemical_symbol_t), intent(in) :: self
        type(json_array_t) :: json

        json = json_array_t(json_element_t(self%components_%to_json()))
    end function

    elemental function to_string(self) result(string)
        class(chemical_symbol_t), intent(in) :: self
        type(varying_string) :: string

        string = join(self%components_%to_string(), "")
    end function

    pure function component_in_list(item, list) result(is_in)
        type(chemical_symbol_component_t), intent(in) :: item
        type(chemical_symbol_component_t), intent(in) :: list(:)
        logical :: is_in

        integer :: i

        is_in = .false.
        do i = 1, size(list)
            if (item == list(i)) then
                is_in = .true.
                exit
            end if
        end do
    end function

    pure function component_not_in_list(item, list) result(is_not_in)
        type(chemical_symbol_component_t), intent(in) :: item
        type(chemical_symbol_component_t), intent(in) :: list(:)
        logical :: is_not_in

        is_not_in = .not. (item.in.list)
    end function

    pure function all_components_in_list(items, list) result(are_all_in)
        type(chemical_symbol_component_t), intent(in) :: items(:)
        type(chemical_symbol_component_t), intent(in) :: list(:)
        logical :: are_all_in

        integer :: i

        are_all_in = .true.
        do i = 1, size(items)
            if (items(i).notIn.list) then
                are_all_in = .false.
                exit
            end if
        end do
    end function
end module
