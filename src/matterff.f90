module matterff
    use matterff_chemical_m, only: &
            chemical_t, &
            chemical_unsafe, &
            combine_by_atom_factors_unsafe, &
            combine_by_weight_factors_unsafe, &
            find, &
            natural_argon_gas, &
            natural_helium_gas, &
            natural_hydrogen_gas, &
            natural_krypton_gas, &
            natural_nitrogen_gas, &
            natural_oxygen_gas, &
            natural_water, &
            natural_xenon_gas
    use matterff_chemical_component_m, only: &
            chemical_component_t, combine_duplicates
    use matterff_chemical_symbol_m, only: &
            chemical_symbol_t, &
            argon_gas_symbol, &
            helium_gas_symbol, &
            hydrogen_gas_symbol, &
            krypton_gas_symbol, &
            nitrogen_gas_symbol, &
            oxygen_gas_symbol, &
            water_symbol, &
            xenon_gas_symbol
    use matterff_chemical_symbol_component_m, only: chemical_symbol_component_t
    use matterff_element_m, only: &
            element_t, &
            combine_by_atom_factors_unsafe, &
            combine_by_weight_factors_unsafe, &
            find, &
            from_atom_fractions_unsafe, &
            from_weight_fractions_unsafe, &
            natural_hydrogen, &
            natural_helium, &
            natural_lithium, &
            natural_beryllium, &
            natural_boron, &
            natural_carbon, &
            natural_nitrogen, &
            natural_oxygen, &
            natural_argon, &
            natural_krypton, &
            natural_xenon
    use matterff_element_component_m, only: &
            element_component_t, combine_duplicates
    use matterff_element_symbol_m, only: &
            element_symbol_t, &
            H, &
            He, &
            Li, &
            Be, &
            B, &
            C, &
            N, &
            O, &
            Ar, &
            Kr, &
            Xe
    use matterff_fallible_chemical_m, only: &
            fallible_chemical_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors
    use matterff_fallible_chemical_component_m, only: &
            fallible_chemical_component_t
    use matterff_fallible_chemical_components_m, only: &
            fallible_chemical_components_t
    use matterff_fallible_chemical_symbol_m, only: fallible_chemical_symbol_t
    use matterff_fallbile_chemical_symbol_component_m, only: &
            fallible_chemical_symbol_component_t
    use matterff_fallible_element_m, only: &
            fallible_element_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors, &
            from_atom_fractions, &
            from_weight_fractions
    use matterff_fallible_element_component_m, only: &
            fallible_element_component_t
    use matterff_fallible_element_components_m, only: &
            fallible_element_components_t
    use matterff_fallible_isotope_m, only: fallible_isotope_t
    use matterff_fallible_material_m, only: &
            fallible_material_t, &
            combine_by_atom_factors, &
            combine_by_weight_factors, &
            from_atom_fractions, &
            from_weight_fractions
    use matterff_fallible_material_component_m, only: &
            fallible_material_component_t
    use matterff_fallible_material_components_m, only: &
            fallible_material_components_t
    use matterff_fallible_matter_m, only: fallible_matter_t
    use matterff_isotope_m, only: &
            isotope_t, &
            find, &
            H_1, &
            H_2, &
            H_3, &
            He_3, &
            He_4, &
            Li_6, &
            Li_7, &
            Be_9, &
            B_10, &
            B_11, &
            C_12, &
            C_13, &
            N_14, &
            N_15, &
            O_16, &
            O_17, &
            O_18, &
            Ar_36, &
            Ar_38, &
            Ar_40, &
            Kr_78, &
            Kr_80, &
            Kr_82, &
            Kr_83, &
            Kr_84, &
            Kr_85, &
            Kr_86, &
            Xe_124, &
            Xe_126, &
            Xe_128, &
            Xe_129, &
            Xe_130, &
            Xe_131, &
            Xe_132, &
            Xe_134, &
            Xe_136
    use matterff_isotope_symbol_m, only: &
            isotope_symbol_t, &
            H_1_SYM, &
            H_2_SYM, &
            H_3_SYM, &
            He_3_SYM, &
            He_4_SYM, &
            Li_6_SYM, &
            Li_7_SYM, &
            Be_9_SYM, &
            B_10_SYM, &
            B_11_SYM, &
            C_12_SYM, &
            C_13_SYM, &
            N_14_SYM, &
            N_15_SYM, &
            O_16_SYM, &
            O_17_SYM, &
            O_18_SYM, &
            Ar_36_SYM, &
            Ar_38_SYM, &
            Ar_40_SYM, &
            Kr_78_SYM, &
            Kr_80_SYM, &
            Kr_82_SYM, &
            Kr_83_SYM, &
            Kr_84_SYM, &
            Kr_85_SYM, &
            Kr_86_SYM, &
            Xe_124_SYM, &
            Xe_126_SYM, &
            Xe_128_SYM, &
            Xe_129_SYM, &
            Xe_130_SYM, &
            Xe_131_SYM, &
            Xe_132_SYM, &
            Xe_134_SYM, &
            Xe_136_SYM
    use matterff_material_m, only: &
            material_t, &
            combine_by_atom_factors_unsafe, &
            combine_by_weight_factors_unsafe, &
            from_atom_fractions_unsafe, &
            from_weight_fractions_unsafe, &
            pure_natural_argon_gas, &
            pure_natural_helium_gas, &
            pure_natural_hydrogen_gas, &
            pure_natural_krypton_gas, &
            pure_natural_nitrogen_gas, &
            pure_natural_oxygen_gas, &
            pure_natural_water, &
            pure_natural_xenon_gas
    use matterff_material_component_m, only: &
            material_component_t, combine_duplicates
    use matterff_matter_m, only: matter_t, matter_unsafe
    use matterff_utilities_m, only: &
            INVALID_ARGUMENT, &
            MISMATCH, &
            NORMALIZED_FRACTIONS
end module
