-- Copyright (c) 2013 Eric McCorkle.  All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Module containing constants for the Dwarf debugging format.
module Data.Dwarf where

import Data.Word

#include <dwarf.h>

dwTag_array_type :: Word
dwTag_array_type = (#const DW_TAG_array_type)

dwTag_class_type :: Word
dwTag_class_type = (#const DW_TAG_class_type)

dwTag_entry_point :: Word
dwTag_entry_point = (#const DW_TAG_entry_point)

dwTag_enumeration_type :: Word
dwTag_enumeration_type = (#const DW_TAG_enumeration_type)

dwTag_formal_parameter :: Word
dwTag_formal_parameter = (#const DW_TAG_formal_parameter)

dwTag_imported_declaration :: Word
dwTag_imported_declaration = (#const DW_TAG_imported_declaration)

dwTag_label :: Word
dwTag_label = (#const DW_TAG_label)

dwTag_lexical_block :: Word
dwTag_lexical_block = (#const DW_TAG_lexical_block)

dwTag_member :: Word
dwTag_member = (#const DW_TAG_member)

dwTag_pointer_type :: Word
dwTag_pointer_type = (#const DW_TAG_pointer_type)

dwTag_reference_type :: Word
dwTag_reference_type = (#const DW_TAG_reference_type)

dwTag_compile_unit :: Word
dwTag_compile_unit = (#const DW_TAG_compile_unit)

dwTag_string_type :: Word
dwTag_string_type = (#const DW_TAG_string_type)

dwTag_structure_type :: Word
dwTag_structure_type = (#const DW_TAG_structure_type)

dwTag_subroutine_type :: Word
dwTag_subroutine_type = (#const DW_TAG_subroutine_type)

dwTag_typedef :: Word
dwTag_typedef = (#const DW_TAG_typedef)

dwTag_union_type :: Word
dwTag_union_type = (#const DW_TAG_union_type)

dwTag_unspecified_parameters :: Word
dwTag_unspecified_parameters = (#const DW_TAG_unspecified_parameters)

dwTag_variant :: Word
dwTag_variant = (#const DW_TAG_variant)

dwTag_common_block :: Word
dwTag_common_block = (#const DW_TAG_common_block)

dwTag_common_inclusion :: Word
dwTag_common_inclusion = (#const DW_TAG_common_inclusion)

dwTag_inheritance :: Word
dwTag_inheritance = (#const DW_TAG_inheritance)

dwTag_inlined_subroutine :: Word
dwTag_inlined_subroutine = (#const DW_TAG_inlined_subroutine)

dwTag_module :: Word
dwTag_module = (#const DW_TAG_module)

dwTag_ptr_to_member_type :: Word
dwTag_ptr_to_member_type = (#const DW_TAG_ptr_to_member_type)

dwTag_set_type :: Word
dwTag_set_type = (#const DW_TAG_set_type)

dwTag_subrange_type :: Word
dwTag_subrange_type = (#const DW_TAG_subrange_type)

dwTag_with_stmt :: Word
dwTag_with_stmt = (#const DW_TAG_with_stmt)

dwTag_access_declaration :: Word
dwTag_access_declaration = (#const DW_TAG_access_declaration)

dwTag_base_type :: Word
dwTag_base_type = (#const DW_TAG_base_type)

dwTag_catch_block :: Word
dwTag_catch_block = (#const DW_TAG_catch_block)

dwTag_const_type :: Word
dwTag_const_type = (#const DW_TAG_const_type)

dwTag_constant :: Word
dwTag_constant = (#const DW_TAG_constant)

dwTag_enumerator :: Word
dwTag_enumerator = (#const DW_TAG_enumerator)

dwTag_file_type :: Word
dwTag_file_type = (#const DW_TAG_file_type)

dwTag_friend :: Word
dwTag_friend = (#const DW_TAG_friend)

dwTag_namelist :: Word
dwTag_namelist = (#const DW_TAG_namelist)

dwTag_namelist_item :: Word
dwTag_namelist_item = (#const DW_TAG_namelist_item)

dwTag_packed_type :: Word
dwTag_packed_type = (#const DW_TAG_packed_type)

dwTag_subprogram :: Word
dwTag_subprogram = (#const DW_TAG_subprogram)

dwTag_template_type_parameter :: Word
dwTag_template_type_parameter = (#const DW_TAG_template_type_parameter)

dwTag_template_value_parameter :: Word
dwTag_template_value_parameter = (#const DW_TAG_template_value_parameter)

dwTag_thrown_type :: Word
dwTag_thrown_type = (#const DW_TAG_thrown_type)

dwTag_try_block :: Word
dwTag_try_block = (#const DW_TAG_try_block)

dwTag_variant_part :: Word
dwTag_variant_part = (#const DW_TAG_variant_part)

dwTag_variable :: Word
dwTag_variable = (#const DW_TAG_variable)

dwTag_volatile_type :: Word
dwTag_volatile_type = (#const DW_TAG_volatile_type)

dwTag_dwarf_procedure :: Word
dwTag_dwarf_procedure = (#const DW_TAG_dwarf_procedure)

dwTag_restrict_type :: Word
dwTag_restrict_type = (#const DW_TAG_restrict_type)

dwTag_interface_type :: Word
dwTag_interface_type = (#const DW_TAG_interface_type)

dwTag_namespace :: Word
dwTag_namespace = (#const DW_TAG_namespace)

dwTag_imported_module :: Word
dwTag_imported_module = (#const DW_TAG_imported_module)

dwTag_unspecified_type :: Word
dwTag_unspecified_type = (#const DW_TAG_unspecified_type)

dwTag_partial_unit :: Word
dwTag_partial_unit = (#const DW_TAG_partial_unit)

dwTag_imported_unit :: Word
dwTag_imported_unit = (#const DW_TAG_imported_unit)

dwTag_mutable_type :: Word
dwTag_mutable_type = (#const DW_TAG_mutable_type)

dwTag_condition :: Word
dwTag_condition = (#const DW_TAG_condition)

dwTag_shared_type :: Word
dwTag_shared_type = (#const DW_TAG_shared_type)

dwTag_type_unit :: Word
dwTag_type_unit = (#const DW_TAG_type_unit)

dwTag_rvalue_reference_type :: Word
dwTag_rvalue_reference_type = (#const DW_TAG_rvalue_reference_type)

dwTag_template_alias :: Word
dwTag_template_alias = (#const DW_TAG_template_alias)

dwTag_lo_user :: Word
dwTag_lo_user = (#const DW_TAG_lo_user)

dwTag_MIPS_loop :: Word
dwTag_MIPS_loop = (#const DW_TAG_MIPS_loop)

dwTag_format_label :: Word
dwTag_format_label = (#const DW_TAG_format_label)

dwTag_function_template :: Word
dwTag_function_template = (#const DW_TAG_function_template)

dwTag_class_template :: Word
dwTag_class_template = (#const DW_TAG_class_template)

dwTag_GNU_BINCL :: Word
dwTag_GNU_BINCL = (#const DW_TAG_GNU_BINCL)

dwTag_GNU_EINCL :: Word
dwTag_GNU_EINCL = (#const DW_TAG_GNU_EINCL)

dwTag_GNU_template_template_param :: Word
dwTag_GNU_template_template_param = (#const DW_TAG_GNU_template_template_param)

dwTag_GNU_template_parameter_pack :: Word
dwTag_GNU_template_parameter_pack = (#const DW_TAG_GNU_template_parameter_pack)

dwTag_GNU_formal_parameter_pack :: Word
dwTag_GNU_formal_parameter_pack = (#const DW_TAG_GNU_formal_parameter_pack)

dwTag_hi_user :: Word
dwTag_hi_user = (#const DW_TAG_hi_user)

dwChildren_no :: Word
dwChildren_no = (#const DW_CHILDREN_no)

dwChildren_yes :: Word
dwChildren_yes = (#const DW_CHILDREN_yes)

dwAt_sibling :: Word
dwAt_sibling = (#const DW_AT_sibling)

dwAt_location :: Word
dwAt_location = (#const DW_AT_location)

dwAt_name :: Word
dwAt_name = (#const DW_AT_name)

dwAt_ordering :: Word
dwAt_ordering = (#const DW_AT_ordering)

dwAt_subscr_data :: Word
dwAt_subscr_data = (#const DW_AT_subscr_data)

dwAt_byte_size :: Word
dwAt_byte_size = (#const DW_AT_byte_size)

dwAt_bit_offset :: Word
dwAt_bit_offset = (#const DW_AT_bit_offset)

dwAt_bit_size :: Word
dwAt_bit_size = (#const DW_AT_bit_size)

dwAt_element_list :: Word
dwAt_element_list = (#const DW_AT_element_list)

dwAt_stmt_list :: Word
dwAt_stmt_list = (#const DW_AT_stmt_list)

dwAt_low_pc :: Word
dwAt_low_pc = (#const DW_AT_low_pc)

dwAt_high_pc :: Word
dwAt_high_pc = (#const DW_AT_high_pc)

dwAt_language :: Word
dwAt_language = (#const DW_AT_language)

dwAt_member :: Word
dwAt_member = (#const DW_AT_member)

dwAt_discr :: Word
dwAt_discr = (#const DW_AT_discr)

dwAt_discr_value :: Word
dwAt_discr_value = (#const DW_AT_discr_value)

dwAt_visibility :: Word
dwAt_visibility = (#const DW_AT_visibility)

dwAt_import :: Word
dwAt_import = (#const DW_AT_import)

dwAt_string_length :: Word
dwAt_string_length = (#const DW_AT_string_length)

dwAt_common_reference :: Word
dwAt_common_reference = (#const DW_AT_common_reference)

dwAt_comp_dir :: Word
dwAt_comp_dir = (#const DW_AT_comp_dir)

dwAt_const_value :: Word
dwAt_const_value = (#const DW_AT_const_value)

dwAt_containing_type :: Word
dwAt_containing_type = (#const DW_AT_containing_type)

dwAt_default_value :: Word
dwAt_default_value = (#const DW_AT_default_value)

dwAt_inline :: Word
dwAt_inline = (#const DW_AT_inline)

dwAt_is_optional :: Word
dwAt_is_optional = (#const DW_AT_is_optional)

dwAt_lower_bound :: Word
dwAt_lower_bound = (#const DW_AT_lower_bound)

dwAt_producer :: Word
dwAt_producer = (#const DW_AT_producer)

dwAt_prototyped :: Word
dwAt_prototyped = (#const DW_AT_prototyped)

dwAt_return_addr :: Word
dwAt_return_addr = (#const DW_AT_return_addr)

dwAt_start_scope :: Word
dwAt_start_scope = (#const DW_AT_start_scope)

dwAt_bit_stride :: Word
dwAt_bit_stride = (#const DW_AT_bit_stride)

dwAt_upper_bound :: Word
dwAt_upper_bound = (#const DW_AT_upper_bound)

dwAt_abstract_origin :: Word
dwAt_abstract_origin = (#const DW_AT_abstract_origin)

dwAt_accessibility :: Word
dwAt_accessibility = (#const DW_AT_accessibility)

dwAt_address_class :: Word
dwAt_address_class = (#const DW_AT_address_class)

dwAt_artificial :: Word
dwAt_artificial = (#const DW_AT_artificial)

dwAt_base_types :: Word
dwAt_base_types = (#const DW_AT_base_types)

dwAt_calling_convention :: Word
dwAt_calling_convention = (#const DW_AT_calling_convention)

dwAt_count :: Word
dwAt_count = (#const DW_AT_count)

dwAt_data_member_location :: Word
dwAt_data_member_location = (#const DW_AT_data_member_location)

dwAt_decl_column :: Word
dwAt_decl_column = (#const DW_AT_decl_column)

dwAt_decl_file :: Word
dwAt_decl_file = (#const DW_AT_decl_file)

dwAt_decl_line :: Word
dwAt_decl_line = (#const DW_AT_decl_line)

dwAt_declaration :: Word
dwAt_declaration = (#const DW_AT_declaration)

dwAt_discr_list :: Word
dwAt_discr_list = (#const DW_AT_discr_list)

dwAt_encoding :: Word
dwAt_encoding = (#const DW_AT_encoding)

dwAt_external :: Word
dwAt_external = (#const DW_AT_external)

dwAt_frame_base :: Word
dwAt_frame_base = (#const DW_AT_frame_base)

dwAt_friend :: Word
dwAt_friend = (#const DW_AT_friend)

dwAt_identifier_case :: Word
dwAt_identifier_case = (#const DW_AT_identifier_case)

dwAt_macro_info :: Word
dwAt_macro_info = (#const DW_AT_macro_info)

dwAt_namelist_item :: Word
dwAt_namelist_item = (#const DW_AT_namelist_item)

dwAt_priority :: Word
dwAt_priority = (#const DW_AT_priority)

dwAt_segment :: Word
dwAt_segment = (#const DW_AT_segment)

dwAt_specification :: Word
dwAt_specification = (#const DW_AT_specification)

dwAt_static_link :: Word
dwAt_static_link = (#const DW_AT_static_link)

dwAt_type :: Word
dwAt_type = (#const DW_AT_type)

dwAt_use_location :: Word
dwAt_use_location = (#const DW_AT_use_location)

dwAt_variable_parameter :: Word
dwAt_variable_parameter = (#const DW_AT_variable_parameter)

dwAt_virtuality :: Word
dwAt_virtuality = (#const DW_AT_virtuality)

dwAt_vtable_elem_location :: Word
dwAt_vtable_elem_location = (#const DW_AT_vtable_elem_location)

dwAt_allocated :: Word
dwAt_allocated = (#const DW_AT_allocated)

dwAt_associated :: Word
dwAt_associated = (#const DW_AT_associated)

dwAt_data_location :: Word
dwAt_data_location = (#const DW_AT_data_location)

dwAt_byte_stride :: Word
dwAt_byte_stride = (#const DW_AT_byte_stride)

dwAt_entry_pc :: Word
dwAt_entry_pc = (#const DW_AT_entry_pc)

dwAt_use_UTF8 :: Word
dwAt_use_UTF8 = (#const DW_AT_use_UTF8)

dwAt_extension :: Word
dwAt_extension = (#const DW_AT_extension)

dwAt_ranges :: Word
dwAt_ranges = (#const DW_AT_ranges)

dwAt_trampoline :: Word
dwAt_trampoline = (#const DW_AT_trampoline)

dwAt_call_column :: Word
dwAt_call_column = (#const DW_AT_call_column)

dwAt_call_file :: Word
dwAt_call_file = (#const DW_AT_call_file)

dwAt_call_line :: Word
dwAt_call_line = (#const DW_AT_call_line)

dwAt_description :: Word
dwAt_description = (#const DW_AT_description)

dwAt_binary_scale :: Word
dwAt_binary_scale = (#const DW_AT_binary_scale)

dwAt_decimal_scale :: Word
dwAt_decimal_scale = (#const DW_AT_decimal_scale)

dwAt_small :: Word
dwAt_small = (#const DW_AT_small)

dwAt_decimal_sign :: Word
dwAt_decimal_sign = (#const DW_AT_decimal_sign)

dwAt_digit_count :: Word
dwAt_digit_count = (#const DW_AT_digit_count)

dwAt_picture_string :: Word
dwAt_picture_string = (#const DW_AT_picture_string)

dwAt_mutable :: Word
dwAt_mutable = (#const DW_AT_mutable)

dwAt_threads_scaled :: Word
dwAt_threads_scaled = (#const DW_AT_threads_scaled)

dwAt_explicit :: Word
dwAt_explicit = (#const DW_AT_explicit)

dwAt_object_pointer :: Word
dwAt_object_pointer = (#const DW_AT_object_pointer)

dwAt_endianity :: Word
dwAt_endianity = (#const DW_AT_endianity)

dwAt_elemental :: Word
dwAt_elemental = (#const DW_AT_elemental)

dwAt_pure :: Word
dwAt_pure = (#const DW_AT_pure)

dwAt_recursive :: Word
dwAt_recursive = (#const DW_AT_recursive)

dwAt_signature :: Word
dwAt_signature = (#const DW_AT_signature)

dwAt_main_subprogram :: Word
dwAt_main_subprogram = (#const DW_AT_main_subprogram)

dwAt_data_bit_offset :: Word
dwAt_data_bit_offset = (#const DW_AT_data_bit_offset)

dwAt_const_expr :: Word
dwAt_const_expr = (#const DW_AT_const_expr)

dwAt_enum_class :: Word
dwAt_enum_class = (#const DW_AT_enum_class)

dwAt_linkage_name :: Word
dwAt_linkage_name = (#const DW_AT_linkage_name)

dwAt_lo_user :: Word
dwAt_lo_user = (#const DW_AT_lo_user)

dwAt_MIPS_fde :: Word
dwAt_MIPS_fde = (#const DW_AT_MIPS_fde)

dwAt_MIPS_loop_begin :: Word
dwAt_MIPS_loop_begin = (#const DW_AT_MIPS_loop_begin)

dwAt_MIPS_tail_loop_begin :: Word
dwAt_MIPS_tail_loop_begin = (#const DW_AT_MIPS_tail_loop_begin)

dwAt_MIPS_epilog_begin :: Word
dwAt_MIPS_epilog_begin = (#const DW_AT_MIPS_epilog_begin)

dwAt_MIPS_loop_unroll_factor :: Word
dwAt_MIPS_loop_unroll_factor = (#const DW_AT_MIPS_loop_unroll_factor)

dwAt_MIPS_software_pipeline_depth :: Word
dwAt_MIPS_software_pipeline_depth = (#const DW_AT_MIPS_software_pipeline_depth)

dwAt_MIPS_linkage_name :: Word
dwAt_MIPS_linkage_name = (#const DW_AT_MIPS_linkage_name)

dwAt_MIPS_stride :: Word
dwAt_MIPS_stride = (#const DW_AT_MIPS_stride)

dwAt_MIPS_abstract_name :: Word
dwAt_MIPS_abstract_name = (#const DW_AT_MIPS_abstract_name)

dwAt_MIPS_clone_origin :: Word
dwAt_MIPS_clone_origin = (#const DW_AT_MIPS_clone_origin)

dwAt_MIPS_has_inlines :: Word
dwAt_MIPS_has_inlines = (#const DW_AT_MIPS_has_inlines)

dwAt_MIPS_stride_byte :: Word
dwAt_MIPS_stride_byte = (#const DW_AT_MIPS_stride_byte)

dwAt_MIPS_stride_elem :: Word
dwAt_MIPS_stride_elem = (#const DW_AT_MIPS_stride_elem)

dwAt_MIPS_ptr_dopetype :: Word
dwAt_MIPS_ptr_dopetype = (#const DW_AT_MIPS_ptr_dopetype)

dwAt_MIPS_allocatable_dopetype :: Word
dwAt_MIPS_allocatable_dopetype = (#const DW_AT_MIPS_allocatable_dopetype)

dwAt_MIPS_assumed_shape_dopetype :: Word
dwAt_MIPS_assumed_shape_dopetype = (#const DW_AT_MIPS_assumed_shape_dopetype)

dwAt_MIPS_assumed_size :: Word
dwAt_MIPS_assumed_size = (#const DW_AT_MIPS_assumed_size)

dwAt_sf_names :: Word
dwAt_sf_names = (#const DW_AT_sf_names)

dwAt_src_info :: Word
dwAt_src_info = (#const DW_AT_src_info)

dwAt_mac_info :: Word
dwAt_mac_info = (#const DW_AT_mac_info)

dwAt_src_coords :: Word
dwAt_src_coords = (#const DW_AT_src_coords)

dwAt_body_begin :: Word
dwAt_body_begin = (#const DW_AT_body_begin)

dwAt_body_end :: Word
dwAt_body_end = (#const DW_AT_body_end)

dwAt_GNU_vector :: Word
dwAt_GNU_vector = (#const DW_AT_GNU_vector)

dwAt_GNU_guarded_by :: Word
dwAt_GNU_guarded_by = (#const DW_AT_GNU_guarded_by)

dwAt_GNU_pt_guarded_by :: Word
dwAt_GNU_pt_guarded_by = (#const DW_AT_GNU_pt_guarded_by)

dwAt_GNU_guarded :: Word
dwAt_GNU_guarded = (#const DW_AT_GNU_guarded)

dwAt_GNU_pt_guarded :: Word
dwAt_GNU_pt_guarded = (#const DW_AT_GNU_pt_guarded)

dwAt_GNU_locks_excluded :: Word
dwAt_GNU_locks_excluded = (#const DW_AT_GNU_locks_excluded)

dwAt_GNU_exclusive_locks_required :: Word
dwAt_GNU_exclusive_locks_required = (#const DW_AT_GNU_exclusive_locks_required)

dwAt_GNU_shared_locks_required :: Word
dwAt_GNU_shared_locks_required = (#const DW_AT_GNU_shared_locks_required)

dwAt_GNU_odr_signature :: Word
dwAt_GNU_odr_signature = (#const DW_AT_GNU_odr_signature)

dwAt_GNU_template_name :: Word
dwAt_GNU_template_name = (#const DW_AT_GNU_template_name)

dwAt_hi_user :: Word
dwAt_hi_user = (#const DW_AT_hi_user)

dwForm_addr :: Word
dwForm_addr = (#const DW_FORM_addr)

dwForm_block2 :: Word
dwForm_block2 = (#const DW_FORM_block2)

dwForm_block4 :: Word
dwForm_block4 = (#const DW_FORM_block4)

dwForm_data2 :: Word
dwForm_data2 = (#const DW_FORM_data2)

dwForm_data4 :: Word
dwForm_data4 = (#const DW_FORM_data4)

dwForm_data8 :: Word
dwForm_data8 = (#const DW_FORM_data8)

dwForm_string :: Word
dwForm_string = (#const DW_FORM_string)

dwForm_block :: Word
dwForm_block = (#const DW_FORM_block)

dwForm_block1 :: Word
dwForm_block1 = (#const DW_FORM_block1)

dwForm_data1 :: Word
dwForm_data1 = (#const DW_FORM_data1)

dwForm_flag :: Word
dwForm_flag = (#const DW_FORM_flag)

dwForm_sdata :: Word
dwForm_sdata = (#const DW_FORM_sdata)

dwForm_strp :: Word
dwForm_strp = (#const DW_FORM_strp)

dwForm_udata :: Word
dwForm_udata = (#const DW_FORM_udata)

dwForm_ref_addr :: Word
dwForm_ref_addr = (#const DW_FORM_ref_addr)

dwForm_ref1 :: Word
dwForm_ref1 = (#const DW_FORM_ref1)

dwForm_ref2 :: Word
dwForm_ref2 = (#const DW_FORM_ref2)

dwForm_ref4 :: Word
dwForm_ref4 = (#const DW_FORM_ref4)

dwForm_ref8 :: Word
dwForm_ref8 = (#const DW_FORM_ref8)

dwForm_ref_udata :: Word
dwForm_ref_udata = (#const DW_FORM_ref_udata)

dwForm_indirect :: Word
dwForm_indirect = (#const DW_FORM_indirect)

dwForm_sec_offset :: Word
dwForm_sec_offset = (#const DW_FORM_sec_offset)

dwForm_exprloc :: Word
dwForm_exprloc = (#const DW_FORM_exprloc)

dwForm_flag_present :: Word
dwForm_flag_present = (#const DW_FORM_flag_present)

dwForm_ref_sig8 :: Word
dwForm_ref_sig8 = (#const DW_FORM_ref_sig8)

dwOp_addr :: Word
dwOp_addr = (#const DW_OP_addr)

dwOp_deref :: Word
dwOp_deref = (#const DW_OP_deref)

dwOp_const1u :: Word
dwOp_const1u = (#const DW_OP_const1u)

dwOp_const1s :: Word
dwOp_const1s = (#const DW_OP_const1s)

dwOp_const2u :: Word
dwOp_const2u = (#const DW_OP_const2u)

dwOp_const2s :: Word
dwOp_const2s = (#const DW_OP_const2s)

dwOp_const4u :: Word
dwOp_const4u = (#const DW_OP_const4u)

dwOp_const4s :: Word
dwOp_const4s = (#const DW_OP_const4s)

dwOp_const8u :: Word
dwOp_const8u = (#const DW_OP_const8u)

dwOp_const8s :: Word
dwOp_const8s = (#const DW_OP_const8s)

dwOp_constu :: Word
dwOp_constu = (#const DW_OP_constu)

dwOp_consts :: Word
dwOp_consts = (#const DW_OP_consts)

dwOp_dup :: Word
dwOp_dup = (#const DW_OP_dup)

dwOp_drop :: Word
dwOp_drop = (#const DW_OP_drop)

dwOp_over :: Word
dwOp_over = (#const DW_OP_over)

dwOp_pick :: Word
dwOp_pick = (#const DW_OP_pick)

dwOp_swap :: Word
dwOp_swap = (#const DW_OP_swap)

dwOp_rot :: Word
dwOp_rot = (#const DW_OP_rot)

dwOp_xderef :: Word
dwOp_xderef = (#const DW_OP_xderef)

dwOp_abs :: Word
dwOp_abs = (#const DW_OP_abs)

dwOp_and :: Word
dwOp_and = (#const DW_OP_and)

dwOp_div :: Word
dwOp_div = (#const DW_OP_div)

dwOp_minus :: Word
dwOp_minus = (#const DW_OP_minus)

dwOp_mod :: Word
dwOp_mod = (#const DW_OP_mod)

dwOp_mul :: Word
dwOp_mul = (#const DW_OP_mul)

dwOp_neg :: Word
dwOp_neg = (#const DW_OP_neg)

dwOp_not :: Word
dwOp_not = (#const DW_OP_not)

dwOp_or :: Word
dwOp_or = (#const DW_OP_or)

dwOp_plus :: Word
dwOp_plus = (#const DW_OP_plus)

dwOp_plus_uconst :: Word
dwOp_plus_uconst = (#const DW_OP_plus_uconst)

dwOp_shl :: Word
dwOp_shl = (#const DW_OP_shl)

dwOp_shr :: Word
dwOp_shr = (#const DW_OP_shr)

dwOp_shra :: Word
dwOp_shra = (#const DW_OP_shra)

dwOp_xor :: Word
dwOp_xor = (#const DW_OP_xor)

dwOp_bra :: Word
dwOp_bra = (#const DW_OP_bra)

dwOp_eq :: Word
dwOp_eq = (#const DW_OP_eq)

dwOp_ge :: Word
dwOp_ge = (#const DW_OP_ge)

dwOp_gt :: Word
dwOp_gt = (#const DW_OP_gt)

dwOp_le :: Word
dwOp_le = (#const DW_OP_le)

dwOp_lt :: Word
dwOp_lt = (#const DW_OP_lt)

dwOp_ne :: Word
dwOp_ne = (#const DW_OP_ne)

dwOp_skip :: Word
dwOp_skip = (#const DW_OP_skip)

dwOp_lit0 :: Word
dwOp_lit0 = (#const DW_OP_lit0)

dwOp_lit1 :: Word
dwOp_lit1 = (#const DW_OP_lit1)

dwOp_lit2 :: Word
dwOp_lit2 = (#const DW_OP_lit2)

dwOp_lit3 :: Word
dwOp_lit3 = (#const DW_OP_lit3)

dwOp_lit4 :: Word
dwOp_lit4 = (#const DW_OP_lit4)

dwOp_lit5 :: Word
dwOp_lit5 = (#const DW_OP_lit5)

dwOp_lit6 :: Word
dwOp_lit6 = (#const DW_OP_lit6)

dwOp_lit7 :: Word
dwOp_lit7 = (#const DW_OP_lit7)

dwOp_lit8 :: Word
dwOp_lit8 = (#const DW_OP_lit8)

dwOp_lit9 :: Word
dwOp_lit9 = (#const DW_OP_lit9)

dwOp_lit10 :: Word
dwOp_lit10 = (#const DW_OP_lit10)

dwOp_lit11 :: Word
dwOp_lit11 = (#const DW_OP_lit11)

dwOp_lit12 :: Word
dwOp_lit12 = (#const DW_OP_lit12)

dwOp_lit13 :: Word
dwOp_lit13 = (#const DW_OP_lit13)

dwOp_lit14 :: Word
dwOp_lit14 = (#const DW_OP_lit14)

dwOp_lit15 :: Word
dwOp_lit15 = (#const DW_OP_lit15)

dwOp_lit16 :: Word
dwOp_lit16 = (#const DW_OP_lit16)

dwOp_lit17 :: Word
dwOp_lit17 = (#const DW_OP_lit17)

dwOp_lit18 :: Word
dwOp_lit18 = (#const DW_OP_lit18)

dwOp_lit19 :: Word
dwOp_lit19 = (#const DW_OP_lit19)

dwOp_lit20 :: Word
dwOp_lit20 = (#const DW_OP_lit20)

dwOp_lit21 :: Word
dwOp_lit21 = (#const DW_OP_lit21)

dwOp_lit22 :: Word
dwOp_lit22 = (#const DW_OP_lit22)

dwOp_lit23 :: Word
dwOp_lit23 = (#const DW_OP_lit23)

dwOp_lit24 :: Word
dwOp_lit24 = (#const DW_OP_lit24)

dwOp_lit25 :: Word
dwOp_lit25 = (#const DW_OP_lit25)

dwOp_lit26 :: Word
dwOp_lit26 = (#const DW_OP_lit26)

dwOp_lit27 :: Word
dwOp_lit27 = (#const DW_OP_lit27)

dwOp_lit28 :: Word
dwOp_lit28 = (#const DW_OP_lit28)

dwOp_lit29 :: Word
dwOp_lit29 = (#const DW_OP_lit29)

dwOp_lit30 :: Word
dwOp_lit30 = (#const DW_OP_lit30)

dwOp_lit31 :: Word
dwOp_lit31 = (#const DW_OP_lit31)

dwOp_reg0 :: Word
dwOp_reg0 = (#const DW_OP_reg0)

dwOp_reg1 :: Word
dwOp_reg1 = (#const DW_OP_reg1)

dwOp_reg2 :: Word
dwOp_reg2 = (#const DW_OP_reg2)

dwOp_reg3 :: Word
dwOp_reg3 = (#const DW_OP_reg3)

dwOp_reg4 :: Word
dwOp_reg4 = (#const DW_OP_reg4)

dwOp_reg5 :: Word
dwOp_reg5 = (#const DW_OP_reg5)

dwOp_reg6 :: Word
dwOp_reg6 = (#const DW_OP_reg6)

dwOp_reg7 :: Word
dwOp_reg7 = (#const DW_OP_reg7)

dwOp_reg8 :: Word
dwOp_reg8 = (#const DW_OP_reg8)

dwOp_reg9 :: Word
dwOp_reg9 = (#const DW_OP_reg9)

dwOp_reg10 :: Word
dwOp_reg10 = (#const DW_OP_reg10)

dwOp_reg11 :: Word
dwOp_reg11 = (#const DW_OP_reg11)

dwOp_reg12 :: Word
dwOp_reg12 = (#const DW_OP_reg12)

dwOp_reg13 :: Word
dwOp_reg13 = (#const DW_OP_reg13)

dwOp_reg14 :: Word
dwOp_reg14 = (#const DW_OP_reg14)

dwOp_reg15 :: Word
dwOp_reg15 = (#const DW_OP_reg15)

dwOp_reg16 :: Word
dwOp_reg16 = (#const DW_OP_reg16)

dwOp_reg17 :: Word
dwOp_reg17 = (#const DW_OP_reg17)

dwOp_reg18 :: Word
dwOp_reg18 = (#const DW_OP_reg18)

dwOp_reg19 :: Word
dwOp_reg19 = (#const DW_OP_reg19)

dwOp_reg20 :: Word
dwOp_reg20 = (#const DW_OP_reg20)

dwOp_reg21 :: Word
dwOp_reg21 = (#const DW_OP_reg21)

dwOp_reg22 :: Word
dwOp_reg22 = (#const DW_OP_reg22)

dwOp_reg23 :: Word
dwOp_reg23 = (#const DW_OP_reg23)

dwOp_reg24 :: Word
dwOp_reg24 = (#const DW_OP_reg24)

dwOp_reg25 :: Word
dwOp_reg25 = (#const DW_OP_reg25)

dwOp_reg26 :: Word
dwOp_reg26 = (#const DW_OP_reg26)

dwOp_reg27 :: Word
dwOp_reg27 = (#const DW_OP_reg27)

dwOp_reg28 :: Word
dwOp_reg28 = (#const DW_OP_reg28)

dwOp_reg29 :: Word
dwOp_reg29 = (#const DW_OP_reg29)

dwOp_reg30 :: Word
dwOp_reg30 = (#const DW_OP_reg30)

dwOp_reg31 :: Word
dwOp_reg31 = (#const DW_OP_reg31)

dwOp_breg0 :: Word
dwOp_breg0 = (#const DW_OP_breg0)

dwOp_breg1 :: Word
dwOp_breg1 = (#const DW_OP_breg1)

dwOp_breg2 :: Word
dwOp_breg2 = (#const DW_OP_breg2)

dwOp_breg3 :: Word
dwOp_breg3 = (#const DW_OP_breg3)

dwOp_breg4 :: Word
dwOp_breg4 = (#const DW_OP_breg4)

dwOp_breg5 :: Word
dwOp_breg5 = (#const DW_OP_breg5)

dwOp_breg6 :: Word
dwOp_breg6 = (#const DW_OP_breg6)

dwOp_breg7 :: Word
dwOp_breg7 = (#const DW_OP_breg7)

dwOp_breg8 :: Word
dwOp_breg8 = (#const DW_OP_breg8)

dwOp_breg9 :: Word
dwOp_breg9 = (#const DW_OP_breg9)

dwOp_breg10 :: Word
dwOp_breg10 = (#const DW_OP_breg10)

dwOp_breg11 :: Word
dwOp_breg11 = (#const DW_OP_breg11)

dwOp_breg12 :: Word
dwOp_breg12 = (#const DW_OP_breg12)

dwOp_breg13 :: Word
dwOp_breg13 = (#const DW_OP_breg13)

dwOp_breg14 :: Word
dwOp_breg14 = (#const DW_OP_breg14)

dwOp_breg15 :: Word
dwOp_breg15 = (#const DW_OP_breg15)

dwOp_breg16 :: Word
dwOp_breg16 = (#const DW_OP_breg16)

dwOp_breg17 :: Word
dwOp_breg17 = (#const DW_OP_breg17)

dwOp_breg18 :: Word
dwOp_breg18 = (#const DW_OP_breg18)

dwOp_breg19 :: Word
dwOp_breg19 = (#const DW_OP_breg19)

dwOp_breg20 :: Word
dwOp_breg20 = (#const DW_OP_breg20)

dwOp_breg21 :: Word
dwOp_breg21 = (#const DW_OP_breg21)

dwOp_breg22 :: Word
dwOp_breg22 = (#const DW_OP_breg22)

dwOp_breg23 :: Word
dwOp_breg23 = (#const DW_OP_breg23)

dwOp_breg24 :: Word
dwOp_breg24 = (#const DW_OP_breg24)

dwOp_breg25 :: Word
dwOp_breg25 = (#const DW_OP_breg25)

dwOp_breg26 :: Word
dwOp_breg26 = (#const DW_OP_breg26)

dwOp_breg27 :: Word
dwOp_breg27 = (#const DW_OP_breg27)

dwOp_breg28 :: Word
dwOp_breg28 = (#const DW_OP_breg28)

dwOp_breg29 :: Word
dwOp_breg29 = (#const DW_OP_breg29)

dwOp_breg30 :: Word
dwOp_breg30 = (#const DW_OP_breg30)

dwOp_breg31 :: Word
dwOp_breg31 = (#const DW_OP_breg31)

dwOp_regx :: Word
dwOp_regx = (#const DW_OP_regx)

dwOp_fbreg :: Word
dwOp_fbreg = (#const DW_OP_fbreg)

dwOp_bregx :: Word
dwOp_bregx = (#const DW_OP_bregx)

dwOp_piece :: Word
dwOp_piece = (#const DW_OP_piece)

dwOp_deref_size :: Word
dwOp_deref_size = (#const DW_OP_deref_size)

dwOp_xderef_size :: Word
dwOp_xderef_size = (#const DW_OP_xderef_size)

dwOp_nop :: Word
dwOp_nop = (#const DW_OP_nop)

dwOp_push_object_address :: Word
dwOp_push_object_address = (#const DW_OP_push_object_address)

dwOp_call2 :: Word
dwOp_call2 = (#const DW_OP_call2)

dwOp_call4 :: Word
dwOp_call4 = (#const DW_OP_call4)

dwOp_call_ref :: Word
dwOp_call_ref = (#const DW_OP_call_ref)

dwOp_form_tls_address :: Word
dwOp_form_tls_address = (#const DW_OP_form_tls_address)

dwOp_call_frame_cfa :: Word
dwOp_call_frame_cfa = (#const DW_OP_call_frame_cfa)

dwOp_bit_piece :: Word
dwOp_bit_piece = (#const DW_OP_bit_piece)

dwOp_implicit_value :: Word
dwOp_implicit_value = (#const DW_OP_implicit_value)

dwOp_stack_value :: Word
dwOp_stack_value = (#const DW_OP_stack_value)

dwOp_GNU_push_tls_address :: Word
dwOp_GNU_push_tls_address = (#const DW_OP_GNU_push_tls_address)

dwOp_GNU_uninit :: Word
dwOp_GNU_uninit = (#const DW_OP_GNU_uninit)

dwOp_GNU_encoded_addr :: Word
dwOp_GNU_encoded_addr = (#const DW_OP_GNU_encoded_addr)

dwOp_GNU_implicit_pointer :: Word
dwOp_GNU_implicit_pointer = (#const DW_OP_GNU_implicit_pointer)

dwOp_lo_user :: Word
dwOp_lo_user = (#const DW_OP_lo_user)

dwOp_hi_user :: Word
dwOp_hi_user = (#const DW_OP_hi_user)

dwAte_void :: Word
dwAte_void = (#const DW_ATE_void)

dwAte_address :: Word
dwAte_address = (#const DW_ATE_address)

dwAte_boolean :: Word
dwAte_boolean = (#const DW_ATE_boolean)

dwAte_complex_float :: Word
dwAte_complex_float = (#const DW_ATE_complex_float)

dwAte_float :: Word
dwAte_float = (#const DW_ATE_float)

dwAte_signed :: Word
dwAte_signed = (#const DW_ATE_signed)

dwAte_signed_char :: Word
dwAte_signed_char = (#const DW_ATE_signed_char)

dwAte_unsigned :: Word
dwAte_unsigned = (#const DW_ATE_unsigned)

dwAte_unsigned_char :: Word
dwAte_unsigned_char = (#const DW_ATE_unsigned_char)

dwAte_imaginary_float :: Word
dwAte_imaginary_float = (#const DW_ATE_imaginary_float)

dwAte_packed_decimal :: Word
dwAte_packed_decimal = (#const DW_ATE_packed_decimal)

dwAte_numeric_string :: Word
dwAte_numeric_string = (#const DW_ATE_numeric_string)

dwAte_edited :: Word
dwAte_edited = (#const DW_ATE_edited)

dwAte_signed_fixed :: Word
dwAte_signed_fixed = (#const DW_ATE_signed_fixed)

dwAte_unsigned_fixed :: Word
dwAte_unsigned_fixed = (#const DW_ATE_unsigned_fixed)

dwAte_decimal_float :: Word
dwAte_decimal_float = (#const DW_ATE_decimal_float)

dwAte_lo_user :: Word
dwAte_lo_user = (#const DW_ATE_lo_user)

dwAte_hi_user :: Word
dwAte_hi_user = (#const DW_ATE_hi_user)

dwDs_unsigned :: Word
dwDs_unsigned = (#const DW_DS_unsigned)

dwDs_leading_overpunch :: Word
dwDs_leading_overpunch = (#const DW_DS_leading_overpunch)

dwDs_trailing_overpunch :: Word
dwDs_trailing_overpunch = (#const DW_DS_trailing_overpunch)

dwDs_leading_separate :: Word
dwDs_leading_separate = (#const DW_DS_leading_separate)

dwDs_trailing_separate :: Word
dwDs_trailing_separate = (#const DW_DS_trailing_separate)

dwEnd_default :: Word
dwEnd_default = (#const DW_END_default)

dwEnd_big :: Word
dwEnd_big = (#const DW_END_big)

dwEnd_little :: Word
dwEnd_little = (#const DW_END_little)

dwEnd_lo_user :: Word
dwEnd_lo_user = (#const DW_END_lo_user)

dwEnd_hi_user :: Word
dwEnd_hi_user = (#const DW_END_hi_user)

dwAccess_public :: Word
dwAccess_public = (#const DW_ACCESS_public)

dwAccess_protected :: Word
dwAccess_protected = (#const DW_ACCESS_protected)

dwAccess_private :: Word
dwAccess_private = (#const DW_ACCESS_private)

dwVis_local :: Word
dwVis_local = (#const DW_VIS_local)

dwVis_exported :: Word
dwVis_exported = (#const DW_VIS_exported)

dwVis_qualified :: Word
dwVis_qualified = (#const DW_VIS_qualified)

dwVirtuality_none :: Word
dwVirtuality_none = (#const DW_VIRTUALITY_none)

dwVirtuality_virtual :: Word
dwVirtuality_virtual = (#const DW_VIRTUALITY_virtual)

dwVirtuality_pure_virtual :: Word
dwVirtuality_pure_virtual = (#const DW_VIRTUALITY_pure_virtual)

dwLang_C89 :: Word
dwLang_C89 = (#const DW_LANG_C89)

dwLang_C :: Word
dwLang_C = (#const DW_LANG_C)

dwLang_Ada83 :: Word
dwLang_Ada83 = (#const DW_LANG_Ada83)

dwLang_C_plus_plus :: Word
dwLang_C_plus_plus = (#const DW_LANG_C_plus_plus)

dwLang_Cobol74 :: Word
dwLang_Cobol74 = (#const DW_LANG_Cobol74)

dwLang_Cobol85 :: Word
dwLang_Cobol85 = (#const DW_LANG_Cobol85)

dwLang_Fortran77 :: Word
dwLang_Fortran77 = (#const DW_LANG_Fortran77)

dwLang_Fortran90 :: Word
dwLang_Fortran90 = (#const DW_LANG_Fortran90)

dwLang_Pascal83 :: Word
dwLang_Pascal83 = (#const DW_LANG_Pascal83)

dwLang_Modula2 :: Word
dwLang_Modula2 = (#const DW_LANG_Modula2)

dwLang_Java :: Word
dwLang_Java = (#const DW_LANG_Java)

dwLang_C99 :: Word
dwLang_C99 = (#const DW_LANG_C99)

dwLang_Ada95 :: Word
dwLang_Ada95 = (#const DW_LANG_Ada95)

dwLang_Fortran95 :: Word
dwLang_Fortran95 = (#const DW_LANG_Fortran95)

dwLang_PL1 :: Word
dwLang_PL1 = (#const DW_LANG_PL1)

dwLang_Objc :: Word
dwLang_Objc = (#const DW_LANG_Objc)

dwLang_ObjC_plus_plus :: Word
dwLang_ObjC_plus_plus = (#const DW_LANG_ObjC_plus_plus)

dwLang_UPC :: Word
dwLang_UPC = (#const DW_LANG_UPC)

dwLang_D :: Word
dwLang_D = (#const DW_LANG_D)

dwLang_Python :: Word
dwLang_Python = (#const DW_LANG_Python)

dwLang_lo_user :: Word
dwLang_lo_user = (#const DW_LANG_lo_user)

dwLang_Mips_Assembler :: Word
dwLang_Mips_Assembler = (#const DW_LANG_Mips_Assembler)

dwLang_hi_user :: Word
dwLang_hi_user = (#const DW_LANG_hi_user)

dwId_case_sensitive :: Word
dwId_case_sensitive = (#const DW_ID_case_sensitive)

dwId_up_case :: Word
dwId_up_case = (#const DW_ID_up_case)

dwId_down_case :: Word
dwId_down_case = (#const DW_ID_down_case)

dwId_case_insensitive :: Word
dwId_case_insensitive = (#const DW_ID_case_insensitive)

dwCc_normal :: Word
dwCc_normal = (#const DW_CC_normal)

dwCc_program :: Word
dwCc_program = (#const DW_CC_program)

dwCc_nocall :: Word
dwCc_nocall = (#const DW_CC_nocall)

dwCc_lo_user :: Word
dwCc_lo_user = (#const DW_CC_lo_user)

dwCc_hi_user :: Word
dwCc_hi_user = (#const DW_CC_hi_user)

dwInl_not_inlined :: Word
dwInl_not_inlined = (#const DW_INL_not_inlined)

dwInl_inlined :: Word
dwInl_inlined = (#const DW_INL_inlined)

dwInl_declared_not_inlined :: Word
dwInl_declared_not_inlined = (#const DW_INL_declared_not_inlined)

dwInl_declared_inlined :: Word
dwInl_declared_inlined = (#const DW_INL_declared_inlined)

dwOrd_row_major :: Word
dwOrd_row_major = (#const DW_ORD_row_major)

dwOrd_col_major :: Word
dwOrd_col_major = (#const DW_ORD_col_major)

dwDsc_label :: Word
dwDsc_label = (#const DW_DSC_label)

dwDsc_range :: Word
dwDsc_range = (#const DW_DSC_range)

dwLns_copy :: Word
dwLns_copy = (#const DW_LNS_copy)

dwLns_advance_pc :: Word
dwLns_advance_pc = (#const DW_LNS_advance_pc)

dwLns_advance_line :: Word
dwLns_advance_line = (#const DW_LNS_advance_line)

dwLns_set_file :: Word
dwLns_set_file = (#const DW_LNS_set_file)

dwLns_set_column :: Word
dwLns_set_column = (#const DW_LNS_set_column)

dwLns_negate_stmt :: Word
dwLns_negate_stmt = (#const DW_LNS_negate_stmt)

dwLns_set_basic_block :: Word
dwLns_set_basic_block = (#const DW_LNS_set_basic_block)

dwLns_const_add_pc :: Word
dwLns_const_add_pc = (#const DW_LNS_const_add_pc)

dwLns_fixed_advance_pc :: Word
dwLns_fixed_advance_pc = (#const DW_LNS_fixed_advance_pc)

dwLns_set_prologue_end :: Word
dwLns_set_prologue_end = (#const DW_LNS_set_prologue_end)

dwLns_set_epilogue_begin :: Word
dwLns_set_epilogue_begin = (#const DW_LNS_set_epilogue_begin)

dwLns_set_isa :: Word
dwLns_set_isa = (#const DW_LNS_set_isa)

dwLne_end_sequence :: Word
dwLne_end_sequence = (#const DW_LNE_end_sequence)

dwLne_set_address :: Word
dwLne_set_address = (#const DW_LNE_set_address)

dwLne_define_file :: Word
dwLne_define_file = (#const DW_LNE_define_file)

dwLne_set_discriminator :: Word
dwLne_set_discriminator = (#const DW_LNE_set_discriminator)

dwLne_lo_user :: Word
dwLne_lo_user = (#const DW_LNE_lo_user)

dwLne_hi_user :: Word
dwLne_hi_user = (#const DW_LNE_hi_user)

dwMacInfo_define :: Word
dwMacInfo_define = (#const DW_MACINFO_define)

dwMacInfo_undef :: Word
dwMacInfo_undef = (#const DW_MACINFO_undef)

dwMacInfo_start_file :: Word
dwMacInfo_start_file = (#const DW_MACINFO_start_file)

dwMacInfo_end_file :: Word
dwMacInfo_end_file = (#const DW_MACINFO_end_file)

dwMacInfo_vendor_ext :: Word
dwMacInfo_vendor_ext = (#const DW_MACINFO_vendor_ext)

dwCfa_advance_loc :: Word
dwCfa_advance_loc = (#const DW_CFA_advance_loc)

dwCfa_offset :: Word
dwCfa_offset = (#const DW_CFA_offset)

dwCfa_restore :: Word
dwCfa_restore = (#const DW_CFA_restore)

dwCfa_extended :: Word
dwCfa_extended = (#const DW_CFA_extended)

dwCfa_nop :: Word
dwCfa_nop = (#const DW_CFA_nop)

dwCfa_set_loc :: Word
dwCfa_set_loc = (#const DW_CFA_set_loc)

dwCfa_advance_loc1 :: Word
dwCfa_advance_loc1 = (#const DW_CFA_advance_loc1)

dwCfa_advance_loc2 :: Word
dwCfa_advance_loc2 = (#const DW_CFA_advance_loc2)

dwCfa_advance_loc4 :: Word
dwCfa_advance_loc4 = (#const DW_CFA_advance_loc4)

dwCfa_offset_extended :: Word
dwCfa_offset_extended = (#const DW_CFA_offset_extended)

dwCfa_restore_extended :: Word
dwCfa_restore_extended = (#const DW_CFA_restore_extended)

dwCfa_undefined :: Word
dwCfa_undefined = (#const DW_CFA_undefined)

dwCfa_same_value :: Word
dwCfa_same_value = (#const DW_CFA_same_value)

dwCfa_register :: Word
dwCfa_register = (#const DW_CFA_register)

dwCfa_remember_state :: Word
dwCfa_remember_state = (#const DW_CFA_remember_state)

dwCfa_restore_state :: Word
dwCfa_restore_state = (#const DW_CFA_restore_state)

dwCfa_def_cfa :: Word
dwCfa_def_cfa = (#const DW_CFA_def_cfa)

dwCfa_def_cfa_register :: Word
dwCfa_def_cfa_register = (#const DW_CFA_def_cfa_register)

dwCfa_def_cfa_offset :: Word
dwCfa_def_cfa_offset = (#const DW_CFA_def_cfa_offset)

dwCfa_def_cfa_expression :: Word
dwCfa_def_cfa_expression = (#const DW_CFA_def_cfa_expression)

dwCfa_expression :: Word
dwCfa_expression = (#const DW_CFA_expression)

dwCfa_offset_extended_sf :: Word
dwCfa_offset_extended_sf = (#const DW_CFA_offset_extended_sf)

dwCfa_def_cfa_sf :: Word
dwCfa_def_cfa_sf = (#const DW_CFA_def_cfa_sf)

dwCfa_def_cfa_offset_sf :: Word
dwCfa_def_cfa_offset_sf = (#const DW_CFA_def_cfa_offset_sf)

dwCfa_val_offset :: Word
dwCfa_val_offset = (#const DW_CFA_val_offset)

dwCfa_val_offset_sf :: Word
dwCfa_val_offset_sf = (#const DW_CFA_val_offset_sf)

dwCfa_val_expression :: Word
dwCfa_val_expression = (#const DW_CFA_val_expression)

dwCfa_low_user :: Word
dwCfa_low_user = (#const DW_CFA_low_user)

dwCfa_MIPS_advance_loc8 :: Word
dwCfa_MIPS_advance_loc8 = (#const DW_CFA_MIPS_advance_loc8)

dwCfa_GNU_window_save :: Word
dwCfa_GNU_window_save = (#const DW_CFA_GNU_window_save)

dwCfa_GNU_args_size :: Word
dwCfa_GNU_args_size = (#const DW_CFA_GNU_args_size)

dwCfa_GNU_negative_offset_extended :: Word
dwCfa_GNU_negative_offset_extended = (#const DW_CFA_GNU_negative_offset_extended)

dwCfa_high_user :: Word
dwCfa_high_user = (#const DW_CFA_high_user)

dwCie_ID_32 :: Word
dwCie_ID_32 = (#const DW_CIE_ID_32)

dwCie_ID_64 :: Word
dwCie_ID_64 = (#const DW_CIE_ID_64)

dwEh_PE_absptr :: Word
dwEh_PE_absptr = (#const DW_EH_PE_absptr)

dwEh_PE_omit :: Word
dwEh_PE_omit = (#const DW_EH_PE_omit)

dwEh_PE_uleb128 :: Word
dwEh_PE_uleb128 = (#const DW_EH_PE_uleb128)

dwEh_PE_udata2 :: Word
dwEh_PE_udata2 = (#const DW_EH_PE_udata2)

dwEh_PE_udata4 :: Word
dwEh_PE_udata4 = (#const DW_EH_PE_udata4)

dwEh_PE_udata8 :: Word
dwEh_PE_udata8 = (#const DW_EH_PE_udata8)

dwEh_PE_sleb128 :: Word
dwEh_PE_sleb128 = (#const DW_EH_PE_sleb128)

dwEh_PE_sdata2 :: Word
dwEh_PE_sdata2 = (#const DW_EH_PE_sdata2)

dwEh_PE_sdata4 :: Word
dwEh_PE_sdata4 = (#const DW_EH_PE_sdata4)

dwEh_PE_sdata8 :: Word
dwEh_PE_sdata8 = (#const DW_EH_PE_sdata8)

dwEh_PE_signed :: Word
dwEh_PE_signed = (#const DW_EH_PE_signed)

dwEh_PE_pcrel :: Word
dwEh_PE_pcrel = (#const DW_EH_PE_pcrel)

dwEh_PE_textrel :: Word
dwEh_PE_textrel = (#const DW_EH_PE_textrel)

dwEh_PE_datarel :: Word
dwEh_PE_datarel = (#const DW_EH_PE_datarel)

dwEh_PE_funcrel :: Word
dwEh_PE_funcrel = (#const DW_EH_PE_funcrel)

dwEh_PE_aligned :: Word
dwEh_PE_aligned = (#const DW_EH_PE_aligned)

dwEh_PE_indirect :: Word
dwEh_PE_indirect = (#const DW_EH_PE_indirect)

