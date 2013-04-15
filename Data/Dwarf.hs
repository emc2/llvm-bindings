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

-- | Module containing constants for the Dwarf debugging format.
module Data.Dwarf where

import Data.Word

dwTag_array_type :: Word
dwTag_array_type = 0x01

dwTag_class_type :: Word
dwTag_class_type = 0x02

dwTag_entry_point :: Word
dwTag_entry_point = 0x03

dwTag_enumeration_type :: Word
dwTag_enumeration_type = 0x04

dwTag_formal_parameter :: Word
dwTag_formal_parameter = 0x05

dwTag_imported_declaration :: Word
dwTag_imported_declaration = 0x08

dwTag_label :: Word
dwTag_label = 0x0a

dwTag_lexical_block :: Word
dwTag_lexical_block = 0x0b

dwTag_member :: Word
dwTag_member = 0x0d

dwTag_pointer_type :: Word
dwTag_pointer_type = 0x0f

dwTag_reference_type :: Word
dwTag_reference_type = 0x10

dwTag_compile_unit :: Word
dwTag_compile_unit = 0x11

dwTag_string_type :: Word
dwTag_string_type = 0x12

dwTag_structure_type :: Word
dwTag_structure_type = 0x13

dwTag_subroutine_type :: Word
dwTag_subroutine_type = 0x15

dwTag_typedef :: Word
dwTag_typedef = 0x16

dwTag_union_type :: Word
dwTag_union_type = 0x17

dwTag_unspecified_parameters :: Word
dwTag_unspecified_parameters = 0x18

dwTag_variant :: Word
dwTag_variant = 0x19

dwTag_common_block :: Word
dwTag_common_block = 0x1a

dwTag_common_inclusion :: Word
dwTag_common_inclusion = 0x1b

dwTag_inheritance :: Word
dwTag_inheritance = 0x1c

dwTag_inlined_subroutine :: Word
dwTag_inlined_subroutine = 0x1d

dwTag_module :: Word
dwTag_module = 0x1e

dwTag_ptr_to_member_type :: Word
dwTag_ptr_to_member_type = 0x1f

dwTag_set_type :: Word
dwTag_set_type = 0x20

dwTag_subrange_type :: Word
dwTag_subrange_type = 0x21

dwTag_with_stmt :: Word
dwTag_with_stmt = 0x22

dwTag_access_declaration :: Word
dwTag_access_declaration = 0x23

dwTag_base_type :: Word
dwTag_base_type = 0x24

dwTag_catch_block :: Word
dwTag_catch_block = 0x25

dwTag_const_type :: Word
dwTag_const_type = 0x26

dwTag_constant :: Word
dwTag_constant = 0x27

dwTag_enumerator :: Word
dwTag_enumerator = 0x28

dwTag_friend :: Word
dwTag_friend = 0x2a

dwTag_namelist :: Word
dwTag_namelist = 0x2b

dwTag_namelist_item :: Word
dwTag_namelist_item = 0x2c

dwTag_packed_type :: Word
dwTag_packed_type = 0x2d

dwTag_subprogram :: Word
dwTag_subprogram = 0x2e

dwTag_template_type_parameter :: Word
dwTag_template_type_parameter = 0x2f

dwTag_template_type_param :: Word
dwTag_template_type_param = 0x2f

dwTag_template_value_parameter :: Word
dwTag_template_value_parameter = 0x30

dwTag_template_value_param :: Word
dwTag_template_value_param = 0x30

dwTag_thrown_type :: Word
dwTag_thrown_type = 0x31

dwTag_try_block :: Word
dwTag_try_block = 0x32

dwTag_variant_part :: Word
dwTag_variant_part = 0x33

dwTag_variable :: Word
dwTag_variable = 0x34

dwTag_volatile_type :: Word
dwTag_volatile_type = 0x35

dwTag_dwarf_procedure :: Word
dwTag_dwarf_procedure = 0x36

dwTag_restrict_type :: Word
dwTag_restrict_type = 0x37

dwTag_interface_type :: Word
dwTag_interface_type = 0x38

dwTag_namespace :: Word
dwTag_namespace = 0x39

dwTag_imported_module :: Word
dwTag_imported_module = 0x3a

dwTag_unspecified_type :: Word
dwTag_unspecified_type = 0x3b

dwTag_partial_unit :: Word
dwTag_partial_unit = 0x3c

dwTag_imported_unit :: Word
dwTag_imported_unit = 0x3d

dwTag_condition :: Word
dwTag_condition = 0x3f

dwTag_shared_type :: Word
dwTag_shared_type = 0x40

dwTag_lo_user :: Word
dwTag_lo_user = 0x4080

dwTag_hi_user :: Word
dwTag_hi_user = 0xffff

dwChildren_no :: Word
dwChildren_no = 0x00

dwChildren_yes :: Word
dwChildren_yes = 0x01

dwAt_sibling :: Word
dwAt_sibling = 0x01

dwAt_location :: Word
dwAt_location = 0x02

dwAt_name :: Word
dwAt_name = 0x03

dwAt_ordering :: Word
dwAt_ordering = 0x09

dwAt_subscr_data :: Word
dwAt_subscr_data = 0x0a

dwAt_byte_size :: Word
dwAt_byte_size = 0x0b

dwAt_bit_offset :: Word
dwAt_bit_offset = 0x0c

dwAt_bit_size :: Word
dwAt_bit_size = 0x0d

dwAt_element_list :: Word
dwAt_element_list = 0x0f

dwAt_stmt_list :: Word
dwAt_stmt_list = 0x10

dwAt_low_pc :: Word
dwAt_low_pc = 0x11

dwAt_high_pc :: Word
dwAt_high_pc = 0x12

dwAt_language :: Word
dwAt_language = 0x13

dwAt_member :: Word
dwAt_member = 0x14

dwAt_discr :: Word
dwAt_discr = 0x15

dwAt_discr_value :: Word
dwAt_discr_value = 0x16

dwAt_visibility :: Word
dwAt_visibility = 0x17

dwAt_import :: Word
dwAt_import = 0x18

dwAt_string_length :: Word
dwAt_string_length = 0x19

dwAt_common_reference :: Word
dwAt_common_reference = 0x1a

dwAt_comp_dir :: Word
dwAt_comp_dir = 0x1b

dwAt_const_value :: Word
dwAt_const_value = 0x1c

dwAt_containing_type :: Word
dwAt_containing_type = 0x1d

dwAt_default_value :: Word
dwAt_default_value = 0x1e

dwAt_inline :: Word
dwAt_inline = 0x20

dwAt_is_optional :: Word
dwAt_is_optional = 0x21

dwAt_lower_bound :: Word
dwAt_lower_bound = 0x22

dwAt_producer :: Word
dwAt_producer = 0x25

dwAt_prototyped :: Word
dwAt_prototyped = 0x27

dwAt_return_addr :: Word
dwAt_return_addr = 0x2a

dwAt_start_scope :: Word
dwAt_start_scope = 0x2c

dwAt_bit_stride :: Word
dwAt_bit_stride = 0x2e

dwAt_stride_size :: Word
dwAt_stride_size = 0x2e

dwAt_upper_bound :: Word
dwAt_upper_bound = 0x2f

dwAt_abstract_origin :: Word
dwAt_abstract_origin = 0x31

dwAt_accessibility :: Word
dwAt_accessibility = 0x32

dwAt_address_class :: Word
dwAt_address_class = 0x33

dwAt_artificial :: Word
dwAt_artificial = 0x34

dwAt_base_types :: Word
dwAt_base_types = 0x35

dwAt_calling_convention :: Word
dwAt_calling_convention = 0x36

dwAt_count :: Word
dwAt_count = 0x37

dwAt_data_member_location :: Word
dwAt_data_member_location = 0x38

dwAt_decl_column :: Word
dwAt_decl_column = 0x39

dwAt_decl_file :: Word
dwAt_decl_file = 0x3a

dwAt_decl_line :: Word
dwAt_decl_line = 0x3b

dwAt_declaration :: Word
dwAt_declaration = 0x3c

dwAt_discr_list :: Word
dwAt_discr_list = 0x3d

dwAt_encoding :: Word
dwAt_encoding = 0x3e

dwAt_external :: Word
dwAt_external = 0x3f

dwAt_frame_base :: Word
dwAt_frame_base = 0x40

dwAt_friend :: Word
dwAt_friend = 0x41

dwAt_identifier_case :: Word
dwAt_identifier_case = 0x42

dwAt_macro_info :: Word
dwAt_macro_info = 0x43

dwAt_namelist_item :: Word
dwAt_namelist_item = 0x44

dwAt_priority :: Word
dwAt_priority = 0x45

dwAt_segment :: Word
dwAt_segment = 0x46

dwAt_specification :: Word
dwAt_specification = 0x47

dwAt_static_link :: Word
dwAt_static_link = 0x48

dwAt_type :: Word
dwAt_type = 0x49

dwAt_use_location :: Word
dwAt_use_location = 0x4a

dwAt_variable_parameter :: Word
dwAt_variable_parameter = 0x4b

dwAt_virtuality :: Word
dwAt_virtuality = 0x4c

dwAt_vtable_elem_location :: Word
dwAt_vtable_elem_location = 0x4d

dwAt_lo_user :: Word
dwAt_lo_user = 0x2000

dwAt_hi_user :: Word
dwAt_hi_user = 0x3fff

dwForm_addr :: Word
dwForm_addr = 0x01

dwForm_block2 :: Word
dwForm_block2 = 0x03

dwForm_block4 :: Word
dwForm_block4 = 0x04

dwForm_data2 :: Word
dwForm_data2 = 0x05

dwForm_data4 :: Word
dwForm_data4 = 0x06

dwForm_data8 :: Word
dwForm_data8 = 0x07

dwForm_string :: Word
dwForm_string = 0x08

dwForm_block :: Word
dwForm_block = 0x09

dwForm_block1 :: Word
dwForm_block1 = 0x0a

dwForm_data1 :: Word
dwForm_data1 = 0x0b

dwForm_flag :: Word
dwForm_flag = 0x0c

dwForm_sdata :: Word
dwForm_sdata = 0x0d

dwForm_strp :: Word
dwForm_strp = 0x0e

dwForm_udata :: Word
dwForm_udata = 0x0f

dwForm_ref_addr :: Word
dwForm_ref_addr = 0x10

dwForm_ref1 :: Word
dwForm_ref1 = 0x11

dwForm_ref2 :: Word
dwForm_ref2 = 0x12

dwForm_ref4 :: Word
dwForm_ref4 = 0x13

dwForm_ref8 :: Word
dwForm_ref8 = 0x14

dwForm_ref_udata :: Word
dwForm_ref_udata = 0x15

dwForm_indirect :: Word
dwForm_indirect = 0x16

dwForm_flag_present :: Word
dwForm_flag_present = 0x19

dwOp_addr :: Word
dwOp_addr = 0x03

dwOp_deref :: Word
dwOp_deref = 0x06

dwOp_const1u :: Word
dwOp_const1u = 0x08

dwOp_const1s :: Word
dwOp_const1s = 0x09

dwOp_const2u :: Word
dwOp_const2u = 0x0a

dwOp_const2s :: Word
dwOp_const2s = 0x0b

dwOp_const4u :: Word
dwOp_const4u = 0x0c

dwOp_const4s :: Word
dwOp_const4s = 0x0d

dwOp_const8u :: Word
dwOp_const8u = 0x0e

dwOp_const8s :: Word
dwOp_const8s = 0x0f

dwOp_constu :: Word
dwOp_constu = 0x10

dwOp_consts :: Word
dwOp_consts = 0x11

dwOp_dup :: Word
dwOp_dup = 0x12

dwOp_drop :: Word
dwOp_drop = 0x13

dwOp_over :: Word
dwOp_over = 0x14

dwOp_pick :: Word
dwOp_pick = 0x15

dwOp_swap :: Word
dwOp_swap = 0x16

dwOp_rot :: Word
dwOp_rot = 0x17

dwOp_xderef :: Word
dwOp_xderef = 0x18

dwOp_abs :: Word
dwOp_abs = 0x19

dwOp_and :: Word
dwOp_and = 0x1a

dwOp_div :: Word
dwOp_div = 0x1b

dwOp_minus :: Word
dwOp_minus = 0x1c

dwOp_mod :: Word
dwOp_mod = 0x1d

dwOp_mul :: Word
dwOp_mul = 0x1e

dwOp_neg :: Word
dwOp_neg = 0x1f

dwOp_not :: Word
dwOp_not = 0x20

dwOp_or :: Word
dwOp_or = 0x21

dwOp_plus :: Word
dwOp_plus = 0x22

dwOp_plus_uconst :: Word
dwOp_plus_uconst = 0x23

dwOp_shl :: Word
dwOp_shl = 0x24

dwOp_shr :: Word
dwOp_shr = 0x25

dwOp_shra :: Word
dwOp_shra = 0x26

dwOp_xor :: Word
dwOp_xor = 0x27

dwOp_bra :: Word
dwOp_bra = 0x28

dwOp_eq :: Word
dwOp_eq = 0x29

dwOp_ge :: Word
dwOp_ge = 0x2a

dwOp_gt :: Word
dwOp_gt = 0x2b

dwOp_le :: Word
dwOp_le = 0x2c

dwOp_lt :: Word
dwOp_lt = 0x2d

dwOp_ne :: Word
dwOp_ne = 0x2e

dwOp_skip :: Word
dwOp_skip = 0x2f

dwOp_lit0 :: Word
dwOp_lit0 = 0x30

dwOp_lit1 :: Word
dwOp_lit1 = 0x31

dwOp_lit2 :: Word
dwOp_lit2 = 0x32

dwOp_lit3 :: Word
dwOp_lit3 = 0x33

dwOp_lit4 :: Word
dwOp_lit4 = 0x34

dwOp_lit5 :: Word
dwOp_lit5 = 0x35

dwOp_lit6 :: Word
dwOp_lit6 = 0x36

dwOp_lit7 :: Word
dwOp_lit7 = 0x37

dwOp_lit8 :: Word
dwOp_lit8 = 0x38

dwOp_lit9 :: Word
dwOp_lit9 = 0x39

dwOp_lit10 :: Word
dwOp_lit10 = 0x3a

dwOp_lit11 :: Word
dwOp_lit11 = 0x3b

dwOp_lit12 :: Word
dwOp_lit12 = 0x3c

dwOp_lit13 :: Word
dwOp_lit13 = 0x3d

dwOp_lit14 :: Word
dwOp_lit14 = 0x3e

dwOp_lit15 :: Word
dwOp_lit15 = 0x3f

dwOp_lit16 :: Word
dwOp_lit16 = 0x40

dwOp_lit17 :: Word
dwOp_lit17 = 0x41

dwOp_lit18 :: Word
dwOp_lit18 = 0x42

dwOp_lit19 :: Word
dwOp_lit19 = 0x43

dwOp_lit20 :: Word
dwOp_lit20 = 0x44

dwOp_lit21 :: Word
dwOp_lit21 = 0x45

dwOp_lit22 :: Word
dwOp_lit22 = 0x46

dwOp_lit23 :: Word
dwOp_lit23 = 0x47

dwOp_lit24 :: Word
dwOp_lit24 = 0x48

dwOp_lit25 :: Word
dwOp_lit25 = 0x49

dwOp_lit26 :: Word
dwOp_lit26 = 0x4a

dwOp_lit27 :: Word
dwOp_lit27 = 0x4b

dwOp_lit28 :: Word
dwOp_lit28 = 0x4c

dwOp_lit29 :: Word
dwOp_lit29 = 0x4d

dwOp_lit30 :: Word
dwOp_lit30 = 0x4e

dwOp_lit31 :: Word
dwOp_lit31 = 0x4f

dwOp_reg0 :: Word
dwOp_reg0 = 0x50

dwOp_reg1 :: Word
dwOp_reg1 = 0x51

dwOp_reg2 :: Word
dwOp_reg2 = 0x52

dwOp_reg3 :: Word
dwOp_reg3 = 0x53

dwOp_reg4 :: Word
dwOp_reg4 = 0x54

dwOp_reg5 :: Word
dwOp_reg5 = 0x55

dwOp_reg6 :: Word
dwOp_reg6 = 0x56

dwOp_reg7 :: Word
dwOp_reg7 = 0x57

dwOp_reg8 :: Word
dwOp_reg8 = 0x58

dwOp_reg9 :: Word
dwOp_reg9 = 0x59

dwOp_reg10 :: Word
dwOp_reg10 = 0x5a

dwOp_reg11 :: Word
dwOp_reg11 = 0x5b

dwOp_reg12 :: Word
dwOp_reg12 = 0x5c

dwOp_reg13 :: Word
dwOp_reg13 = 0x5d

dwOp_reg14 :: Word
dwOp_reg14 = 0x5e

dwOp_reg15 :: Word
dwOp_reg15 = 0x5f

dwOp_reg16 :: Word
dwOp_reg16 = 0x60

dwOp_reg17 :: Word
dwOp_reg17 = 0x61

dwOp_reg18 :: Word
dwOp_reg18 = 0x62

dwOp_reg19 :: Word
dwOp_reg19 = 0x63

dwOp_reg20 :: Word
dwOp_reg20 = 0x64

dwOp_reg21 :: Word
dwOp_reg21 = 0x65

dwOp_reg22 :: Word
dwOp_reg22 = 0x66

dwOp_reg23 :: Word
dwOp_reg23 = 0x67

dwOp_reg24 :: Word
dwOp_reg24 = 0x68

dwOp_reg25 :: Word
dwOp_reg25 = 0x69

dwOp_reg26 :: Word
dwOp_reg26 = 0x6a

dwOp_reg27 :: Word
dwOp_reg27 = 0x6b

dwOp_reg28 :: Word
dwOp_reg28 = 0x6c

dwOp_reg29 :: Word
dwOp_reg29 = 0x6d

dwOp_reg30 :: Word
dwOp_reg30 = 0x6e

dwOp_reg31 :: Word
dwOp_reg31 = 0x6f

dwOp_breg0 :: Word
dwOp_breg0 = 0x70

dwOp_breg1 :: Word
dwOp_breg1 = 0x71

dwOp_breg2 :: Word
dwOp_breg2 = 0x72

dwOp_breg3 :: Word
dwOp_breg3 = 0x73

dwOp_breg4 :: Word
dwOp_breg4 = 0x74

dwOp_breg5 :: Word
dwOp_breg5 = 0x75

dwOp_breg6 :: Word
dwOp_breg6 = 0x76

dwOp_breg7 :: Word
dwOp_breg7 = 0x77

dwOp_breg8 :: Word
dwOp_breg8 = 0x78

dwOp_breg9 :: Word
dwOp_breg9 = 0x79

dwOp_breg10 :: Word
dwOp_breg10 = 0x7a

dwOp_breg11 :: Word
dwOp_breg11 = 0x7b

dwOp_breg12 :: Word
dwOp_breg12 = 0x7c

dwOp_breg13 :: Word
dwOp_breg13 = 0x7d

dwOp_breg14 :: Word
dwOp_breg14 = 0x7e

dwOp_breg15 :: Word
dwOp_breg15 = 0x7f

dwOp_breg16 :: Word
dwOp_breg16 = 0x80

dwOp_breg17 :: Word
dwOp_breg17 = 0x81

dwOp_breg18 :: Word
dwOp_breg18 = 0x82

dwOp_breg19 :: Word
dwOp_breg19 = 0x83

dwOp_breg20 :: Word
dwOp_breg20 = 0x84

dwOp_breg21 :: Word
dwOp_breg21 = 0x85

dwOp_breg22 :: Word
dwOp_breg22 = 0x86

dwOp_breg23 :: Word
dwOp_breg23 = 0x87

dwOp_breg24 :: Word
dwOp_breg24 = 0x88

dwOp_breg25 :: Word
dwOp_breg25 = 0x89

dwOp_breg26 :: Word
dwOp_breg26 = 0x8a

dwOp_breg27 :: Word
dwOp_breg27 = 0x8b

dwOp_breg28 :: Word
dwOp_breg28 = 0x8c

dwOp_breg29 :: Word
dwOp_breg29 = 0x8d

dwOp_breg30 :: Word
dwOp_breg30 = 0x8e

dwOp_breg31 :: Word
dwOp_breg31 = 0x8f

dwOp_regx :: Word
dwOp_regx = 0x90

dwOp_fbreg :: Word
dwOp_fbreg = 0x91

dwOp_bregx :: Word
dwOp_bregx = 0x92

dwOp_piece :: Word
dwOp_piece = 0x93

dwOp_deref_size :: Word
dwOp_deref_size = 0x94

dwOp_xderef_size :: Word
dwOp_xderef_size = 0x95

dwOp_nop :: Word
dwOp_nop = 0x96

dwOp_lo_user :: Word
dwOp_lo_user = 0xe0

dwOp_hi_user :: Word
dwOp_hi_user = 0xff

dwAte_address :: Word
dwAte_address = 0x1

dwAte_boolean :: Word
dwAte_boolean = 0x2

dwAte_complex_float :: Word
dwAte_complex_float = 0x3

dwAte_float :: Word
dwAte_float = 0x4

dwAte_signed :: Word
dwAte_signed = 0x5

dwAte_signed_char :: Word
dwAte_signed_char = 0x6

dwAte_unsigned :: Word
dwAte_unsigned = 0x7

dwAte_unsigned_char :: Word
dwAte_unsigned_char = 0x8

dwAte_imaginary_float :: Word
dwAte_imaginary_float = 0x9

dwAte_packed_decimal :: Word
dwAte_packed_decimal = 0xa

dwAte_numeric_string :: Word
dwAte_numeric_string = 0xb

dwAte_edited :: Word
dwAte_edited = 0xc

dwAte_signed_fixed :: Word
dwAte_signed_fixed = 0xd

dwAte_unsigned_fixed :: Word
dwAte_unsigned_fixed = 0xe

dwAte_decimal_float :: Word
dwAte_decimal_float = 0xf

dwAte_lo_user :: Word
dwAte_lo_user = 0x80

dwAte_hi_user :: Word
dwAte_hi_user = 0xff

dwAccess_public :: Word
dwAccess_public = 0x01

dwAccess_protected :: Word
dwAccess_protected = 0x02

dwAccess_private :: Word
dwAccess_private = 0x03

dwVis_local :: Word
dwVis_local = 0x01

dwVis_exported :: Word
dwVis_exported = 0x02

dwVis_qualified :: Word
dwVis_qualified = 0x03

dwVirtuality_none :: Word
dwVirtuality_none = 0x00

dwVirtuality_virtual :: Word
dwVirtuality_virtual = 0x01

dwVirtuality_pure_virtual :: Word
dwVirtuality_pure_virtual = 0x02

dwLang_C89 :: Word
dwLang_C89 = 0x0001

dwLang_C :: Word
dwLang_C = 0x0002

dwLang_Ada83 :: Word
dwLang_Ada83 = 0x0003

dwLang_C_plus_plus :: Word
dwLang_C_plus_plus = 0x0004

dwLang_Cobol74 :: Word
dwLang_Cobol74 = 0x0005

dwLang_Cobol85 :: Word
dwLang_Cobol85 = 0x0006

dwLang_Fortran77 :: Word
dwLang_Fortran77 = 0x0007

dwLang_Fortran90 :: Word
dwLang_Fortran90 = 0x0008

dwLang_Pascal83 :: Word
dwLang_Pascal83 = 0x0009

dwLang_Modula2 :: Word
dwLang_Modula2 = 0x000a

dwLang_Java :: Word
dwLang_Java = 0x000b

dwLang_C99 :: Word
dwLang_C99 = 0x000c

dwLang_Ada95 :: Word
dwLang_Ada95 = 0x000d

dwLang_Fortran95 :: Word
dwLang_Fortran95 = 0x000e

dwLang_PLI :: Word
dwLang_PLI = 0x000f

dwLang_ObjC :: Word
dwLang_ObjC = 0x0010

dwLang_ObjC_plus_plus :: Word
dwLang_ObjC_plus_plus = 0x0011

dwLang_UPC :: Word
dwLang_UPC = 0x0012

dwLang_D :: Word
dwLang_D = 0x0013

dwLang_lo_user :: Word
dwLang_lo_user = 0x8000

dwLang_hi_user :: Word
dwLang_hi_user = 0xffff

dwId_case_sensitive :: Word
dwId_case_sensitive = 0x00

dwId_up_case :: Word
dwId_up_case = 0x01

dwId_down_case :: Word
dwId_down_case = 0x02

dwId_case_insensitive :: Word
dwId_case_insensitive = 0x03

dwCc_normal :: Word
dwCc_normal = 0x01

dwCc_program :: Word
dwCc_program = 0x02

dwCc_nocall :: Word
dwCc_nocall = 0x03

dwCc_lo_user :: Word
dwCc_lo_user = 0x40

dwCc_hi_user :: Word
dwCc_hi_user = 0xff

dwInl_not_inlined :: Word
dwInl_not_inlined = 0x00

dwInl_inlined :: Word
dwInl_inlined = 0x01

dwInl_declared_not_inlined :: Word
dwInl_declared_not_inlined = 0x02

dwInl_declared_inlined :: Word
dwInl_declared_inlined = 0x03

dwOrd_row_major :: Word
dwOrd_row_major = 0x00

dwOrd_col_major :: Word
dwOrd_col_major = 0x01

dwDsc_label :: Word
dwDsc_label = 0x00

dwDsc_range :: Word
dwDsc_range = 0x01

dwLns_copy :: Word
dwLns_copy = 0x01

dwLns_advance_pc :: Word
dwLns_advance_pc = 0x02

dwLns_advance_line :: Word
dwLns_advance_line = 0x03

dwLns_set_file :: Word
dwLns_set_file = 0x04

dwLns_set_column :: Word
dwLns_set_column = 0x05

dwLns_negate_stmt :: Word
dwLns_negate_stmt = 0x06

dwLns_set_basic_block :: Word
dwLns_set_basic_block = 0x07

dwLns_const_add_pc :: Word
dwLns_const_add_pc = 0x08

dwLns_fixed_advance_pc :: Word
dwLns_fixed_advance_pc = 0x09

dwLns_set_prologue_end :: Word
dwLns_set_prologue_end = 0x0a

dwLns_set_epilogue_begin :: Word
dwLns_set_epilogue_begin = 0x0b

dwLns_set_isa :: Word
dwLns_set_isa = 0x0c

dwLne_end_sequence :: Word
dwLne_end_sequence = 0x01

dwLne_set_address :: Word
dwLne_set_address = 0x02

dwLne_define_file :: Word
dwLne_define_file = 0x03

dwLne_lo_user :: Word
dwLne_lo_user = 0x80

dwLne_hi_user :: Word
dwLne_hi_user = 0xff

dwMacInfo_define :: Word
dwMacInfo_define = 0x01

dwMacInfo_undef :: Word
dwMacInfo_undef = 0x02

dwMacInfo_start_file :: Word
dwMacInfo_start_file = 0x03

dwMacInfo_end_file :: Word
dwMacInfo_end_file = 0x04

dwMacInfo_vendor_ext :: Word
dwMacInfo_vendor_ext = 0xff

dwCfa_advance_loc :: Word
dwCfa_advance_loc = 0x40

dwCfa_offset :: Word
dwCfa_offset = 0x80

dwCfa_restore :: Word
dwCfa_restore = 0xc0

dwCfa_extended :: Word
dwCfa_extended = 0

dwCfa_nop :: Word
dwCfa_nop = 0x00

dwCfa_set_loc :: Word
dwCfa_set_loc = 0x01

dwCfa_advance_loc1 :: Word
dwCfa_advance_loc1 = 0x02

dwCfa_advance_loc2 :: Word
dwCfa_advance_loc2 = 0x03

dwCfa_advance_loc4 :: Word
dwCfa_advance_loc4 = 0x04

dwCfa_offset_extended :: Word
dwCfa_offset_extended = 0x05

dwCfa_restore_extended :: Word
dwCfa_restore_extended = 0x06

dwCfa_undefined :: Word
dwCfa_undefined = 0x07

dwCfa_same_value :: Word
dwCfa_same_value = 0x08

dwCfa_register :: Word
dwCfa_register = 0x09

dwCfa_remember_state :: Word
dwCfa_remember_state = 0x0a

dwCfa_restore_state :: Word
dwCfa_restore_state = 0x0b

dwCfa_def_cfa :: Word
dwCfa_def_cfa = 0x0c

dwCfa_def_cfa_register :: Word
dwCfa_def_cfa_register = 0x0d

dwCfa_def_cfa_offset :: Word
dwCfa_def_cfa_offset = 0x0e

dwCfa_def_cfa_expression :: Word
dwCfa_def_cfa_expression = 0x0f

dwCfa_expression :: Word
dwCfa_expression = 0x10

dwCfa_cfa_offset_extended_sf :: Word
dwCfa_cfa_offset_extended_sf = 0x11

dwCfa_def_cfa_sf :: Word
dwCfa_def_cfa_sf = 0x12

dwCfa_def_cfa_offset_sf :: Word
dwCfa_def_cfa_offset_sf = 0x13

dwCfa_val_offset :: Word
dwCfa_val_offset = 0x14

dwCfa_val_offset_sf :: Word
dwCfa_val_offset_sf = 0x15

dwCfa_val_expression :: Word
dwCfa_val_expression = 0x16

dwCfa_lo_user :: Word
dwCfa_lo_user = 0x1c

dwCfa_high_user :: Word
dwCfa_high_user = 0x3f

