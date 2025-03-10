/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium
Copyright (c) 2014 Surplus Users Ham Society
Copyright (c) 2022-2023 CERN

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of
the software without specific, written prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

/* file was renamed from phigscfunc.h */

#ifndef _phigsfunc_h
#define _phigsfunc_h

#ifdef __cplusplus
extern "C" {
#endif

#define		Pfn_open_phigs			(0)
#define		Pfn_close_phigs			(1)
#define		Pfn_open_ws			(2)
#define		Pfn_close_ws			(3)
#define		Pfn_redraw_all_structs		(4)
#define		Pfn_upd_ws			(5)
#define		Pfn_set_disp_upd_st		(6)
#define		Pfn_message			(7)
#define		Pfn_polyline3			(8)
#define		Pfn_polyline			(9)
#define		Pfn_polymarker3			(10)
#define		Pfn_polymarker			(11)
#define		Pfn_text3			(12)
#define		Pfn_text			(13)
#define		Pfn_anno_text_rel3		(14)
#define		Pfn_anno_text_rel		(15)
#define		Pfn_fill_area3			(16)
#define		Pfn_fill_area			(17)
#define		Pfn_fill_area_set3		(18)
#define		Pfn_fill_area_set		(19)
#define		Pfn_cell_array3			(20)
#define		Pfn_cell_array			(21)
#define		Pfn_gdp3			(22)
#define		Pfn_gdp				(23)
#define		Pfn_set_line_ind		(24)
#define		Pfn_set_marker_ind		(25)
#define		Pfn_set_text_ind		(26)
#define		Pfn_set_int_ind			(27)
#define		Pfn_set_edge_ind		(28)
#define		Pfn_set_linetype		(29)
#define		Pfn_set_linewidth		(30)
#define		Pfn_set_line_colr_ind		(31)
#define		Pfn_set_marker_type		(32)
#define		Pfn_set_marker_size		(33)
#define		Pfn_set_marker_colr_ind		(34)
#define		Pfn_set_text_font		(35)
#define		Pfn_set_text_prec		(36)
#define		Pfn_set_char_expan		(37)
#define		Pfn_set_char_space		(38)
#define		Pfn_set_text_colr_ind		(39)
#define		Pfn_set_char_ht			(40)
#define		Pfn_set_char_up_vec		(41)
#define		Pfn_set_text_path		(42)
#define		Pfn_set_text_align		(43)
#define		Pfn_set_anno_char_ht		(44)
#define		Pfn_set_anno_char_up_vec	(45)
#define		Pfn_set_anno_path		(46)
#define		Pfn_set_anno_align		(47)
#define		Pfn_set_anno_style		(48)
#define		Pfn_set_int_style		(49)
#define		Pfn_set_int_style_ind		(50)
#define		Pfn_set_int_colr_ind		(51)
#define		Pfn_set_edge_flag		(52)
#define		Pfn_set_edgetype		(53)
#define		Pfn_set_edgewidth		(54)
#define		Pfn_set_edge_colr_ind		(55)
#define		Pfn_set_pat_size		(56)
#define		Pfn_set_pat_ref_point_vecs	(57)
#define		Pfn_set_pat_ref_point		(58)
#define		Pfn_add_names_set		(59)
#define		Pfn_remove_names_set		(60)
#define		Pfn_set_indiv_asf		(61)
#define		Pfn_set_line_rep		(62)
#define		Pfn_set_marker_rep		(63)
#define		Pfn_set_text_rep		(64)
#define		Pfn_set_int_rep			(65)
#define		Pfn_set_edge_rep		(66)
#define		Pfn_set_pat_rep			(67)
#define		Pfn_set_colr_rep		(68)
#define		Pfn_set_highl_filter		(69)
#define		Pfn_set_invis_filter		(70)
#define		Pfn_set_colr_model		(71)
#define		Pfn_set_hlhsr_id		(72)
#define		Pfn_set_hlhsr_mode		(73)
#define		Pfn_set_local_tran3		(74)
#define		Pfn_set_local_tran		(75)
#define		Pfn_set_global_tran3		(76)
#define		Pfn_set_global_tran		(77)
#define		Pfn_set_model_clip_vol3		(78)
#define		Pfn_set_model_clip_vol		(79)
#define		Pfn_set_model_clip_ind		(80)
#define		Pfn_restore_model_clip_vol	(81)
#define		Pfn_set_view_ind		(82)
#define		Pfn_set_view_rep3		(83)
#define		Pfn_set_view_rep		(84)
#define		Pfn_set_view_tran_in_pri	(85)
#define		Pfn_set_ws_win3			(86)
#define		Pfn_set_ws_win			(87)
#define		Pfn_set_ws_vp3			(88)
#define		Pfn_set_ws_vp			(89)
#define		Pfn_open_struct			(90)
#define		Pfn_close_struct		(91)
#define		Pfn_exec_struct			(92)
#define		Pfn_label			(93)
#define		Pfn_appl_data			(94)
#define		Pfn_gse				(95)
#define		Pfn_set_edit_mode		(96)
#define		Pfn_copy_all_elems_struct	(97)
#define		Pfn_set_elem_ptr		(98)
#define		Pfn_offset_elem_ptr		(99)
#define		Pfn_set_elem_ptr_label		(100)
#define		Pfn_del_elem			(101)
#define		Pfn_del_elem_range		(102)
#define		Pfn_del_elems_labels		(103)
#define		Pfn_empty_struct		(104)
#define		Pfn_del_struct			(105)
#define		Pfn_del_struct_net		(106)
#define		Pfn_del_all_struct		(107)
#define		Pfn_change_struct_id		(108)
#define		Pfn_change_struct_refs		(109)
#define		Pfn_change_struct_id_refs	(110)
#define		Pfn_post_struct			(111)
#define		Pfn_unpost_struct		(112)
#define		Pfn_unpost_all_structs		(113)
#define		Pfn_open_ar_file		(114)
#define		Pfn_close_ar_file		(115)
#define		Pfn_ar_structs			(116)
#define		Pfn_ar_struct_nets		(117)
#define		Pfn_ar_all_structs		(118)
#define		Pfn_set_conf_res		(119)
#define		Pfn_ret_struct_ids		(120)
#define		Pfn_ret_structs			(121)
#define		Pfn_ret_struct_nets		(122)
#define		Pfn_ret_all_structs		(123)
#define		Pfn_ret_paths_ancest		(124)
#define		Pfn_ret_paths_descs		(125)
#define		Pfn_del_structs_ar		(126)
#define		Pfn_del_struct_nets_ar		(127)
#define		Pfn_del_all_structs_ar		(128)
#define		Pfn_set_pick_id			(129)
#define		Pfn_set_pick_filter		(130)
#define		Pfn_init_loc3			(131)
#define		Pfn_init_loc			(132)
#define		Pfn_init_stroke3		(133)
#define		Pfn_init_stroke			(134)
#define		Pfn_init_val3			(135)
#define		Pfn_init_val			(136)
#define		Pfn_init_choice3		(137)
#define		Pfn_init_choice			(138)
#define		Pfn_init_pick3			(139)
#define		Pfn_init_pick			(140)
#define		Pfn_init_string3		(141)
#define		Pfn_init_string			(142)
#define		Pfn_set_loc_mode		(143)
#define		Pfn_set_stroke_mode		(144)
#define		Pfn_set_val_mode		(145)
#define		Pfn_set_choice_mode		(146)
#define		Pfn_set_pick_mode		(147)
#define		Pfn_set_string_mode		(148)
#define		Pfn_req_loc3			(149)
#define		Pfn_req_loc			(150)
#define		Pfn_req_stroke3			(151)
#define		Pfn_req_stroke			(152)
#define		Pfn_req_val			(153)
#define		Pfn_req_choice			(154)
#define		Pfn_req_pick			(155)
#define		Pfn_req_string			(156)
#define		Pfn_sample_loc3			(157)
#define		Pfn_sample_loc			(158)
#define		Pfn_sample_stroke3		(159)
#define		Pfn_sample_stroke		(160)
#define		Pfn_sample_val			(161)
#define		Pfn_sample_choice		(162)
#define		Pfn_sample_pick			(163)
#define		Pfn_sample_string		(164)
#define		Pfn_await_event			(165)
#define		Pfn_flush_events		(166)
#define		Pfn_get_loc3			(167)
#define		Pfn_get_loc			(168)
#define		Pfn_get_stroke3			(169)
#define		Pfn_get_stroke			(170)
#define		Pfn_get_val			(171)
#define		Pfn_get_choice			(172)
#define		Pfn_get_pick			(173)
#define		Pfn_get_string			(174)
#define		Pfn_write_item			(175)
#define		Pfn_get_item_type		(176)
#define		Pfn_read_item			(177)
#define		Pfn_interpret_item		(178)
#define		Pfn_set_err_hand_mode		(179)
#define		Pfn_escape			(180)

#define         Pfn_fill_area_set_data          (301)
#define         Pfn_fill_area_set3_data         (302)
#define         Pfn_set_of_fill_area_set3_data  (304)
#define         Pfn_set_line_colr               (310)
#define         Pfn_set_marker_colr             (312)
#define         Pfn_set_text_colr               (313)
#define         Pfn_set_face_disting_mode       (314)
#define         Pfn_set_face_cull_mode          (315)
#define         Pfn_set_int_colr                (316)
#define         Pfn_set_int_shad_meth           (317)
#define         Pfn_set_refl_props              (318)
#define         Pfn_set_refl_eqn                (319)
#define         Pfn_set_back_int_style          (320)
#define         Pfn_set_back_int_style_ind      (321)
#define         Pfn_set_back_int_colr           (322)
#define         Pfn_set_back_int_shad_meth      (323)
#define         Pfn_set_back_refl_props         (324)
#define         Pfn_set_back_refl_eqn           (325)
#define         Pfn_set_light_src_state         (326)
#define         Pfn_set_edge_colr               (327)
#define         Pfn_set_light_src_rep           (340)

#define         Pfn_set_alpha_channel           (900)
#define         Pfn_INQUIRY                     (1000)

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _phigsfunc_h */
