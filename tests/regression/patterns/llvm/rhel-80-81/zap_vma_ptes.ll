; Found differences in functions called by zap_vma_ptes
;
; SWP_OFFSET_MASK differs:
;   Callstack (snapshots/linux-4.18.0-80.el8):
;   zap_page_range_single at mm/memory.c:1670
;   unmap_single_vma at mm/memory.c:1647
;   unmap_page_range at mm/memory.c:1556
;   zap_p4d_range at mm/memory.c:1511
;   zap_pud_range at mm/memory.c:1490
;   zap_pmd_range at mm/memory.c:1469
;   zap_pte_range at mm/memory.c:1440
;   device_private_entry_to_page at mm/memory.c:1351
;   swp_offset at ./include/linux/swapops.h:134
;   SWP_OFFSET_MASK (macro) at ./include/linux/swapops.h:51
;
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   zap_page_range_single at mm/memory.c:1670
;   unmap_single_vma at mm/memory.c:1647
;   unmap_page_range at mm/memory.c:1556
;   zap_p4d_range at mm/memory.c:1511
;   zap_pud_range at mm/memory.c:1490
;   zap_pmd_range at mm/memory.c:1469
;   zap_pte_range at mm/memory.c:1440
;   device_private_entry_to_page at mm/memory.c:1351
;   swp_offset at ./include/linux/swapops.h:129
;   SWP_OFFSET_MASK (macro) at ./include/linux/swapops.h:49
;
;   Diff:
;     ((1UL << SWP_TYPE_SHIFT(e)) - 1)
;
;     ((1UL << SWP_TYPE_SHIFT) - 1)

define i64 @diffkemp.old.SWP_OFFSET_MASK(i64) {
  %2 = and i64 %0, 144115188075855871
  ret i64 %2
}

define i64 @diffkemp.new.SWP_OFFSET_MASK(i64) {
  %2 = and i64 %0, 288230376151711743
  ret i64 %2
}
