; Found differences in functions called by __put_page
;
; __update_lru_size differs:
;   Callstack (snapshots/linux-4.18.0-193.el8):
;   __put_compound_page at mm/swap.c:111
;   __page_cache_release at mm/swap.c:93
;   del_page_from_lru_list at mm/swap.c:69
;   update_lru_size at ./include/linux/mm_inline.h:65
;   __update_lru_size at ./include/linux/mm_inline.h:41
;
;   Callstack (snapshots/linux-4.18.0-240.el8):
;   __put_compound_page at mm/swap.c:111
;   __page_cache_release at mm/swap.c:93
;   del_page_from_lru_list at mm/swap.c:69
;   update_lru_size at ./include/linux/mm_inline.h:65
;   __update_lru_size at ./include/linux/mm_inline.h:41
;
;   Diff:
;   *************** static __always_inline void __update_lru_size(struct lruvec *lruvec,
;   *** 31,33 ***
;
;   ! 	__mod_node_page_state(pgdat, NR_LRU_BASE + lru, nr_pages);
;     	__mod_zone_page_state(&pgdat->node_zones[zid],
;   --- 31,33 ---
;
;   ! 	__mod_lruvec_state(lruvec, NR_LRU_BASE + lru, nr_pages);
;     	__mod_zone_page_state(&pgdat->node_zones[zid],
;
; Note: When this pattern gets matched, the following difference gets reported
; as well. However, function percpu_ref_put_many is covered by the bio_endio
; pattern. In other words, complete function equality is expected.
;
; percpu_ref_put_many differs:
;   Callstack (snapshots/linux-4.18.0-193.el8):
;   put_dev_pagemap at mm/swap.c:101
;   percpu_ref_put at ./include/linux/memremap.h:194
;   percpu_ref_put_many at ./include/linux/percpu-refcount.h:301
;
;   Callstack (snapshots/linux-4.18.0-240.el8):
;   put_dev_pagemap at mm/swap.c:101
;   percpu_ref_put at ./include/linux/memremap.h:207
;   percpu_ref_put_many at ./include/linux/percpu-refcount.h:325
;
;   Diff:
;   *************** static inline void percpu_ref_put_many(struct percpu_ref *ref, unsigned long nr)
;   *** 279,281 ***
;
;   !   rcu_read_lock_sched();
;
;   --- 303,305 ---
;
;   !   rcu_read_lock();
;
;   *************** static inline void percpu_ref_put_many(struct percpu_ref *ref, unsigned long nr)
;   *** 286,288 ***
;
;   !   rcu_read_unlock_sched();
;     }
;   --- 310,312 ---
;
;   !   rcu_read_unlock();
;     }

; Metadata
!0 = !{ !"pattern-start" }

; Structures
%struct.lruvec = type {}
%struct.pglist_data = type {}

; Functions
declare %struct.pglist_data* @lruvec_pgdat(...)
declare void @__mod_node_page_state(...)
declare void @__mod_lruvec_state(...)

define void @diffkemp.old.__update_lru_size(%struct.lruvec*, i32, i32) {
  %4 = call %struct.pglist_data* (...) @lruvec_pgdat(%struct.lruvec* %0)
  %5 = sext i32 %2 to i64
  call void (...) @__mod_node_page_state(%struct.pglist_data* %4, i32 %1, i64 %5), !diffkemp.pattern !0
  ret void
}

define void @diffkemp.new.__update_lru_size(%struct.lruvec*, i32, i32) {
  %4 = call %struct.pglist_data* (...) @lruvec_pgdat(%struct.lruvec* %0)
  call void (...) @__mod_lruvec_state(%struct.lruvec* %0, i32 %1, i32 %2), !diffkemp.pattern !0
  ret void
}
