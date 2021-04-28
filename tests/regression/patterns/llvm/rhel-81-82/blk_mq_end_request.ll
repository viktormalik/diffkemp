; Found differences in functions called by blk_mq_end_request
;
; arch_atomic_dec differs:
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   __blk_mq_end_request at block/blk-mq.c:566
;   blk_mq_free_request at block/blk-mq.c:557
;   atomic_dec at block/blk-mq.c:521
;   arch_atomic_dec at ./include/asm-generic/atomic-instrumented.h:115

;   Callstack (snapshots/linux-4.18.0-193.el8):
;   __blk_mq_end_request at block/blk-mq.c:562
;   blk_mq_free_request at block/blk-mq.c:553
;   atomic_dec at block/blk-mq.c:517
;   arch_atomic_dec at ./include/asm-generic/atomic-instrumented.h:115

;   Diff:
;   *************** static __always_inline void arch_atomic_dec(atomic_t *v)
;   *** 108,110 ***
;     	asm volatile(LOCK_PREFIX "decl %0"
;   ! 		     : "+m" (v->counter));
;     }
;   --- 108,110 ---
;     	asm volatile(LOCK_PREFIX "decl %0"
;   ! 		     : "+m" (v->counter) :: "memory");
;     }

; Metadata
!0 = !{ !"pattern-start" }
!1 = !{ !"pattern-end" }

; Structures
%struct.custom = type { i32 }

; Functions
declare void @simpll__inlineasm(...)

define void @diffkemp.old.arch_atomic_dec(%struct.custom*) {
  %2 = getelementptr inbounds %struct.custom, %struct.custom* %0, i32 0, i32 0
  call void (...) @simpll__inlineasm(i32* %2, i32* %2), !diffkemp.pattern !0
  ret void, !diffkemp.pattern !1
}

define void @diffkemp.new.arch_atomic_dec(%struct.custom*) {
  %2 = getelementptr inbounds %struct.custom, %struct.custom* %0, i32 0, i32 0
  call void (...) @simpll__inlineasm(i32* %2, i32* %2), !diffkemp.pattern !0
  ret void, !diffkemp.pattern !1
}
