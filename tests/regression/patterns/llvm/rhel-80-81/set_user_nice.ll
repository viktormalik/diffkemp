; Found differences in functions called by set_user_nice
;
; dequeue_task differs:
;   Callstack (snapshots/linux-4.18.0-80.el8):
;   dequeue_task at kernel/sched/core.c:3930
;
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   dequeue_task at kernel/sched/core.c:3919
;
;   Diff:
;   *************** static inline void dequeue_task(struct rq *rq, struct task_struct *p, int flags)
;   *** 770,773 ***
;
;   ! 	if (!(flags & DEQUEUE_SAVE))
;     		sched_info_dequeued(rq, p);
;
;   --- 750,755 ---
;
;   ! 	if (!(flags & DEQUEUE_SAVE)) {
;     		sched_info_dequeued(rq, p);
;   + 		psi_dequeue(p, flags & DEQUEUE_SLEEP);
;   + 	}
;
;
; enqueue_task differs:
;   Callstack (snapshots/linux-4.18.0-80.el8):
;   enqueue_task at kernel/sched/core.c:3941
;
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   enqueue_task at kernel/sched/core.c:3930
;
;   Diff:
;   *************** static inline void enqueue_task(struct rq *rq, struct task_struct *p, int flags)
;   *** 759,762 ***
;
;   ! 	if (!(flags & ENQUEUE_RESTORE))
;     		sched_info_queued(rq, p);
;
;   --- 737,742 ---
;
;   ! 	if (!(flags & ENQUEUE_RESTORE)) {
;     		sched_info_queued(rq, p);
;   + 		psi_enqueue(p, flags & ENQUEUE_WAKEUP);
;   + 	}

; Metadata
!0 = !{ !"pattern-start" }
!1 = !{ !"pattern-end" }

; Structures
%struct.task_struct = type {}

; Functions
declare void @psi_dequeue(...)
declare void @diffkemp.psi_enqueue(...)

define void @diffkemp.old.dequeue_task(%struct.task_struct*, i32) {
  br label %aBB, !diffkemp.pattern !0

aBB:
  ret void
}

define void @diffkemp.new.dequeue_task(%struct.task_struct*, i32) {
  %3 = and i32 %1, 1, !diffkemp.pattern !0
  %4 = icmp ne i32 %3, 0
  call void (...) @psi_dequeue(%struct.task_struct* %0, i1 zeroext %4)
  br label %aBB

aBB:
  ret void
}

define void @diffkemp.old.enqueue_task(%struct.task_struct*, i32) {
  br label %aBB

aBB:
  ret void, !diffkemp.pattern !1
}

define void @diffkemp.new.enqueue_task(%struct.task_struct*, i32) {
  %3 = and i32 %1, 1
  %4 = icmp ne i32 %3, 0
  call void (...) @diffkemp.psi_enqueue(%struct.task_struct* %0, i1 zeroext %4)
  br label %aBB

aBB:
  ret void, !diffkemp.pattern !1
}
