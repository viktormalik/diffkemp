; Found differences in functions called by blk_execute_rq
;
; blk_execute_rq_nowait differs:
;   Callstack (snapshots/linux-4.18.0-193.el8):
;   blk_execute_rq_nowait at block/blk-exec.c:83
;
;   Callstack (snapshots/linux-4.18.0-240.el8):
;   blk_execute_rq_nowait at block/blk-exec.c:85
;
;   Diff:
;   *************** void blk_execute_rq_nowait(struct request_queue *q, struct gendisk *bd_disk,
;   *** 56,57 ***
;   --- 56,59 ---
;
;   + 	blk_account_io_start(rq, true);
;   + 
;     	/*

; Metadata
!0 = !{ !"pattern-start" }
!1 = !{ !"pattern-end" }

; Structures
%struct.request = type {}

; Functions
declare void @blk_account_io_start(...)

define i1 @diffkemp.old.blk_execute_rq_nowait(%struct.request*, i32) {
  %3 = icmp ne i32 %1, 0, !diffkemp.pattern !0
  ret i1 %3, !diffkemp.pattern !1
}

define i1 @diffkemp.new.blk_execute_rq_nowait(%struct.request*, i32) {
  call void (...) @blk_account_io_start(%struct.request* %0, i1 zeroext true), !diffkemp.pattern !0
  %3 = icmp ne i32 %1, 0
  ret i1 %3, !diffkemp.pattern !1
}

