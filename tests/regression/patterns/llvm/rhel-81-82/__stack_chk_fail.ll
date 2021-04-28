; Found differences in functions called by __stack_chk_fail
;
; panic differs:
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   panic at kernel/panic.c:648
;
;   Callstack (snapshots/linux-4.18.0-193.el8):
;   panic at kernel/panic.c:683
;
;   Diff:
;   *************** void panic(const char *fmt, ...)
;   *** 246,248 ***
;     	debug_locks_off();
;   ! 	console_flush_on_panic();
;
;   --- 276,280 ---
;     	debug_locks_off();
;   ! 	console_flush_on_panic(CONSOLE_FLUSH_PENDING);
;   ! 
;   ! 	panic_print_sys_info();
;
;   *************** void panic(const char *fmt, ...)
;   *** 293,294 ***
;   --- 325,329 ---
;     	pr_emerg("---[ end Kernel panic - not syncing: %s ]---\n", buf);
;   + 
;   + 	/* Do not scroll important messages printed above */
;   + 	suppress_printk = 1;
;     	local_irq_enable();

; Metadata
!0 = !{ !"pattern-start" }
!1 = !{ !"pattern-end" }
!2 = !{ !"pattern-start", !"group-start" }
!3 = !{ !"group-end" }

; Globals
@suppress_printk = external global i32, align 4

; Functions
declare void @diffkemp.mapping(...)
declare void @console_flush_on_panic(...)
declare void @panic_print_sys_info(...)
declare void @arch_local_irq_enable(...)

define void @diffkemp.old.panic_first() {
  call void (...) @console_flush_on_panic(), !diffkemp.pattern !0
  ret void, !diffkemp.pattern !1
}

define void @diffkemp.new.panic_first() {
  call void (...) @console_flush_on_panic(i32 0), !diffkemp.pattern !2
  call void (...) @panic_print_sys_info(), !diffkemp.pattern !3
  ret void, !diffkemp.pattern !1
}

define void @diffkemp.old.panic_second() {
  call void (...) @arch_local_irq_enable(), !diffkemp.pattern !0
  br label %aBB, !diffkemp.pattern !1

aBB:
  call void (...) @diffkemp.mapping()
  ret void
}

define void @diffkemp.new.panic_second() {
  store i32 1, i32* @suppress_printk, align 4, !diffkemp.pattern !2
  call void (...) @arch_local_irq_enable(), !diffkemp.pattern !3
  br label %bBB, !diffkemp.pattern !1

bBB:
  call void (...) @diffkemp.mapping()
  ret void
}
