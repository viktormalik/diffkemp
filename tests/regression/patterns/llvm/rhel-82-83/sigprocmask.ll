; Found differences in functions called by sigprocmask
;
; JOBCTL_PENDING_MASK differs:
;   Callstack (snapshots/linux-4.18.0-193.el8):
;   __set_current_blocked at kernel/signal.c:2857
;   __set_task_blocked at kernel/signal.c:2822
;   recalc_sigpending at kernel/signal.c:2794
;   recalc_sigpending_tsk at kernel/signal.c:181
;   JOBCTL_PENDING_MASK (macro) at kernel/signal.c:153
;
;   Callstack (snapshots/linux-4.18.0-240.el8):
;   __set_current_blocked at kernel/signal.c:2893
;   __set_task_blocked at kernel/signal.c:2858
;   recalc_sigpending at kernel/signal.c:2830
;   recalc_sigpending_tsk at kernel/signal.c:181
;   JOBCTL_PENDING_MASK (macro) at kernel/signal.c:153
;
;   Diff:
;     (JOBCTL_STOP_PENDING | JOBCTL_TRAP_MASK)
;
;     (JOBCTL_STOP_PENDING | JOBCTL_TRAP_MASK | JOBCTL_TASK_WORK)

define i64 @diffkemp.old.JOBCTL_PENDING_MASK() {
  ret i64 10092544
}

define i64 @diffkemp.new.JOBCTL_PENDING_MASK() {
  ret i64 26869760
}
