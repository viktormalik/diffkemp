; Found differences in functions called by __put_task_struct
;
; __put_task_struct differs:
;   Diff:
;   *************** void __put_task_struct(struct task_struct *tsk)
;   *** 726,728 ***
;     	cgroup_free(tsk);
;   ! 	task_numa_free(tsk);
;     	security_task_free(tsk);
;   --- 725,727 ---
;     	cgroup_free(tsk);
;   ! 	task_numa_free(tsk, true);
;     	security_task_free(tsk);

; Structures
%diffkemp.old.struct.task_struct = type {}
%diffkemp.new.struct.task_struct = type {}

; Functions
declare void @diffkemp.old.task_numa_free(%diffkemp.old.struct.task_struct*)
declare void @diffkemp.new.task_numa_free(%diffkemp.new.struct.task_struct*, i1)

define void @diffkemp.old.__put_task_struct(%diffkemp.old.struct.task_struct*) {
  call void @diffkemp.old.task_numa_free(%diffkemp.old.struct.task_struct* %0)
  ret void
}

define void @diffkemp.new.__put_task_struct(%diffkemp.new.struct.task_struct*) {
  call void @diffkemp.new.task_numa_free(%diffkemp.new.struct.task_struct* %0, i1 zeroext true)
  ret void
}
