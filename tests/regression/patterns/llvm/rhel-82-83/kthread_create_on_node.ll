; Found differences in functions called by kthread_create_on_node
;
; __kthread_create_on_node differs:
;   Callstack (snapshots/linux-4.18.0-193.el8):
;   __kthread_create_on_node at kernel/kthread.c:380
;
;   Callstack (snapshots/linux-4.18.0-240.el8):
;   __kthread_create_on_node at kernel/kthread.c:414
;
;   Diff:
;   *************** struct task_struct *__kthread_create_on_node(int (*threadfn)(void *data),
;   *** 341,343 ***
;     		sched_setscheduler_nocheck(task, SCHED_NORMAL, &param);
;   ! 		set_cpus_allowed_ptr(task, cpu_all_mask);
;     	}
;   --- 374,377 ---
;     		sched_setscheduler_nocheck(task, SCHED_NORMAL, &param);
;   ! 		set_cpus_allowed_ptr(task,
;   ! 				     housekeeping_cpumask(HK_FLAG_KTHREAD));
;     	}

; Metadata
!0 = !{ !"pattern-start" }
!1 = !{ !"pattern-end" }

; Structures
%struct.kthread_create_info = type { i32 (i8*)*, i8*, i32, %struct.task_struct*, %struct.completion*, %struct.list_head }
%struct.task_struct = type {}
%struct.completion = type {}
%struct.list_head = type {}
%struct.cpumask = type {}

; Globals
@diffkemp.old.cpu_all_bits = external constant [128 x i64], align 16

; Functions
declare void @diffkemp.mapping(...)
declare %struct.cpumask* @diffkemp.new.housekeeping_cpumask(i32)
declare i8* @kmalloc(...)
declare i32 @set_cpus_allowed_ptr(...)

define void @diffkemp.old.__kthread_create_on_node() {
  %1 = call i8* (...) @kmalloc(i64 56, i32 6291648)
  %2 = bitcast i8* %1 to %struct.kthread_create_info*
  %3 = getelementptr inbounds %struct.kthread_create_info, %struct.kthread_create_info* %2, i32 0, i32 3
  %4 = load %struct.task_struct*, %struct.task_struct** %3, align 8
  %5 = call i32 (...) @set_cpus_allowed_ptr(%struct.task_struct* %4, %struct.cpumask* bitcast ([128 x i64]* @diffkemp.old.cpu_all_bits to %struct.cpumask*)), !diffkemp.pattern !0
  br label %aBB, !diffkemp.pattern !1

aBB:
  call void (...) @diffkemp.mapping(i32 %5)
  ret void
}

define void @diffkemp.new.__kthread_create_on_node() {
  %1 = call i8* (...) @kmalloc(i64 56, i32 6291648)
  %2 = bitcast i8* %1 to %struct.kthread_create_info*
  %3 = getelementptr inbounds %struct.kthread_create_info, %struct.kthread_create_info* %2, i32 0, i32 3
  %4 = load %struct.task_struct*, %struct.task_struct** %3, align 8
  %5 = call %struct.cpumask* @diffkemp.new.housekeeping_cpumask(i32 256), !diffkemp.pattern !0
  %6 = call i32 (...) @set_cpus_allowed_ptr(%struct.task_struct* %4, %struct.cpumask* %5)
  br label %aBB, !diffkemp.pattern !1

aBB:
  call void (...) @diffkemp.mapping(i32 %6)
  ret void
}
