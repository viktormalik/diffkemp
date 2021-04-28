; Found differences in functions called by down_read
;
; __down_read differs:
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   __down_read at kernel/locking/rwsem.c:26
;
;   Callstack (snapshots/linux-4.18.0-193.el8):
;   __down_read at kernel/locking/rwsem.c:1505
;
;   Diff:
;   *************** static inline void __down_read(struct rw_semaphore *sem)
;   *** 175,181 ***
;   ! static inline void __down_read(struct rw_semaphore *sem)
;     {
;   ! 	if (unlikely(atomic_long_inc_return_acquire(&sem->count) <= 0)) {
;   ! 		rwsem_down_read_failed(sem);
;   ! 		DEBUG_RWSEMS_WARN_ON(!((unsigned long)sem->owner &
;   ! 					RWSEM_READER_OWNED), sem);
;     	} else {
;   --- 1349,1354 ---
;   ! inline void __down_read(struct rw_semaphore *sem)
;     {
;   ! 	if (!rwsem_read_trylock(sem)) {
;   ! 		rwsem_down_read_slowpath(sem, TASK_UNINTERRUPTIBLE);
;   ! 		DEBUG_RWSEMS_WARN_ON(!is_rwsem_reader_owned(sem), sem);
;     	} else {

; Metadata
!0 = !{ !"pattern-start" }
!1 = !{ !"pattern-end" }

; Structures
%struct.rw_semaphore = type { %struct.atomic64_t, %struct.list_head, %struct.raw_spinlock, %struct.optimistic_spin_queue, %struct.task_struct* }
%struct.atomic64_t = type {}
%struct.list_head = type {}
%struct.raw_spinlock = type {}
%struct.optimistic_spin_queue = type {}
%struct.task_struct = type {}

; Functions
declare i64 @atomic_long_inc_return_acquire(...)
declare %struct.rw_semaphore* @rwsem_down_read_failed(...)
declare void @rwsem_set_reader_owned(...)
declare i1 @rwsem_read_trylock(...)
declare %struct.rw_semaphore* @rwsem_down_read_slowpath(...)

define void @diffkemp.old.__down_read(%struct.rw_semaphore*) {
  %2 = getelementptr inbounds %struct.rw_semaphore, %struct.rw_semaphore* %0, i32 0, i32 0
  br label %aBB

aBB:
  %3 = call i64 (...) @atomic_long_inc_return_acquire(%struct.atomic64_t* %2), !diffkemp.pattern !0
  %4 = icmp sle i64 %3, 0
  %5 = zext i1 %4 to i32
  %6 = sext i32 %5 to i64
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %bBB, label %cBB

bBB:
  %8 = call %struct.rw_semaphore* (...) @rwsem_down_read_failed(%struct.rw_semaphore* %0)
  br label %dBB, !diffkemp.pattern !1

cBB:
  call void (...) @rwsem_set_reader_owned(%struct.rw_semaphore* %0)
  br label %dBB, !diffkemp.pattern !1

dBB:
  ret void
}

define void @diffkemp.new.__down_read(%struct.rw_semaphore*) {
  %2 = call zeroext i1 (...) @rwsem_read_trylock(%struct.rw_semaphore* %0), !diffkemp.pattern !0
  br i1 %2, label %bBB, label %aBB

aBB:
  %3 = call %struct.rw_semaphore* (...) @rwsem_down_read_slowpath(%struct.rw_semaphore* %0, i32 2)
  br label %cBB, !diffkemp.pattern !1

bBB:
  call void (...) @rwsem_set_reader_owned(%struct.rw_semaphore* %0)
  br label %cBB, !diffkemp.pattern !1

cBB:
  ret void
}
