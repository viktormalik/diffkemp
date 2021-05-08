define void @percpu_down_read(%struct.percpu_rw_semaphore*) #4 {
  call void @__this_cpu_preempt_check(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.22, i64 0, i64 0))
  %2 = getelementptr inbounds %struct.percpu_rw_semaphore, %struct.percpu_rw_semaphore* %0, i32 0, i32 1
  %3 = load i32*, i32** %2, align 8
  call void @simpll__inlineasm.1(i32* %3, i32* %3)
  %4 = getelementptr inbounds %struct.percpu_rw_semaphore, %struct.percpu_rw_semaphore* %0, i32 0, i32 0
  %5 = call zeroext i1 @rcu_sync_is_idle(%struct.rcu_sync* %4)
  %6 = xor i1 %5, true
  %7 = zext i1 %6 to i32
  %8 = sext i32 %7 to i64
  %9 = icmp ne i64 %8, 0
  br i1 %9, label %10, label %12

10:                                               ; preds = %1
  %11 = call i32 @__percpu_down_read(%struct.percpu_rw_semaphore* %0, i32 0)
  br label %12

12:                                               ; preds = %10, %1
  ret void
}
