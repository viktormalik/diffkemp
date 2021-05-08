define void @percpu_down_read(%struct.percpu_rw_semaphore*) #4 {
  %2 = getelementptr inbounds %struct.percpu_rw_semaphore, %struct.percpu_rw_semaphore* %0, i32 0, i32 0
  %3 = call zeroext i1 @rcu_sync_is_idle(%struct.rcu_sync* %2)
  %4 = zext i1 %3 to i32
  %5 = sext i32 %4 to i64
  %6 = icmp ne i64 %5, 0
  br i1 %6, label %7, label %10

7:                                                ; preds = %1
  call void @__this_cpu_preempt_check(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.23, i64 0, i64 0))
  %8 = getelementptr inbounds %struct.percpu_rw_semaphore, %struct.percpu_rw_semaphore* %0, i32 0, i32 1
  %9 = load i32*, i32** %8, align 8
  call void @simpll__inlineasm.1(i32* %9, i32* %9)
  br label %12

10:                                               ; preds = %1
  %11 = call zeroext i1 @__percpu_down_read(%struct.percpu_rw_semaphore* %0, i1 zeroext false)
  br label %12

12:                                               ; preds = %10, %7
  ret void
}
