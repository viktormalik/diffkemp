define void @blk_cleanup_queue(%struct.request_queue*) #2 {
  %2 = call i32 @_cond_resched()
  %3 = getelementptr inbounds %struct.request_queue, %struct.request_queue* %0, i32 0, i32 15
  %4 = call zeroext i1 @constant_test_bit(i64 26, i64* %3)
  %5 = zext i1 %4 to i32
  %6 = sext i32 %5 to i64
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

8:                                                ; preds = %1
  call void @simpll__inlineasm.2(i8* null, i32 0, i32 2307, i64 12)
  call void @simpll__inlineasm.3(i32 141)
  br label %9

9:                                                ; preds = %8, %1
  call void @blk_set_queue_dying(%struct.request_queue* %0)
  call void @blk_queue_flag_set(i32 5, %struct.request_queue* %0)
  call void @blk_queue_flag_set(i32 12, %struct.request_queue* %0)
  call void @blk_queue_flag_set(i32 2, %struct.request_queue* %0)
  br label %10

10:                                               ; preds = %9
  %11 = getelementptr inbounds %struct.request_queue, %struct.request_queue* %0, i32 0, i32 54
  call void @mutex_lock(%struct.mutex* %11)
  br label %12

12:                                               ; preds = %10
  ret void
}
