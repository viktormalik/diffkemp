define i32 @wait_on_page_bit_common(%struct.wait_queue_head*, %struct.page*, i32, i32, i32) #5 {
  br label %6

6:                                                ; preds = %11, %5
  %7 = sext i32 %3 to i64
  br label %8

8:                                                ; preds = %6
  %9 = icmp eq i32 %4, 2
  br label %10

10:                                               ; preds = %8
  switch i32 %4, label %11 [
    i32 0, label %11
    i32 1, label %11
  ]

11:                                               ; preds = %10, %10, %10
  %12 = call %struct.task_struct* @get_current()
  %13 = call i32 @signal_pending_state(i64 %7, %struct.task_struct* %12)
  %14 = icmp ne i32 %13, 0
  %15 = zext i1 %14 to i32
  %16 = sext i32 %15 to i64
  %17 = icmp ne i64 %16, 0
  %brmerge = or i1 %17, %9
  %.mux = select i1 %17, i32 -4, i32 0
  br i1 %brmerge, label %18, label %6

18:                                               ; preds = %11
  ret i32 undef
}
