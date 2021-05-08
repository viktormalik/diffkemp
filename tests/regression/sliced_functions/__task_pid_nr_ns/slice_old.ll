define void @__task_pid_nr_ns(%struct.task_struct*, i32, %struct.pid_namespace*) #0 {
  %4 = alloca %union.anon.16667506017158663808.24, align 8
  br label %5

5:                                                ; preds = %3
  switch i32 %1, label %7 [
    i32 0, label %10
    i32 4, label %6
  ]

6:                                                ; preds = %5
  br label %7

7:                                                ; preds = %6, %5
  %.01 = phi i32 [ 0, %6 ], [ %1, %5 ]
  %8 = getelementptr inbounds %struct.task_struct, %struct.task_struct* %0, i32 0, i32 55
  %9 = load %struct.task_struct*, %struct.task_struct** %8, align 64
  br label %10

10:                                               ; preds = %7, %5
  %.1 = phi i32 [ %.01, %7 ], [ 0, %5 ]
  %.0 = phi %struct.task_struct* [ %9, %7 ], [ %0, %5 ]
  %11 = getelementptr inbounds %struct.task_struct, %struct.task_struct* %.0, i32 0, i32 58
  %12 = zext i32 %.1 to i64
  %13 = getelementptr inbounds [3 x %struct.pid_link], [3 x %struct.pid_link]* %11, i64 0, i64 %12
  %14 = getelementptr inbounds %struct.pid_link, %struct.pid_link* %13, i32 0, i32 1
  %15 = bitcast %struct.pid** %14 to i8*
  %16 = bitcast %union.anon.16667506017158663808.24* %4 to [1 x i8]*
  %17 = getelementptr inbounds [1 x i8], [1 x i8]* %16, i64 0, i64 0
  call void @__read_once_size(i8* %15, i8* %17, i32 8)
  br label %18

18:                                               ; preds = %10
  ret void
}
