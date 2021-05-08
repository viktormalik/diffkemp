define void @__task_pid_nr_ns(%struct.task_struct*, i32, %struct.pid_namespace*) #0 {
  %4 = alloca %union.anon.16667506017158663808.23, align 8
  br label %5

5:                                                ; preds = %3
  %6 = call %struct.pid** @task_pid_ptr(%struct.task_struct* %0, i32 %1)
  %7 = bitcast %struct.pid** %6 to i8*
  %8 = bitcast %union.anon.16667506017158663808.23* %4 to [1 x i8]*
  %9 = getelementptr inbounds [1 x i8], [1 x i8]* %8, i64 0, i64 0
  call void @__read_once_size(i8* %7, i8* %9, i32 8)
  br label %10

10:                                               ; preds = %5
  ret void
}
