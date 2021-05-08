define i32 @call_usermodehelper_exec_async(i8*) #0 {
  %2 = bitcast i8* %0 to %struct.subprocess_info*
  br label %3

3:                                                ; preds = %1
  %4 = getelementptr inbounds %struct.subprocess_info, %struct.subprocess_info* %2, i32 0, i32 5
  %5 = load %struct.file*, %struct.file** %4, align 8
  br label %6

6:                                                ; preds = %3
  %7 = getelementptr inbounds %struct.subprocess_info, %struct.subprocess_info* %2, i32 0, i32 3
  %8 = load i8**, i8*** %7, align 8
  %9 = bitcast i8** %8 to i8*
  %10 = getelementptr inbounds %struct.subprocess_info, %struct.subprocess_info* %2, i32 0, i32 4
  %11 = load i8**, i8*** %10, align 8
  %12 = bitcast i8** %11 to i8*
  %13 = call i32 @do_execve_file(%struct.file* %5, i8* %9, i8* %12)
  %14 = icmp ne i32 %13, 0
  br i1 %14, label %20, label %15

15:                                               ; preds = %6
  %16 = call %struct.task_struct* @get_current()
  %17 = getelementptr inbounds %struct.task_struct, %struct.task_struct* %16, i32 0, i32 4
  %18 = load i32, i32* %17, align 4
  %19 = or i32 %18, 33554432
  store i32 %19, i32* %17, align 4
  br label %20

20:                                               ; preds = %15, %6
  unreachable
}
