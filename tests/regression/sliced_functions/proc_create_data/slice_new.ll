define void @proc_create_data(i8*, i16 zeroext, %struct.proc_dir_entry*, %struct.file_operations*, i8*) #0 {
  %6 = alloca %struct.proc_dir_entry*, align 8
  store %struct.proc_dir_entry* %2, %struct.proc_dir_entry** %6, align 8
  %7 = zext i16 %1 to i32
  %8 = and i32 %7, 61440
  %9 = icmp eq i32 %8, 0
  %10 = or i32 %7, 32768
  %11 = trunc i32 %10 to i16
  %.01 = select i1 %9, i16 %11, i16 %1
  %12 = zext i16 %.01 to i32
  br label %13

13:                                               ; preds = %5
  %14 = icmp eq %struct.file_operations* %3, null
  %15 = zext i1 %14 to i32
  %16 = sext i32 %15 to i64
  %17 = icmp ne i64 %16, 0
  br i1 %17, label %18, label %20

18:                                               ; preds = %13
  call void @simpll__inlineasm.0(i8* null, i32 0, i64 12)
  br label %19

19:                                               ; preds = %19, %18
  br label %19

20:                                               ; preds = %13
  %21 = and i32 %12, 4095
  %22 = icmp eq i32 %21, 0
  %23 = or i32 %12, 292
  %24 = trunc i32 %23 to i16
  %.1 = select i1 %22, i16 %24, i16 %.01
  %25 = call %struct.proc_dir_entry* @__proc_create(%struct.proc_dir_entry** %6, i8* %0, i16 zeroext %.1, i32 1)
  br label %26

26:                                               ; preds = %20
  %27 = getelementptr inbounds %struct.proc_dir_entry, %struct.proc_dir_entry* %25, i32 0, i32 6
  store %struct.inode_operations* @proc_file_inode_operations, %struct.inode_operations** %27, align 8
  br label %28

28:                                               ; preds = %26
  ret void
}
