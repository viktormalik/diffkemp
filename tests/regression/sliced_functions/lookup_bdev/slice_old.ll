define %struct.block_device* @lookup_bdev(i8*) #2 {
  %2 = alloca %struct.path, align 8
  br label %3

3:                                                ; preds = %1
  %4 = call i32 @kern_path(i8* %0, i32 1, %struct.path* %2)
  br label %5

5:                                                ; preds = %3
  %6 = getelementptr inbounds %struct.path, %struct.path* %2, i32 0, i32 0
  %7 = load %struct.vfsmount*, %struct.vfsmount** %6, align 8
  %8 = getelementptr inbounds %struct.vfsmount, %struct.vfsmount* %7, i32 0, i32 2
  %9 = load i32, i32* %8, align 8
  %10 = and i32 %9, 2
  %11 = icmp ne i32 %10, 0
  br i1 %11, label %12, label %12

12:                                               ; preds = %5, %5
  call void @path_put(%struct.path* %2)
  br label %13

13:                                               ; preds = %12
  ret %struct.block_device* undef
}
