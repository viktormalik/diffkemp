define %struct.block_device* @lookup_bdev(i8*) #2 {
  %2 = alloca %struct.path, align 8
  br label %3

3:                                                ; preds = %1
  %4 = call i32 @kern_path(i8* %0, i32 1, %struct.path* %2)
  br label %5

5:                                                ; preds = %3
  %6 = call zeroext i1 @may_open_dev(%struct.path* %2)
  br i1 %6, label %7, label %7

7:                                                ; preds = %5, %5
  call void @path_put(%struct.path* %2)
  br label %8

8:                                                ; preds = %7
  ret %struct.block_device* undef
}
