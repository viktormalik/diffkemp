define i32 @dev_close(%struct.net_device*) #0 {
  %2 = alloca %struct.list_head, align 8
  br label %3

3:                                                ; preds = %1
  %4 = getelementptr inbounds %struct.list_head, %struct.list_head* %2, i32 0, i32 0
  store %struct.list_head* %2, %struct.list_head** %4, align 8
  %5 = getelementptr inbounds %struct.list_head, %struct.list_head* %2, i32 0, i32 1
  store %struct.list_head* %2, %struct.list_head** %5, align 8
  br label %6

6:                                                ; preds = %3
  %7 = getelementptr inbounds %struct.net_device, %struct.net_device* %0, i32 0, i32 120
  call void @list_add(%struct.list_head* %7, %struct.list_head* %2)
  br label %UnifiedReturnBlock

UnifiedReturnBlock:                               ; preds = %6
  ret i32 undef
}
