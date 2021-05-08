define i32 @set_page_dirty(%struct.page*) #0 {
  br label %2

2:                                                ; preds = %1
  call void @ClearPageReclaim(%struct.page* %0)
  br label %3

3:                                                ; preds = %2
  ret i32 undef
}
