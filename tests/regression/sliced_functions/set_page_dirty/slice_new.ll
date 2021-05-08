define i32 @set_page_dirty(%struct.page*) #0 {
  br label %2

2:                                                ; preds = %1
  %3 = call i32 @PageReclaim(%struct.page* %0)
  %4 = icmp ne i32 %3, 0
  br i1 %4, label %5, label %6

5:                                                ; preds = %2
  call void @ClearPageReclaim(%struct.page* %0)
  br label %6

6:                                                ; preds = %5, %2
  ret i32 undef
}
