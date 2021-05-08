define %struct.request* @blk_get_request(%struct.request_queue*, i32, i32) #2 {
  br label %4

4:                                                ; preds = %3
  %5 = and i32 %2, 16
  %6 = icmp ne i32 %5, 0
  %7 = select i1 %6, i32 0, i32 1
  %8 = call %struct.request* @blk_mq_alloc_request(%struct.request_queue* %0, i32 %1, i32 %7)
  br label %9

9:                                                ; preds = %4
  ret %struct.request* undef
}
