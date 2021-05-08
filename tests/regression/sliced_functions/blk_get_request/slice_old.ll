define %struct.request* @blk_get_request(%struct.request_queue*, i32, i32) #2 {
  br label %4

4:                                                ; preds = %3
  %5 = call %struct.request* @blk_mq_alloc_request(%struct.request_queue* %0, i32 %1, i32 %2, i1 zeroext false)
  br label %6

6:                                                ; preds = %4
  ret %struct.request* undef
}
