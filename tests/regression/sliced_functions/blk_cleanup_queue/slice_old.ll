define void @blk_cleanup_queue(%struct.request_queue*) #2 {
  %2 = getelementptr inbounds %struct.request_queue, %struct.request_queue* %0, i32 0, i32 54
  call void @mutex_lock(%struct.mutex* %2)
  call void @blk_set_queue_dying(%struct.request_queue* %0)
  call void @blk_queue_flag_set(i32 5, %struct.request_queue* %0)
  call void @blk_queue_flag_set(i32 12, %struct.request_queue* %0)
  call void @blk_queue_flag_set(i32 2, %struct.request_queue* %0)
  call void @mutex_unlock(%struct.mutex* %2)
  br label %3

3:                                                ; preds = %1
  call void @mutex_lock(%struct.mutex* %2)
  br label %4

4:                                                ; preds = %3
  ret void
}
