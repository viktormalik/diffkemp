define %struct.request_queue* @blk_alloc_queue_node(i32, i32) #2 {
  %3 = load %struct.kmem_cache*, %struct.kmem_cache** @blk_requestq_cachep, align 8
  %4 = or i32 %0, 32768
  %5 = call noalias i8* @kmem_cache_alloc_node(%struct.kmem_cache* %3, i32 %4, i32 %1)
  %6 = bitcast i8* %5 to %struct.request_queue*
  br label %7

7:                                                ; preds = %2
  %8 = getelementptr inbounds %struct.request_queue, %struct.request_queue* %6, i32 0, i32 0
  call void @INIT_LIST_HEAD(%struct.list_head* %8)
  br label %9

9:                                                ; preds = %7
  ret %struct.request_queue* undef
}
