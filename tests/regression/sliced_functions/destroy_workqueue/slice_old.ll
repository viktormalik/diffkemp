define void @destroy_workqueue(%struct.workqueue_struct*) #2 {
  br label %LeafBlock

LeafBlock:                                        ; preds = %1
  %2 = getelementptr inbounds %struct.workqueue_struct, %struct.workqueue_struct* %0, i32 0, i32 17
  call void @call_rcu_sched(%struct.callback_head* %2, void (%struct.callback_head*)* @rcu_free_wq)
  br label %NewDefault

NewDefault:                                       ; preds = %LeafBlock
  ret void
}
