define void @memcg_unlink_cache(%struct.kmem_cache*) #2 {
  br label %2

2:                                                ; preds = %1
  br label %3

3:                                                ; preds = %2
  ret void
}
