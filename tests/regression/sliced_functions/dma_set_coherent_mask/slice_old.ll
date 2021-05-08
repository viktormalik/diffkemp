define void @dma_set_coherent_mask(%struct.device*, i64) #0 {
  br label %3

3:                                                ; preds = %2
  call void @dma_check_mask(%struct.device* %0, i64 %1)
  br label %4

4:                                                ; preds = %3
  ret void
}
