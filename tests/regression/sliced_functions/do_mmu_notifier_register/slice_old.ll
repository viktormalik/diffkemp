define i32 @do_mmu_notifier_register(%struct.mmu_notifier*, %struct.mm_struct*, i32) #0 {
  %4 = getelementptr inbounds %struct.mm_struct, %struct.mm_struct* %1, i32 0, i32 0
  br label %5

5:                                                ; preds = %3
  %6 = getelementptr inbounds %struct.anon.14783511004515759537, %struct.anon.14783511004515759537* %4, i32 0, i32 51
  %7 = getelementptr inbounds %struct.mmu_notifier, %struct.mmu_notifier* %0, i32 0, i32 0
  %8 = load %struct.mmu_notifier_mm*, %struct.mmu_notifier_mm** %6, align 8
  %9 = getelementptr inbounds %struct.mmu_notifier_mm, %struct.mmu_notifier_mm* %8, i32 0, i32 0
  call void @hlist_add_head(%struct.hlist_node* %7, %struct.hlist_head* %9)
  br label %10

10:                                               ; preds = %10, %5
  br label %10
}
