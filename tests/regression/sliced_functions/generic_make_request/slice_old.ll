define void @generic_make_request(%struct.bio*) #2 {
  %2 = alloca [2 x %struct.bio_list], align 16
  br label %3

3:                                                ; preds = %1
  %4 = getelementptr inbounds [2 x %struct.bio_list], [2 x %struct.bio_list]* %2, i64 0, i64 0
  br label %5

5:                                                ; preds = %14, %3
  %.0 = phi %struct.bio* [ %0, %3 ], [ %15, %14 ]
  %6 = getelementptr inbounds %struct.bio, %struct.bio* %.0, i32 0, i32 2
  %7 = load %struct.block_device*, %struct.block_device** %6, align 8
  %8 = call %struct.request_queue* @bdev_get_queue(%struct.block_device* %7)
  %9 = call i32 @blk_queue_enter(%struct.request_queue* %8, i1 zeroext false)
  %10 = icmp eq i32 %9, 0
  %11 = zext i1 %10 to i32
  %12 = sext i32 %11 to i64
  %13 = icmp ne i64 %12, 0
  br i1 %13, label %14, label %14

14:                                               ; preds = %5, %5
  %15 = call %struct.bio* @bio_list_pop(%struct.bio_list* %4)
  %16 = icmp ne %struct.bio* %15, null
  br i1 %16, label %5, label %17

17:                                               ; preds = %14
  ret void
}
