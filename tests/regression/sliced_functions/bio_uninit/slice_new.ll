define void @bio_uninit(%struct.bio*) #0 {
  %2 = call %struct.bio_integrity_payload* @bio_integrity(%struct.bio* %0)
  %3 = icmp ne %struct.bio_integrity_payload* %2, null
  br i1 %3, label %4, label %5

4:                                                ; preds = %1
  call void @bio_integrity_free(%struct.bio* %0)
  br label %5

5:                                                ; preds = %4, %1
  ret void
}
