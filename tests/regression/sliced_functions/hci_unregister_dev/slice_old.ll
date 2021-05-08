define void @hci_unregister_dev(%struct.hci_dev*) #0 {
  br label %2

2:                                                ; preds = %1
  %3 = call i32 @hci_dev_do_close(%struct.hci_dev* %0)
  %4 = getelementptr inbounds %struct.hci_dev, %struct.hci_dev* %0, i32 0, i32 75
  %5 = call zeroext i1 @cancel_work_sync(%struct.work_struct* %4)
  br label %6

6:                                                ; preds = %6, %2
  br label %6
}
