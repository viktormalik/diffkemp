define void @hci_unregister_dev(%struct.hci_dev*) #0 {
  br label %2

2:                                                ; preds = %1
  %3 = getelementptr inbounds %struct.hci_dev, %struct.hci_dev* %0, i32 0, i32 75
  %4 = call zeroext i1 @cancel_work_sync(%struct.work_struct* %3)
  %5 = call i32 @hci_dev_do_close(%struct.hci_dev* %0)
  br label %6

6:                                                ; preds = %2
  %7 = getelementptr inbounds %struct.hci_dev, %struct.hci_dev* %0, i32 0, i32 188
  %8 = load i8*, i8** %7, align 8
  call void @kfree_const(i8* %8)
  %9 = getelementptr inbounds %struct.hci_dev, %struct.hci_dev* %0, i32 0, i32 189
  %10 = load i8*, i8** %9, align 8
  call void @kfree_const(i8* %10)
  ret void
}
