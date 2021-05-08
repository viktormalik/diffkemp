define void @up_read(%struct.rw_semaphore*) #0 {
  call void @rwsem_clear_reader_owned(%struct.rw_semaphore* %0)
  ret void
}
