define void @down_write(%struct.rw_semaphore*) #0 {
  call void @rwsem_set_owner(%struct.rw_semaphore* %0)
  ret void
}
