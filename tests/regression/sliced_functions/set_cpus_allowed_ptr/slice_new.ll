define i32 @set_cpus_allowed_ptr(%struct.task_struct*, %struct.cpumask*) #2 {
  br label %3

3:                                                ; preds = %2
  %4 = call i32 @task_on_rq_queued(%struct.task_struct* %0)
  %5 = icmp ne i32 %4, 0
  br i1 %5, label %6, label %6

6:                                                ; preds = %3, %3
  ret i32 undef
}
