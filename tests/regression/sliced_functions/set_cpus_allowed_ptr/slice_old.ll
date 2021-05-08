define i32 @set_cpus_allowed_ptr(%struct.task_struct*, %struct.cpumask*) #2 {
  br label %3

3:                                                ; preds = %2
  %4 = getelementptr inbounds %struct.task_struct, %struct.task_struct* %0, i32 0, i32 11
  %5 = load i32, i32* %4, align 4
  %6 = icmp ne i32 %5, 0
  br i1 %6, label %7, label %7

7:                                                ; preds = %3, %3
  ret i32 undef
}
