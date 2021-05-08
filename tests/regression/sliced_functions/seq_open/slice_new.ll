define i32 @seq_open(%struct.file*, %struct.seq_operations*) #0 {
  %3 = getelementptr inbounds %struct.file, %struct.file* %0, i32 0, i32 15
  %4 = load i8*, i8** %3, align 8
  %5 = bitcast i8* %4 to %struct.seq_file*
  %6 = icmp ne %struct.seq_file* %5, null
  br i1 %6, label %10, label %7

7:                                                ; preds = %2
  %8 = call i8* @kmalloc(i64 136, i32 208)
  %9 = bitcast i8* %8 to %struct.seq_file*
  br label %10

10:                                               ; preds = %7, %2
  %.01 = phi %struct.seq_file* [ %5, %2 ], [ %9, %7 ]
  %11 = getelementptr inbounds %struct.seq_file, %struct.seq_file* %.01, i32 0, i32 12
  store %struct.file* %0, %struct.file** %11, align 8
  br label %12

12:                                               ; preds = %10
  ret i32 undef
}
