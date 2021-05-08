define void @check_new_page_bad(%struct.page*) #0 {
  %2 = getelementptr inbounds %struct.page, %struct.page* %0, i32 0, i32 2
  %3 = bitcast %union.anon.7504143666799817323.12* %2 to %struct.atomic_t*
  %4 = call i32 @atomic_read(%struct.atomic_t* %3)
  %5 = icmp ne i32 %4, -1
  %6 = zext i1 %5 to i32
  %7 = sext i32 %6 to i64
  %8 = icmp ne i64 %7, 0
  %spec.select = select i1 %8, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.46, i64 0, i64 0), i8* null
  %9 = getelementptr inbounds %struct.page, %struct.page* %0, i32 0, i32 1
  %10 = bitcast %union.anon.9690635198121769342* %9 to %struct.anon.7026248042790727088*
  %11 = getelementptr inbounds %struct.anon.7026248042790727088, %struct.anon.7026248042790727088* %10, i32 0, i32 1
  %12 = load %struct.address_space*, %struct.address_space** %11, align 8
  %13 = icmp ne %struct.address_space* %12, null
  %14 = zext i1 %13 to i32
  %15 = sext i32 %14 to i64
  %16 = icmp ne i64 %15, 0
  %.1 = select i1 %16, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.47, i64 0, i64 0), i8* %spec.select
  %17 = call i32 @page_ref_count(%struct.page* %0)
  %18 = icmp ne i32 %17, 0
  %19 = zext i1 %18 to i32
  %20 = sext i32 %19 to i64
  %21 = icmp ne i64 %20, 0
  %spec.select3 = select i1 %21, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.57, i64 0, i64 0), i8* %.1
  br label %22

22:                                               ; preds = %1
  ret void
}
