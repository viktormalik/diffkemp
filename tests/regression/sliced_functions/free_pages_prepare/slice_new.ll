define zeroext i1 @free_pages_prepare(%struct.page*, i32, i1 zeroext) #6 {
  br label %4

4:                                                ; preds = %12, %3
  %.01 = phi i32 [ %13, %12 ], [ 1, %3 ]
  br label %5

5:                                                ; preds = %4
  %6 = sext i32 %.01 to i64
  br label %._crit_edge

._crit_edge:                                      ; preds = %5
  %7 = getelementptr inbounds %struct.page, %struct.page* %0, i64 %6
  br label %8

8:                                                ; preds = %._crit_edge
  %9 = getelementptr inbounds %struct.page, %struct.page* %7, i32 0, i32 0
  %10 = load i64, i64* %9, align 16
  %11 = and i64 %10, -62914560
  store i64 %11, i64* %9, align 16
  br label %12

12:                                               ; preds = %8
  %13 = add nsw i32 %.01, 1
  br label %4
}
