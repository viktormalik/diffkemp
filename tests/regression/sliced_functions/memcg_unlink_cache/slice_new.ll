define void @memcg_unlink_cache(%struct.kmem_cache*) #2 {
  %2 = alloca %union.anon.12630085663318556666.23, align 8
  %3 = getelementptr inbounds %struct.kmem_cache, %struct.kmem_cache* %0, i32 0, i32 20
  %4 = getelementptr inbounds %struct.memcg_cache_params, %struct.memcg_cache_params* %3, i32 0, i32 1
  br label %5

5:                                                ; preds = %1
  %6 = bitcast %union.anon.10829206188889125693* %4 to %struct.anon.4932632430402441177*
  %7 = getelementptr inbounds %struct.anon.4932632430402441177, %struct.anon.4932632430402441177* %6, i32 0, i32 0
  %8 = load %struct.mem_cgroup*, %struct.mem_cgroup** %7, align 8
  call void @mem_cgroup_put(%struct.mem_cgroup* %8)
  %9 = bitcast %union.anon.12630085663318556666.23* %2 to i8*
  call void @llvm.memset.p0i8.i64(i8* align 8 %9, i8 0, i64 8, i1 false)
  %10 = bitcast %struct.mem_cgroup** %7 to i8*
  %11 = bitcast %union.anon.12630085663318556666.23* %2 to [1 x i8]*
  %12 = getelementptr inbounds [1 x i8], [1 x i8]* %11, i64 0, i64 0
  call void @__write_once_size(i8* %10, i8* %12, i32 8)
  br label %13

13:                                               ; preds = %5
  ret void
}
