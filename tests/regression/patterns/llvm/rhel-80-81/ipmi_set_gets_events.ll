; Found differences in functions called by ipmi_set_gets_events
;
; free_user differs:
;   Callstack (snapshots/linux-4.18.0-80.el8):
;   deliver_local_response at drivers/char/ipmi/ipmi_msghandler.c:1498
;   deliver_response at drivers/char/ipmi/ipmi_msghandler.c:902
;   ipmi_free_recv_msg at drivers/char/ipmi/ipmi_msghandler.c:876
;   free_user at drivers/char/ipmi/ipmi_msghandler.c:4752
;
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   deliver_local_response at drivers/char/ipmi/ipmi_msghandler.c:1580
;   deliver_response at drivers/char/ipmi/ipmi_msghandler.c:930
;   ipmi_free_recv_msg at drivers/char/ipmi/ipmi_msghandler.c:904
;   free_user at drivers/char/ipmi/ipmi_msghandler.c:4813
;
;   Diff:
;   *************** static void free_user(struct kref *ref)
;   *** 1186,1188 ***
;     	struct ipmi_user *user = container_of(ref, struct ipmi_user, refcount);
;   ! 	cleanup_srcu_struct(&user->release_barrier);
;     	kfree(user);
;   --- 1263,1270 ---
;     	struct ipmi_user *user = container_of(ref, struct ipmi_user, refcount);
;   ! 
;   ! 	/*
;   ! 	 * Cleanup without waiting. This could be called in atomic context.
;   ! 	 * Refcount is zero: all read-sections should have been ended.
;   ! 	 */
;   ! 	cleanup_srcu_struct_quiesced(&user->release_barrier);
;     	kfree(user);

; Metadata
!0 = !{ !"pattern-start" }
!1 = !{ !"pattern-end" }

; Structures
%struct.ipmi_user = type { i1, i1, %struct.srcu_struct }
%struct.srcu_struct = type {}
%struct.kref = type {}

; Functions
declare void @diffkemp.old.cleanup_srcu_struct(%struct.srcu_struct*)
declare void @diffkemp.new.cleanup_srcu_struct_quiesced(%struct.srcu_struct*)

define void @diffkemp.old.free_user(%struct.kref*) {
  %2 = bitcast %struct.kref* %0 to i8*
  %3 = getelementptr i8, i8* %2, i64 -50456
  %4 = bitcast i8* %3 to %struct.ipmi_user*
  %5 = getelementptr inbounds %struct.ipmi_user, %struct.ipmi_user* %4, i32 0, i32 2
  call void @diffkemp.old.cleanup_srcu_struct(%struct.srcu_struct* %5), !diffkemp.pattern !0
  ret void, !diffkemp.pattern !1
}

define void @diffkemp.new.free_user(%struct.kref*) {
  %2 = bitcast %struct.kref* %0 to i8*
  %3 = getelementptr i8, i8* %2, i64 -50456
  %4 = bitcast i8* %3 to %struct.ipmi_user*
  %5 = getelementptr inbounds %struct.ipmi_user, %struct.ipmi_user* %4, i32 0, i32 2
  call void @diffkemp.new.cleanup_srcu_struct_quiesced(%struct.srcu_struct* %5), !diffkemp.pattern !0
  ret void, !diffkemp.pattern !1
}
