; Found differences in functions called by vfree
;
; __vunmap differs:
;   Callstack (snapshots/linux-4.18.0-80.el8):
;   __vunmap at mm/vmalloc.c:1593
;
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   __vunmap at mm/vmalloc.c:1658
;
;   Diff:
;   *************** static void __vunmap(const void *addr, int deallocate_pages)
;   *** 1517,1519 ***
;
;   ! 	remove_vm_area(addr);
;     	if (deallocate_pages) {
;   --- 1581,1584 ---
;
;   ! 	vm_remove_mappings(area, deallocate_pages);
;   ! 
;     	if (deallocate_pages) {

; Metadata
!0 = !{ !"pattern-start" }
!1 = !{ !"pattern-end" }

; Structures
%struct.vmap_area = type { i1, i1, i1, i1, i1, i1, %struct.vm_struct* }
%struct.vm_struct = type {}

; Functions
declare %struct.vmap_area* @find_vmap_area(...)
declare %struct.vm_struct* @remove_vm_area(...)
declare void @vm_remove_mappings(...)

define void @diffkemp.old.__vunmap(i8*, i32) {
  %3 = ptrtoint i8* %0 to i64
  %4 = call %struct.vmap_area* (...) @find_vmap_area(i64 %3)
  %5 = getelementptr inbounds %struct.vmap_area, %struct.vmap_area* %4, i32 0, i32 6
  %6 = load %struct.vm_struct*, %struct.vm_struct** %5, align 8
  br label %aBB

aBB:
  %7 = call %struct.vm_struct* (...) @remove_vm_area(i8* %0), !diffkemp.pattern !0
  br label %bBB, !diffkemp.pattern !1

bBB:
  ret void
}

define void @diffkemp.new.__vunmap(i8*, i32) {
  %3 = ptrtoint i8* %0 to i64
  %4 = call %struct.vmap_area* (...) @find_vmap_area(i64 %3)
  %5 = getelementptr inbounds %struct.vmap_area, %struct.vmap_area* %4, i32 0, i32 6
  %6 = load %struct.vm_struct*, %struct.vm_struct** %5, align 8
  br label %aBB

aBB:
  call void (...) @vm_remove_mappings(%struct.vm_struct* %6, i32 %1), !diffkemp.pattern !0
  br label %bBB, !diffkemp.pattern !1

bBB:
  ret void
}
