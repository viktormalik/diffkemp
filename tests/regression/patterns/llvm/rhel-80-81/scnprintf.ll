; Found differences in functions called by scnprintf
;
; NR_PAGEFLAGS differs:
;   Callstack (snapshots/linux-4.18.0-80.el8):
;   vscnprintf at lib/vsprintf.c:2441
;   vsnprintf at lib/vsprintf.c:2387
;   pointer at lib/vsprintf.c:2288
;   flags_string at lib/vsprintf.c:1956
;   NR_PAGEFLAGS (macro) at lib/vsprintf.c:1519
;
;   Callstack (snapshots/linux-4.18.0-147.el8):
;   vscnprintf at lib/vsprintf.c:2441
;   vsnprintf at lib/vsprintf.c:2387
;   pointer at lib/vsprintf.c:2288
;   flags_string at lib/vsprintf.c:1956
;   NR_PAGEFLAGS (macro) at lib/vsprintf.c:1519
;
;   Diff:
;     25
;
;     26

define i64 @diffkemp.old.NR_PAGEFLAGS() {
  ret i64 33554431
}

define i64 @diffkemp.new.NR_PAGEFLAGS() {
  ret i64 67108863
}
